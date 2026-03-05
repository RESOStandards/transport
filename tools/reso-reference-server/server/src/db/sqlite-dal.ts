/**
 * SQLite implementation of the Data Access Layer.
 *
 * Uses LEFT JOINs for $expand resolution with app-side grouping to nest
 * expanded navigation properties into their parent entities, mirroring
 * the PostgreSQL implementation pattern.
 *
 * better-sqlite3 is synchronous — methods return Promise-wrapped results
 * to conform to the async DataAccessLayer interface.
 */

import type Database from 'better-sqlite3';
import type { ResoField } from '../metadata/types.js';
import type {
  CollectionQueryOptions,
  CollectionResult,
  DataAccessLayer,
  EntityRecord,
  NavigationPropertyBinding,
  ResourceContext,
  SingleResult
} from './data-access.js';
import { filterToSqlite } from './filter-to-sqlite.js';
import { deserializeRow } from './queries.js';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Serialize a value for SQLite insertion based on field definition. */
const serializeValue = (value: unknown, field: ResoField): unknown => {
  if (field.isCollection && Array.isArray(value)) {
    return JSON.stringify(value);
  }
  // SQLite stores booleans as 0/1
  if (field.type === 'Edm.Boolean' && typeof value === 'boolean') {
    return value ? 1 : 0;
  }
  return value;
};

/**
 * SQLite-aware deserialization wrapper.
 * Handles boolean 0/1 → true/false before delegating to the shared deserializer.
 */
const deserializeSqliteRow = (row: Record<string, unknown>, fields: ReadonlyArray<ResoField>): Record<string, unknown> => {
  const fieldMap = new Map(fields.map(f => [f.fieldName, f]));
  const coerced: Record<string, unknown> = {};
  for (const [key, value] of Object.entries(row)) {
    const field = fieldMap.get(key);
    if (field?.type === 'Edm.Boolean' && typeof value === 'number') {
      coerced[key] = value !== 0;
    } else {
      coerced[key] = value;
    }
  }
  return deserializeRow(coerced, fields);
};

/** Parse a $select string into an array of field names. */
const parseSelect = (select: string): ReadonlyArray<string> =>
  select
    .split(',')
    .map(s => s.trim())
    .filter(s => s.length > 0);

/** Parse a $orderby string into validated ORDER BY clauses. */
const parseOrderBy = (orderby: string, fields: ReadonlyArray<ResoField>, tableAlias: string): string => {
  const fieldNames = new Set(fields.map(f => f.fieldName));
  return orderby
    .split(',')
    .map(part => {
      const trimmed = part.trim();
      const [field, ...rest] = trimmed.split(/\s+/);
      if (!field || !fieldNames.has(field)) return undefined;
      const dir = rest[0]?.toLowerCase();
      const direction = dir === 'desc' ? 'DESC' : 'ASC';
      return `${tableAlias}."${field}" ${direction}`;
    })
    .filter((clause): clause is string => clause !== undefined)
    .join(', ');
};

/** Parse a $expand string into navigation property names. */
const parseExpand = (expand: string): ReadonlyArray<string> =>
  expand
    .split(',')
    .map(s => {
      const parenIdx = s.indexOf('(');
      return (parenIdx >= 0 ? s.slice(0, parenIdx) : s).trim();
    })
    .filter(s => s.length > 0);

/**
 * Build the SELECT column list for the parent resource, optionally
 * restricted by $select. Always includes the key field.
 */
const buildSelectColumns = (
  fields: ReadonlyArray<ResoField>,
  keyField: string,
  select: string | undefined,
  alias: string
): ReadonlyArray<string> => {
  let selectedNames: ReadonlyArray<string>;
  if (select) {
    const requested = new Set(parseSelect(select));
    requested.add(keyField);
    selectedNames = fields.filter(f => requested.has(f.fieldName)).map(f => f.fieldName);
  } else {
    selectedNames = fields.map(f => f.fieldName);
  }
  return selectedNames.map(name => `${alias}."${name}" AS "${alias}.${name}"`);
};

/**
 * Build SELECT column list for an expanded navigation property.
 * Excludes expansion fields — they are lazy, loaded via nested $expand.
 */
const buildNavSelectColumns = (binding: NavigationPropertyBinding, alias: string): ReadonlyArray<string> =>
  binding.targetFields.filter(f => !f.isExpansion).map(f => `${alias}."${f.fieldName}" AS "${alias}.${f.fieldName}"`);

/**
 * Build the LEFT JOIN clause for a navigation property.
 */
const buildExpandJoin = (
  parentResource: string,
  parentKeyField: string,
  parentAlias: string,
  binding: NavigationPropertyBinding,
  navAlias: string
): string => {
  const fk = binding.foreignKey;
  if (fk.strategy === 'resource-record-key') {
    return (
      `LEFT JOIN "${binding.targetResource}" ${navAlias} ` +
      `ON ${navAlias}."ResourceName" = '${parentResource}' ` +
      `AND ${navAlias}."ResourceRecordKey" = ${parentAlias}."${parentKeyField}"`
    );
  }
  if (fk.strategy === 'parent-fk') {
    const rawCol = fk.parentColumn!;
    const dotIdx = parentKeyField.indexOf('.');
    const parentCol = dotIdx >= 0 ? `${parentKeyField.substring(0, dotIdx)}.${rawCol}` : rawCol;
    return (
      `LEFT JOIN "${binding.targetResource}" ${navAlias} ` + `ON ${navAlias}."${binding.targetKeyField}" = ${parentAlias}."${parentCol}"`
    );
  }
  // direct FK — target has parent's key field
  const targetCol = fk.targetColumn ?? parentKeyField;
  return `LEFT JOIN "${binding.targetResource}" ${navAlias} ` + `ON ${navAlias}."${targetCol}" = ${parentAlias}."${parentKeyField}"`;
};

// ---------------------------------------------------------------------------
// Row grouping — collapse LEFT JOIN results into nested entities
// ---------------------------------------------------------------------------

/** Extract parent-level columns from a flat row. */
const extractParentColumns = (row: Record<string, unknown>, alias: string, fields: ReadonlyArray<ResoField>): Record<string, unknown> => {
  const prefix = `${alias}.`;
  const result: Record<string, unknown> = {};
  for (const field of fields) {
    const colKey = `${prefix}${field.fieldName}`;
    if (colKey in row) {
      result[field.fieldName] = row[colKey];
    }
  }
  return result;
};

/** Extract navigation property columns from a flat row. */
const extractNavColumns = (
  row: Record<string, unknown>,
  navAlias: string,
  binding: NavigationPropertyBinding
): Record<string, unknown> | undefined => {
  const prefix = `${navAlias}.`;
  const result: Record<string, unknown> = {};
  let hasNonNull = false;
  for (const field of binding.targetFields) {
    const colKey = `${prefix}${field.fieldName}`;
    if (colKey in row) {
      result[field.fieldName] = row[colKey];
      if (row[colKey] !== null && row[colKey] !== undefined) {
        hasNonNull = true;
      }
    }
  }
  return hasNonNull ? result : undefined;
};

/**
 * Group flat JOIN result rows into parent entities with nested navprops.
 */
const groupRows = (
  rows: ReadonlyArray<Record<string, unknown>>,
  parentAlias: string,
  keyField: string,
  parentFields: ReadonlyArray<ResoField>,
  selectedFields: ReadonlyArray<ResoField>,
  expandBindings: ReadonlyArray<{
    readonly binding: NavigationPropertyBinding;
    readonly alias: string;
  }>
): ReadonlyArray<EntityRecord> => {
  const grouped = new Map<
    string,
    {
      parent: Record<string, unknown>;
      navs: Map<string, Record<string, unknown>[]>;
    }
  >();

  for (const row of rows) {
    const parentData = extractParentColumns(row, parentAlias, selectedFields);
    const parentKey = String(parentData[keyField] ?? '');

    if (!grouped.has(parentKey)) {
      const deserializedParent = deserializeSqliteRow(parentData, parentFields);
      grouped.set(parentKey, {
        parent: deserializedParent,
        navs: new Map(expandBindings.map(({ binding }) => [binding.name, []]))
      });
    }

    const entry = grouped.get(parentKey)!;
    for (const { binding, alias } of expandBindings) {
      const navData = extractNavColumns(row, alias, binding);
      if (navData) {
        const deserialized = deserializeSqliteRow(navData, [...binding.targetFields]);
        entry.navs.get(binding.name)?.push(deserialized);
      }
    }
  }

  const result: EntityRecord[] = [];
  for (const { parent, navs } of grouped.values()) {
    const entity: Record<string, unknown> = { ...parent };
    for (const { binding } of expandBindings) {
      const navRows = navs.get(binding.name) ?? [];
      if (binding.isCollection) {
        const seen = new Set<string>();
        const unique = navRows.filter(r => {
          const k = String(r[binding.targetKeyField] ?? '');
          if (seen.has(k)) return false;
          seen.add(k);
          return true;
        });
        entity[binding.name] = unique;
      } else {
        entity[binding.name] = navRows[0] ?? null;
      }
    }
    result.push(entity);
  }

  return result;
};

// ---------------------------------------------------------------------------
// SQLite DAL implementation
// ---------------------------------------------------------------------------

/** Creates a SQLite Data Access Layer implementation. */
export const createSqliteDal = (db: Database.Database): DataAccessLayer => {
  const queryCollection = async (ctx: ResourceContext, options?: CollectionQueryOptions): Promise<CollectionResult> => {
    const parentAlias = 'p';
    const values: unknown[] = [];

    // Determine which fields to select (exclude expansion fields)
    const dataFields = ctx.fields.filter(f => !f.isExpansion);
    let selectedFields: ReadonlyArray<ResoField>;
    if (options?.$select) {
      const requested = new Set(parseSelect(options.$select));
      requested.add(ctx.keyField);
      selectedFields = dataFields.filter(f => requested.has(f.fieldName));
    } else {
      selectedFields = dataFields;
    }

    // Resolve $expand bindings
    const expandBindings: Array<{
      readonly binding: NavigationPropertyBinding;
      readonly alias: string;
    }> = [];

    if (options?.$expand) {
      const expandNames = parseExpand(options.$expand);
      const bindingMap = new Map(ctx.navigationBindings.map(b => [b.name, b]));

      for (const name of expandNames) {
        const binding = bindingMap.get(name);
        if (!binding) continue;
        expandBindings.push({ binding, alias: `nav_${name}` });
      }
    }

    // Build SELECT columns — include FK columns needed for $expand JOINs
    let expandSelect = options?.$select;
    if (options?.$select && expandBindings.length > 0) {
      const fkCols = expandBindings.map(({ binding }) => binding.foreignKey.parentColumn).filter((col): col is string => col !== undefined);
      if (fkCols.length > 0) {
        expandSelect = `${options.$select},${fkCols.join(',')}`;
      }
    }
    const parentSelectCols: ReadonlyArray<string> = buildSelectColumns(dataFields, ctx.keyField, expandSelect, parentAlias);

    // Build WHERE clause from $filter
    let whereClause = '';
    const filterValues: unknown[] = [];
    if (options?.$filter) {
      const filterResult = filterToSqlite(options.$filter, ctx.fields, parentAlias);
      whereClause = `WHERE ${filterResult.where}`;
      filterValues.push(...filterResult.values);
      values.push(...filterResult.values);
    }

    // Build ORDER BY
    let orderByClause = '';
    if (options?.$orderby) {
      const parsed = parseOrderBy(options.$orderby, ctx.fields, parentAlias);
      if (parsed) {
        orderByClause = `ORDER BY ${parsed}`;
      }
    }

    // Build LIMIT/OFFSET (SQLite uses ? placeholders)
    let limitClause = '';
    if (options?.$top !== undefined) {
      limitClause = 'LIMIT ?';
      values.push(options.$top);
    }

    let offsetClause = '';
    if (options?.$skip !== undefined) {
      offsetClause = 'OFFSET ?';
      values.push(options.$skip);
    }

    // $count — window function for total matching parent count
    const countCol = options?.$count ? ', COUNT(*) OVER() AS "__total_count"' : '';

    // ===== Branch: $expand present — use CTE to paginate parents first =====
    if (expandBindings.length > 0) {
      const cteSql = [
        `SELECT ${parentSelectCols.join(', ')}${countCol}`,
        `FROM "${ctx.resource}" ${parentAlias}`,
        whereClause,
        orderByClause,
        limitClause,
        offsetClause
      ]
        .filter(s => s.length > 0)
        .join(' ');

      const cteAlias = 'parent_page';

      const outerParentCols = parentSelectCols.map(col => {
        const match = col.match(/AS "(.+)"$/);
        return match ? `${cteAlias}."${match[1]}"` : col;
      });
      const outerCountCol = options?.$count ? `, ${cteAlias}."__total_count"` : '';

      const outerNavCols: string[] = [];
      const outerJoinClauses: string[] = [];
      for (const { binding, alias: navAlias } of expandBindings) {
        outerNavCols.push(...buildNavSelectColumns(binding, navAlias));
        outerJoinClauses.push(buildExpandJoin(ctx.resource, `${parentAlias}.${ctx.keyField}`, cteAlias, binding, navAlias));
      }

      let outerOrderBy = '';
      if (options?.$orderby) {
        const outerParts = options.$orderby
          .split(',')
          .map(part => {
            const trimmed = part.trim();
            const [field, ...rest] = trimmed.split(/\s+/);
            if (!field || !ctx.fields.some(f => f.fieldName === field)) return undefined;
            const dir = rest[0]?.toLowerCase();
            return `${cteAlias}."${parentAlias}.${field}" ${dir === 'desc' ? 'DESC' : 'ASC'}`;
          })
          .filter((clause): clause is string => clause !== undefined);
        if (outerParts.length > 0) outerOrderBy = `ORDER BY ${outerParts.join(', ')}`;
      }
      if (!outerOrderBy) {
        outerOrderBy = `ORDER BY ${cteAlias}."${parentAlias}.${ctx.keyField}"`;
      }

      const outerSql = [
        `SELECT ${[...outerParentCols, ...outerNavCols].join(', ')}${outerCountCol}`,
        `FROM ${cteAlias}`,
        ...outerJoinClauses,
        outerOrderBy
      ]
        .filter(s => s.length > 0)
        .join(' ');

      const sql = `WITH ${cteAlias} AS (${cteSql}) ${outerSql}`;

      const rows = db.prepare(sql).all(...values) as Record<string, unknown>[];

      let count: number | undefined;
      if (options?.$count && rows.length > 0) {
        count = Number(rows[0].__total_count);
      }

      const entities = groupRows(rows, parentAlias, ctx.keyField, ctx.fields, selectedFields, expandBindings);
      return { value: entities, ...(count !== undefined ? { count } : {}) };
    }

    // ===== Branch: no $expand — simple query =====
    const sql = [
      `SELECT ${parentSelectCols.join(', ')}${countCol}`,
      `FROM "${ctx.resource}" ${parentAlias}`,
      whereClause,
      orderByClause,
      limitClause,
      offsetClause
    ]
      .filter(s => s.length > 0)
      .join(' ');

    const rows = db.prepare(sql).all(...values) as Record<string, unknown>[];

    let count: number | undefined;
    if (options?.$count && rows.length > 0) {
      count = Number(rows[0].__total_count);
    } else if (options?.$count && rows.length === 0) {
      // Window function returns nothing when LIMIT 0 — run a separate COUNT query
      const countSql = ['SELECT COUNT(*) AS cnt', `FROM "${ctx.resource}" ${parentAlias}`, whereClause].filter(s => s.length > 0).join(' ');
      const countRow = db.prepare(countSql).get(...filterValues) as Record<string, unknown> | undefined;
      count = Number(countRow?.cnt ?? 0);
    }

    const entities = rows.map(row => {
      const parent = extractParentColumns(row, parentAlias, selectedFields);
      return deserializeSqliteRow(parent, [...ctx.fields]);
    });

    return { value: entities, ...(count !== undefined ? { count } : {}) };
  };

  const readByKey = async (
    ctx: ResourceContext,
    keyValue: string,
    options?: { readonly $select?: string; readonly $expand?: string }
  ): Promise<SingleResult> => {
    if (options?.$expand) {
      const result = await queryCollection(ctx, {
        $filter: `${ctx.keyField} eq '${keyValue}'`,
        $select: options.$select,
        $expand: options.$expand,
        $top: 1
      });
      return result.value[0];
    }

    const parentAlias = 'p';
    const dataFields = ctx.fields.filter(f => !f.isExpansion);
    const selectCols = buildSelectColumns(dataFields, ctx.keyField, options?.$select, parentAlias);

    const sql = `SELECT ${selectCols.join(', ')} FROM "${ctx.resource}" ${parentAlias} WHERE ${parentAlias}."${ctx.keyField}" = ?`;
    const row = db.prepare(sql).get(keyValue) as Record<string, unknown> | undefined;

    if (!row) return undefined;

    const parent = extractParentColumns(row, parentAlias, dataFields);
    return deserializeSqliteRow(parent, [...dataFields]);
  };

  const insert = async (ctx: ResourceContext, record: Readonly<Record<string, unknown>>): Promise<EntityRecord> => {
    const fieldMap = new Map(ctx.fields.map(f => [f.fieldName, f]));
    const entries = Object.entries(record).filter(([key]) => fieldMap.has(key));

    const columns = entries.map(([key]) => `"${key}"`);
    const placeholders = entries.map(() => '?');
    const values = entries.map(([key, value]) => {
      const field = fieldMap.get(key)!;
      return serializeValue(value, field);
    });

    const sql = `INSERT INTO "${ctx.resource}" (${columns.join(', ')}) VALUES (${placeholders.join(', ')}) RETURNING *`;
    const row = db.prepare(sql).get(...values) as Record<string, unknown>;
    return deserializeSqliteRow(row, [...ctx.fields]);
  };

  const update = async (ctx: ResourceContext, keyValue: string, updates: Readonly<Record<string, unknown>>): Promise<SingleResult> => {
    const fieldMap = new Map(ctx.fields.map(f => [f.fieldName, f]));
    const entries = Object.entries(updates).filter(([key]) => key !== ctx.keyField && fieldMap.has(key));

    if (entries.length === 0) {
      return readByKey(ctx, keyValue);
    }

    const setClauses = entries.map(([key]) => `"${key}" = ?`);
    const values = entries.map(([key, value]) => {
      const field = fieldMap.get(key)!;
      return serializeValue(value, field);
    });
    values.push(keyValue);

    const sql = `UPDATE "${ctx.resource}" SET ${setClauses.join(', ')} WHERE "${ctx.keyField}" = ? RETURNING *`;
    const row = db.prepare(sql).get(...values) as Record<string, unknown> | undefined;

    if (!row) return undefined;
    return deserializeSqliteRow(row, [...ctx.fields]);
  };

  const deleteByKey = async (ctx: ResourceContext, keyValue: string): Promise<boolean> => {
    const sql = `DELETE FROM "${ctx.resource}" WHERE "${ctx.keyField}" = ?`;
    const result = db.prepare(sql).run(keyValue);
    return result.changes > 0;
  };

  return { queryCollection, readByKey, insert, update, deleteByKey };
};
