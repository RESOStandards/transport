/**
 * PostgreSQL implementation of the Data Access Layer.
 *
 * Uses LEFT JOINs for $expand resolution with app-side grouping to nest
 * expanded navigation properties into their parent entities.
 */

import type pg from 'pg';
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
import { filterToSql } from './filter-to-sql.js';
import { deserializeRow } from './queries.js';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Serialize a value for PostgreSQL insertion based on field definition. */
const serializeValue = (value: unknown, field: ResoField): unknown => {
  if (field.isCollection && Array.isArray(value)) {
    return JSON.stringify(value);
  }
  return value;
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
      // Strip any parenthesized options for now (single-level only)
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
    requested.add(keyField); // always include the key
    selectedNames = fields.filter(f => requested.has(f.fieldName)).map(f => f.fieldName);
  } else {
    selectedNames = fields.map(f => f.fieldName);
  }
  return selectedNames.map(name => `${alias}."${name}" AS "${alias}.${name}"`);
};

/**
 * Build SELECT column list for an expanded navigation property.
 * All columns are aliased as "nav_<NavName>.<ColumnName>".
 */
const buildNavSelectColumns = (binding: NavigationPropertyBinding, alias: string): ReadonlyArray<string> =>
  binding.targetFields.map(f => `${alias}."${f.fieldName}" AS "${alias}.${f.fieldName}"`);

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
  // direct FK
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
  // If all columns are null, the LEFT JOIN didn't match — return undefined
  return hasNonNull ? result : undefined;
};

/**
 * Group flat JOIN result rows into parent entities with nested navprops.
 *
 * Groups by parent key value. For to-many navprops, collects all matching
 * rows into an array. For to-one, takes the first non-null match.
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
      const deserializedParent = deserializeRow(parentData, parentFields);
      grouped.set(parentKey, {
        parent: deserializedParent,
        navs: new Map(expandBindings.map(({ binding }) => [binding.name, []]))
      });
    }

    const entry = grouped.get(parentKey)!;
    for (const { binding, alias } of expandBindings) {
      const navData = extractNavColumns(row, alias, binding);
      if (navData) {
        const deserialized = deserializeRow(navData, [...binding.targetFields]);
        entry.navs.get(binding.name)?.push(deserialized);
      }
    }
  }

  // Assemble final entities with inline expanded navprops
  const result: EntityRecord[] = [];
  for (const { parent, navs } of grouped.values()) {
    const entity: Record<string, unknown> = { ...parent };
    for (const { binding } of expandBindings) {
      const navRows = navs.get(binding.name) ?? [];
      if (binding.isCollection) {
        // Deduplicate by target key
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
// PostgreSQL DAL implementation
// ---------------------------------------------------------------------------

/** Creates a PostgreSQL Data Access Layer implementation. */
export const createPostgresDal = (pool: pg.Pool): DataAccessLayer => {
  const queryCollection = async (ctx: ResourceContext, options?: CollectionQueryOptions): Promise<CollectionResult> => {
    const parentAlias = 'p';
    const values: unknown[] = [];
    let paramIndex = 1;

    // Determine which fields to select
    const fieldNames = new Set(ctx.fields.map(f => f.fieldName));
    let selectedFields: ReadonlyArray<ResoField>;
    if (options?.$select) {
      const requested = new Set(parseSelect(options.$select));
      requested.add(ctx.keyField);
      selectedFields = ctx.fields.filter(f => requested.has(f.fieldName));
    } else {
      selectedFields = ctx.fields;
    }

    // Build SELECT columns for parent
    const selectCols: string[] = [...buildSelectColumns(ctx.fields, ctx.keyField, options?.$select, parentAlias)];

    // Resolve $expand bindings
    const expandBindings: Array<{
      readonly binding: NavigationPropertyBinding;
      readonly alias: string;
    }> = [];
    const joinClauses: string[] = [];

    if (options?.$expand) {
      const expandNames = parseExpand(options.$expand);
      const bindingMap = new Map(ctx.navigationBindings.map(b => [b.name, b]));

      for (const name of expandNames) {
        const binding = bindingMap.get(name);
        if (!binding) continue;

        const navAlias = `nav_${name}`;
        expandBindings.push({ binding, alias: navAlias });

        selectCols.push(...buildNavSelectColumns(binding, navAlias));
        joinClauses.push(buildExpandJoin(ctx.resource, ctx.keyField, parentAlias, binding, navAlias));
      }
    }

    // Build FROM clause
    let fromClause = `"${ctx.resource}" ${parentAlias}`;
    if (joinClauses.length > 0) {
      fromClause += ` ${joinClauses.join(' ')}`;
    }

    // Build WHERE clause from $filter
    let whereClause = '';
    if (options?.$filter) {
      const filterResult = filterToSql(options.$filter, ctx.fields, parentAlias, paramIndex);
      whereClause = `WHERE ${filterResult.where}`;
      values.push(...filterResult.values);
      paramIndex += filterResult.values.length;
    }

    // Build ORDER BY
    let orderByClause = '';
    if (options?.$orderby) {
      const parsed = parseOrderBy(options.$orderby, ctx.fields, parentAlias);
      if (parsed) {
        orderByClause = `ORDER BY ${parsed}`;
      }
    }

    // Build LIMIT/OFFSET
    let limitClause = '';
    if (options?.$top !== undefined) {
      limitClause = `LIMIT $${paramIndex}`;
      values.push(options.$top);
      paramIndex++;
    }

    let offsetClause = '';
    if (options?.$skip !== undefined) {
      offsetClause = `OFFSET $${paramIndex}`;
      values.push(options.$skip);
      paramIndex++;
    }

    // $count — use a window function so we get total count with the query
    const countCol = options?.$count ? `, COUNT(*) OVER() AS "__total_count"` : '';

    // Assemble final SQL
    const sql = [`SELECT ${selectCols.join(', ')}${countCol}`, `FROM ${fromClause}`, whereClause, orderByClause, limitClause, offsetClause]
      .filter(s => s.length > 0)
      .join(' ');

    const result = await pool.query(sql, values);
    const rows = result.rows as Record<string, unknown>[];

    // Extract total count if requested
    let count: number | undefined;
    if (options?.$count && rows.length > 0) {
      count = Number(rows[0].__total_count);
    }

    // If no $expand, skip grouping — just extract parent columns
    if (expandBindings.length === 0) {
      const entities = rows.map(row => {
        const parent = extractParentColumns(row, parentAlias, selectedFields);
        return deserializeRow(parent, [...ctx.fields]);
      });
      return { value: entities, ...(count !== undefined ? { count } : {}) };
    }

    // Group rows by parent key and nest expanded navprops
    const entities = groupRows(rows, parentAlias, ctx.keyField, ctx.fields, selectedFields, expandBindings);

    return { value: entities, ...(count !== undefined ? { count } : {}) };
  };

  const readByKey = async (
    ctx: ResourceContext,
    keyValue: string,
    options?: { readonly $select?: string; readonly $expand?: string }
  ): Promise<SingleResult> => {
    // If $expand is requested, delegate to queryCollection with a key filter
    if (options?.$expand) {
      const result = await queryCollection(ctx, {
        $filter: `${ctx.keyField} eq '${keyValue}'`,
        $select: options.$select,
        $expand: options.$expand,
        $top: 1
      });
      return result.value[0];
    }

    // Simple key lookup without expansion
    const parentAlias = 'p';
    const selectCols = buildSelectColumns(ctx.fields, ctx.keyField, options?.$select, parentAlias);

    const sql = `SELECT ${selectCols.join(', ')} FROM "${ctx.resource}" ${parentAlias} WHERE ${parentAlias}."${ctx.keyField}" = $1`;
    const result = await pool.query(sql, [keyValue]);

    if (result.rows.length === 0) return undefined;

    const row = result.rows[0] as Record<string, unknown>;
    const parent = extractParentColumns(row, parentAlias, ctx.fields);
    return deserializeRow(parent, [...ctx.fields]);
  };

  const insert = async (ctx: ResourceContext, record: Readonly<Record<string, unknown>>): Promise<EntityRecord> => {
    const fieldMap = new Map(ctx.fields.map(f => [f.fieldName, f]));
    const entries = Object.entries(record).filter(([key]) => fieldMap.has(key));

    const columns = entries.map(([key]) => `"${key}"`);
    const placeholders = entries.map((_, i) => `$${i + 1}`);
    const values = entries.map(([key, value]) => {
      const field = fieldMap.get(key)!;
      return serializeValue(value, field);
    });

    const sql = `INSERT INTO "${ctx.resource}" (${columns.join(', ')}) VALUES (${placeholders.join(', ')}) RETURNING *`;
    const result = await pool.query(sql, values);
    return deserializeRow(result.rows[0] as Record<string, unknown>, [...ctx.fields]);
  };

  const update = async (ctx: ResourceContext, keyValue: string, updates: Readonly<Record<string, unknown>>): Promise<SingleResult> => {
    const fieldMap = new Map(ctx.fields.map(f => [f.fieldName, f]));
    const entries = Object.entries(updates).filter(([key]) => key !== ctx.keyField && fieldMap.has(key));

    if (entries.length === 0) {
      // No updatable fields — just return the existing record
      return readByKey(ctx, keyValue);
    }

    const setClauses = entries.map(([key], i) => `"${key}" = $${i + 1}`);
    const values = entries.map(([key, value]) => {
      const field = fieldMap.get(key)!;
      return serializeValue(value, field);
    });
    values.push(keyValue);

    const sql = `UPDATE "${ctx.resource}" SET ${setClauses.join(', ')} WHERE "${ctx.keyField}" = $${values.length} RETURNING *`;
    const result = await pool.query(sql, values);

    if (result.rows.length === 0) return undefined;
    return deserializeRow(result.rows[0] as Record<string, unknown>, [...ctx.fields]);
  };

  const deleteByKey = async (ctx: ResourceContext, keyValue: string): Promise<boolean> => {
    const sql = `DELETE FROM "${ctx.resource}" WHERE "${ctx.keyField}" = $1`;
    const result = await pool.query(sql, [keyValue]);
    return (result.rowCount ?? 0) > 0;
  };

  return { queryCollection, readByKey, insert, update, deleteByKey };
};
