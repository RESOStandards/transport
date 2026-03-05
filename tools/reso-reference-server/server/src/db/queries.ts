import type { ResoField } from '../metadata/types.js';

/** A parameterized SQL query ready for pg.Pool.query(). */
export interface QueryConfig {
  readonly text: string;
  readonly values: ReadonlyArray<unknown>;
}

/** Serializes a value for PostgreSQL insertion based on the field definition. */
const serializeValue = (value: unknown, field: ResoField): unknown => {
  if (field.isCollection && Array.isArray(value)) {
    return JSON.stringify(value);
  }
  return value;
};

/** Edm types that must be returned as JavaScript integers. */
const INT_TYPES = new Set(['Edm.Int16', 'Edm.Int32', 'Edm.Int64', 'Edm.Byte']);

/** Edm types that must be returned as JavaScript decimals. */
const DECIMAL_TYPES = new Set(['Edm.Decimal', 'Edm.Double', 'Edm.Single']);

/** Deserializes a database row value back to its API representation. */
const deserializeValue = (value: unknown, field: ResoField): unknown => {
  if (value == null) return value;
  if (field.isCollection && typeof value === 'string') {
    return JSON.parse(value) as unknown;
  }
  // PostgreSQL returns BIGINT/NUMERIC as strings — coerce to JS numbers
  if (typeof value === 'string' && INT_TYPES.has(field.type)) {
    const n = Number(value);
    return Number.isFinite(n) ? Math.trunc(n) : value;
  }
  if (typeof value === 'string' && DECIMAL_TYPES.has(field.type)) {
    const n = Number(value);
    return Number.isFinite(n) ? n : value;
  }
  // Edm.Date must return ISO 8601 date-only (YYYY-MM-DD), not a full timestamp
  if (field.type === 'Edm.Date') {
    if (value instanceof Date) {
      return value.toISOString().split('T')[0];
    }
    if (typeof value === 'string' && value.includes('T')) {
      return value.split('T')[0];
    }
  }
  return value;
};

/** Deserializes an entire database row using field definitions. */
export const deserializeRow = (row: Record<string, unknown>, fields: ReadonlyArray<ResoField>): Record<string, unknown> => {
  const fieldMap = new Map(fields.map(f => [f.fieldName, f]));
  return Object.fromEntries(
    Object.entries(row).map(([key, value]) => {
      const field = fieldMap.get(key);
      return [key, field ? deserializeValue(value, field) : value];
    })
  );
};

/** Builds an INSERT query for a new record. */
export const buildInsertQuery = (
  tableName: string,
  record: Readonly<Record<string, unknown>>,
  fields: ReadonlyArray<ResoField>
): QueryConfig => {
  const fieldMap = new Map(fields.map(f => [f.fieldName, f]));
  const entries = Object.entries(record).filter(([key]) => fieldMap.has(key));

  const columns = entries.map(([key]) => `"${key}"`);
  const placeholders = entries.map((_, i) => `$${i + 1}`);
  const values = entries.map(([key, value]) => {
    const field = fieldMap.get(key)!;
    return serializeValue(value, field);
  });

  return {
    text: `INSERT INTO "${tableName}" (${columns.join(', ')}) VALUES (${placeholders.join(', ')}) RETURNING *`,
    values
  };
};

/** Builds a SELECT query for a single record by primary key. */
export const buildSelectByKeyQuery = (tableName: string, keyField: string, keyValue: string): QueryConfig => ({
  text: `SELECT * FROM "${tableName}" WHERE "${keyField}" = $1`,
  values: [keyValue]
});

/** Builds an UPDATE query for an existing record (merge semantics). */
export const buildUpdateQuery = (
  tableName: string,
  keyField: string,
  keyValue: string,
  updates: Readonly<Record<string, unknown>>,
  fields: ReadonlyArray<ResoField>
): QueryConfig => {
  const fieldMap = new Map(fields.map(f => [f.fieldName, f]));
  const entries = Object.entries(updates).filter(([key]) => key !== keyField && fieldMap.has(key));

  const setClauses = entries.map(([key], i) => `"${key}" = $${i + 1}`);
  const values = entries.map(([key, value]) => {
    const field = fieldMap.get(key)!;
    return serializeValue(value, field);
  });

  values.push(keyValue);

  return {
    text: `UPDATE "${tableName}" SET ${setClauses.join(', ')} WHERE "${keyField}" = $${values.length} RETURNING *`,
    values
  };
};

/** Builds a DELETE query for a record by primary key. */
export const buildDeleteQuery = (tableName: string, keyField: string, keyValue: string): QueryConfig => ({
  text: `DELETE FROM "${tableName}" WHERE "${keyField}" = $1`,
  values: [keyValue]
});
