import type { ResoField } from "../metadata/types.js";

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

/** Deserializes a database row value back to its API representation. */
const deserializeValue = (value: unknown, field: ResoField): unknown => {
  if (field.isCollection && typeof value === "string") {
    return JSON.parse(value) as unknown;
  }
  return value;
};

/** Deserializes an entire database row using field definitions. */
export const deserializeRow = (
  row: Record<string, unknown>,
  fields: ReadonlyArray<ResoField>,
): Record<string, unknown> => {
  const fieldMap = new Map(fields.map((f) => [f.fieldName, f]));
  return Object.fromEntries(
    Object.entries(row).map(([key, value]) => {
      const field = fieldMap.get(key);
      return [key, field ? deserializeValue(value, field) : value];
    }),
  );
};

/** Builds an INSERT query for a new record. */
export const buildInsertQuery = (
  tableName: string,
  record: Readonly<Record<string, unknown>>,
  fields: ReadonlyArray<ResoField>,
): QueryConfig => {
  const fieldMap = new Map(fields.map((f) => [f.fieldName, f]));
  const entries = Object.entries(record).filter(
    ([key]) => fieldMap.has(key),
  );

  const columns = entries.map(([key]) => `"${key}"`);
  const placeholders = entries.map((_, i) => `$${i + 1}`);
  const values = entries.map(([key, value]) => {
    const field = fieldMap.get(key)!;
    return serializeValue(value, field);
  });

  return {
    text: `INSERT INTO "${tableName}" (${columns.join(", ")}) VALUES (${placeholders.join(", ")}) RETURNING *`,
    values,
  };
};

/** Builds a SELECT query for a single record by primary key. */
export const buildSelectByKeyQuery = (
  tableName: string,
  keyField: string,
  keyValue: string,
): QueryConfig => ({
  text: `SELECT * FROM "${tableName}" WHERE "${keyField}" = $1`,
  values: [keyValue],
});

/** Builds an UPDATE query for an existing record (merge semantics). */
export const buildUpdateQuery = (
  tableName: string,
  keyField: string,
  keyValue: string,
  updates: Readonly<Record<string, unknown>>,
  fields: ReadonlyArray<ResoField>,
): QueryConfig => {
  const fieldMap = new Map(fields.map((f) => [f.fieldName, f]));
  const entries = Object.entries(updates).filter(
    ([key]) => key !== keyField && fieldMap.has(key),
  );

  const setClauses = entries.map(([key], i) => `"${key}" = $${i + 1}`);
  const values = entries.map(([key, value]) => {
    const field = fieldMap.get(key)!;
    return serializeValue(value, field);
  });

  values.push(keyValue);

  return {
    text: `UPDATE "${tableName}" SET ${setClauses.join(", ")} WHERE "${keyField}" = $${values.length} RETURNING *`,
    values,
  };
};

/** Builds a DELETE query for a record by primary key. */
export const buildDeleteQuery = (
  tableName: string,
  keyField: string,
  keyValue: string,
): QueryConfig => ({
  text: `DELETE FROM "${tableName}" WHERE "${keyField}" = $1`,
  values: [keyValue],
});
