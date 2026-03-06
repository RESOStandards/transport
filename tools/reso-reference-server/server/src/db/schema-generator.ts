import { isEnumType } from '../metadata/loader.js';
import type { ResoField } from '../metadata/types.js';

/** Maps an Edm type string to a PostgreSQL column type. */
export const edmTypeToSql = (field: ResoField): string => {
  if (field.isCollection) return 'JSONB';

  const type = field.type;

  if (isEnumType(type)) return 'TEXT';

  switch (type) {
    case 'Edm.String':
      return 'TEXT';
    case 'Edm.Int64':
      return 'BIGINT';
    case 'Edm.Int32':
      return 'INTEGER';
    case 'Edm.Int16':
      return 'SMALLINT';
    case 'Edm.Decimal':
      if (field.precision !== undefined && field.scale !== undefined) {
        return `NUMERIC(${field.precision},${field.scale})`;
      }
      return 'NUMERIC';
    case 'Edm.Boolean':
      return 'BOOLEAN';
    case 'Edm.Date':
      return 'DATE';
    case 'Edm.DateTimeOffset':
      return 'TIMESTAMPTZ';
    case 'Edm.Double':
      return 'DOUBLE PRECISION';
    case 'Edm.Single':
      return 'REAL';
    case 'Edm.Byte':
      return 'SMALLINT';
    case 'Edm.Binary':
      return 'BYTEA';
    default:
      return 'TEXT';
  }
};

/** Generates a CREATE TABLE IF NOT EXISTS DDL statement for a resource. */
export const generateCreateTable = (resourceName: string, keyField: string, fields: ReadonlyArray<ResoField>): string => {
  const isEntityEvent = resourceName === 'EntityEvent';

  const columns = fields.map(field => {
    if (isEntityEvent && field.fieldName === keyField) {
      return `  "${field.fieldName}" BIGSERIAL NOT NULL PRIMARY KEY`;
    }
    const sqlType = edmTypeToSql(field);
    const pk = field.fieldName === keyField ? ' PRIMARY KEY' : '';
    const nullable = field.fieldName === keyField ? ' NOT NULL' : '';
    return `  "${field.fieldName}" ${sqlType}${nullable}${pk}`;
  });

  return `CREATE TABLE IF NOT EXISTS "${resourceName}" (\n${columns.join(',\n')}\n);`;
};

/** Generates CREATE TABLE DDL for multiple resources. */
export const generateSchema = (
  resourceFields: ReadonlyArray<{
    readonly resourceName: string;
    readonly keyField: string;
    readonly fields: ReadonlyArray<ResoField>;
  }>
): ReadonlyArray<string> => {
  const statements: string[] = [];

  for (const { resourceName, keyField, fields } of resourceFields) {
    statements.push(generateCreateTable(resourceName, keyField, fields));

    if (resourceName === 'EntityEvent') {
      statements.push('CREATE INDEX IF NOT EXISTS "idx_EntityEvent_resource" ON "EntityEvent" ("ResourceName", "ResourceRecordKey");');
    }
  }

  return statements;
};
