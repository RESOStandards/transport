import { isEnumType } from '../metadata/loader.js';
import type { ResoField } from '../metadata/types.js';

/** Maps an Edm type string to a SQLite column type. */
export const edmTypeToSqlite = (field: ResoField): string => {
  if (field.isCollection) return 'TEXT';

  const type = field.type;

  if (isEnumType(type)) return 'TEXT';

  switch (type) {
    case 'Edm.String':
      return 'TEXT';
    case 'Edm.Int64':
    case 'Edm.Int32':
    case 'Edm.Int16':
    case 'Edm.Byte':
      return 'INTEGER';
    case 'Edm.Decimal':
    case 'Edm.Double':
    case 'Edm.Single':
      return 'REAL';
    case 'Edm.Boolean':
      return 'INTEGER';
    case 'Edm.Date':
    case 'Edm.DateTimeOffset':
      return 'TEXT';
    case 'Edm.Binary':
      return 'BLOB';
    default:
      return 'TEXT';
  }
};

/** Generates a CREATE TABLE IF NOT EXISTS DDL statement for a resource. */
export const generateCreateTable = (resourceName: string, keyField: string, fields: ReadonlyArray<ResoField>): string => {
  const isEntityEvent = resourceName === 'EntityEvent';

  const columns = fields.map(field => {
    if (isEntityEvent && field.fieldName === keyField) {
      return `  "${field.fieldName}" INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT`;
    }
    const sqlType = edmTypeToSqlite(field);
    const pk = field.fieldName === keyField ? ' PRIMARY KEY' : '';
    const nullable = field.fieldName === keyField ? ' NOT NULL' : '';
    return `  "${field.fieldName}" ${sqlType}${nullable}${pk}`;
  });

  return `CREATE TABLE IF NOT EXISTS "${resourceName}" (\n${columns.join(',\n')}\n);`;
};

/** Generates CREATE TABLE DDL + FK indexes for multiple resources. */
export const generateSqliteSchema = (
  resourceFields: ReadonlyArray<{
    readonly resourceName: string;
    readonly keyField: string;
    readonly fields: ReadonlyArray<ResoField>;
  }>
): ReadonlyArray<string> => {
  const statements: string[] = [];

  for (const { resourceName, keyField, fields } of resourceFields) {
    statements.push(generateCreateTable(resourceName, keyField, fields));

    // Create index for ResourceName + ResourceRecordKey FK pattern (polymorphic)
    if (fields.some(f => f.fieldName === 'ResourceRecordKey')) {
      statements.push(`CREATE INDEX IF NOT EXISTS "idx_${resourceName}_fk" ON "${resourceName}" ("ResourceName", "ResourceRecordKey");`);
    }
  }

  return statements;
};
