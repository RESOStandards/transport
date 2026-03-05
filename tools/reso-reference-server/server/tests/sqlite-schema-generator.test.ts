import { describe, expect, it } from 'vitest';
import { edmTypeToSqlite, generateCreateTable, generateSqliteSchema } from '../src/db/sqlite-schema-generator.js';
import type { ResoField } from '../src/metadata/types.js';

const makeField = (overrides: Partial<ResoField> & Pick<ResoField, 'fieldName' | 'type'>): ResoField => ({
  resourceName: 'Property',
  nullable: true,
  annotations: [],
  ...overrides
});

describe('edmTypeToSqlite', () => {
  it('maps Edm.String to TEXT', () => {
    expect(edmTypeToSqlite(makeField({ fieldName: 'City', type: 'Edm.String' }))).toBe('TEXT');
  });

  it('maps Edm.Int64 to INTEGER', () => {
    expect(edmTypeToSqlite(makeField({ fieldName: 'Beds', type: 'Edm.Int64' }))).toBe('INTEGER');
  });

  it('maps Edm.Int32 to INTEGER', () => {
    expect(edmTypeToSqlite(makeField({ fieldName: 'Beds', type: 'Edm.Int32' }))).toBe('INTEGER');
  });

  it('maps Edm.Decimal to REAL', () => {
    expect(edmTypeToSqlite(makeField({ fieldName: 'Price', type: 'Edm.Decimal', precision: 14, scale: 2 }))).toBe('REAL');
  });

  it('maps Edm.Boolean to INTEGER', () => {
    expect(edmTypeToSqlite(makeField({ fieldName: 'Active', type: 'Edm.Boolean' }))).toBe('INTEGER');
  });

  it('maps Edm.DateTimeOffset to TEXT', () => {
    expect(edmTypeToSqlite(makeField({ fieldName: 'ModTime', type: 'Edm.DateTimeOffset' }))).toBe('TEXT');
  });

  it('maps Edm.Date to TEXT', () => {
    expect(edmTypeToSqlite(makeField({ fieldName: 'OnMarketDate', type: 'Edm.Date' }))).toBe('TEXT');
  });

  it('maps Edm.Binary to BLOB', () => {
    expect(edmTypeToSqlite(makeField({ fieldName: 'Data', type: 'Edm.Binary' }))).toBe('BLOB');
  });

  it('maps Edm.Double to REAL', () => {
    expect(edmTypeToSqlite(makeField({ fieldName: 'Lat', type: 'Edm.Double' }))).toBe('REAL');
  });

  it('maps enum types to TEXT', () => {
    expect(edmTypeToSqlite(makeField({ fieldName: 'Status', type: 'org.reso.metadata.enums.StandardStatus' }))).toBe('TEXT');
  });

  it('maps collection fields to TEXT', () => {
    expect(
      edmTypeToSqlite(
        makeField({
          fieldName: 'Features',
          type: 'org.reso.metadata.enums.Features',
          isCollection: true
        })
      )
    ).toBe('TEXT');
  });
});

describe('generateCreateTable', () => {
  it('generates valid DDL with primary key', () => {
    const fields: ResoField[] = [
      makeField({ fieldName: 'ListingKey', type: 'Edm.String', maxLength: 255 }),
      makeField({ fieldName: 'ListPrice', type: 'Edm.Decimal', precision: 14, scale: 2 }),
      makeField({ fieldName: 'City', type: 'Edm.String' })
    ];

    const ddl = generateCreateTable('Property', 'ListingKey', fields);

    expect(ddl).toContain('CREATE TABLE IF NOT EXISTS "Property"');
    expect(ddl).toContain('"ListingKey" TEXT NOT NULL PRIMARY KEY');
    expect(ddl).toContain('"ListPrice" REAL');
    expect(ddl).toContain('"City" TEXT');
  });
});

describe('generateSqliteSchema', () => {
  it('creates FK index for resources with ResourceRecordKey', () => {
    const fields: ResoField[] = [
      makeField({ fieldName: 'MediaKey', type: 'Edm.String' }),
      makeField({ fieldName: 'ResourceName', type: 'Edm.String' }),
      makeField({ fieldName: 'ResourceRecordKey', type: 'Edm.String' })
    ];

    const statements = generateSqliteSchema([{ resourceName: 'Media', keyField: 'MediaKey', fields }]);

    expect(statements).toHaveLength(2);
    expect(statements[1]).toContain('CREATE INDEX IF NOT EXISTS "idx_Media_fk"');
    expect(statements[1]).toContain('"ResourceName", "ResourceRecordKey"');
  });

  it('skips FK index for resources without ResourceRecordKey', () => {
    const fields: ResoField[] = [
      makeField({ fieldName: 'ListingKey', type: 'Edm.String' }),
      makeField({ fieldName: 'City', type: 'Edm.String' })
    ];

    const statements = generateSqliteSchema([{ resourceName: 'Property', keyField: 'ListingKey', fields }]);

    expect(statements).toHaveLength(1);
  });
});
