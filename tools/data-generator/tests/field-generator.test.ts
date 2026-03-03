import { describe, expect, it } from 'vitest';
import {
  generateFieldValue,
  generateRecord,
  generateRecords,
  isEnumType,
  randomChoice,
  randomDecimal,
  randomInt
} from '../src/generators/field-generator.js';
import type { ResoField, ResoLookup } from '../src/generators/types.js';

const makeField = (overrides: Partial<ResoField> = {}): ResoField => ({
  resourceName: 'Property',
  fieldName: 'TestField',
  type: 'Edm.String',
  nullable: true,
  annotations: [],
  ...overrides
});

const SAMPLE_LOOKUPS: Record<string, ReadonlyArray<ResoLookup>> = {
  'org.reso.metadata.enums.StandardStatus': [
    { lookupName: 'org.reso.metadata.enums.StandardStatus', lookupValue: 'Active', type: 'Edm.Int32', annotations: [] },
    { lookupName: 'org.reso.metadata.enums.StandardStatus', lookupValue: 'Pending', type: 'Edm.Int32', annotations: [] },
    { lookupName: 'org.reso.metadata.enums.StandardStatus', lookupValue: 'Closed', type: 'Edm.Int32', annotations: [] }
  ]
};

describe('isEnumType', () => {
  it('returns false for Edm types', () => {
    expect(isEnumType('Edm.String')).toBe(false);
    expect(isEnumType('Edm.Int32')).toBe(false);
    expect(isEnumType('Edm.Decimal')).toBe(false);
  });

  it('returns true for non-Edm types', () => {
    expect(isEnumType('org.reso.metadata.enums.StandardStatus')).toBe(true);
    expect(isEnumType('CustomType')).toBe(true);
  });
});

describe('randomInt', () => {
  it('returns values within range', () => {
    for (let i = 0; i < 50; i++) {
      const val = randomInt(1, 10);
      expect(val).toBeGreaterThanOrEqual(1);
      expect(val).toBeLessThanOrEqual(10);
      expect(Number.isInteger(val)).toBe(true);
    }
  });
});

describe('randomDecimal', () => {
  it('returns values within range with correct decimal places', () => {
    for (let i = 0; i < 50; i++) {
      const val = randomDecimal(0, 100, 2);
      expect(val).toBeGreaterThanOrEqual(0);
      expect(val).toBeLessThanOrEqual(100);
      // Check decimal places
      const parts = String(val).split('.');
      if (parts[1]) {
        expect(parts[1].length).toBeLessThanOrEqual(2);
      }
    }
  });
});

describe('randomChoice', () => {
  it('returns an element from the array', () => {
    const arr = ['a', 'b', 'c'];
    for (let i = 0; i < 20; i++) {
      expect(arr).toContain(randomChoice(arr));
    }
  });
});

describe('generateFieldValue', () => {
  it('generates string values', () => {
    const field = makeField({ type: 'Edm.String', fieldName: 'City' });
    const value = generateFieldValue(field, {}, 0);
    expect(typeof value).toBe('string');
    expect((value as string).length).toBeGreaterThan(0);
  });

  it('respects maxLength for strings', () => {
    const field = makeField({ type: 'Edm.String', fieldName: 'VeryLongFieldNameThatMakesTheSampleStringExceedTheLimit', maxLength: 10 });
    const value = generateFieldValue(field, {}, 0);
    expect(typeof value).toBe('string');
    expect((value as string).length).toBeLessThanOrEqual(10);
  });

  it('generates boolean values', () => {
    const field = makeField({ type: 'Edm.Boolean' });
    const value = generateFieldValue(field, {}, 0);
    expect(typeof value).toBe('boolean');
  });

  it('generates Int32 values', () => {
    const field = makeField({ type: 'Edm.Int32' });
    const value = generateFieldValue(field, {}, 0);
    expect(typeof value).toBe('number');
    expect(Number.isInteger(value)).toBe(true);
  });

  it('generates Int64 values', () => {
    const field = makeField({ type: 'Edm.Int64' });
    const value = generateFieldValue(field, {}, 0);
    expect(typeof value).toBe('number');
    expect(Number.isInteger(value)).toBe(true);
  });

  it('generates Decimal values', () => {
    const field = makeField({ type: 'Edm.Decimal', scale: 2 });
    const value = generateFieldValue(field, {}, 0);
    expect(typeof value).toBe('number');
  });

  it('respects precision/scale constraints for Decimal fields', () => {
    // NUMERIC(5,2) allows max 999.99 (3 integer digits + 2 decimal)
    const field = makeField({ type: 'Edm.Decimal', precision: 5, scale: 2 });
    for (let i = 0; i < 100; i++) {
      const value = generateFieldValue(field, {}, i) as number;
      expect(value).toBeGreaterThanOrEqual(0);
      expect(value).toBeLessThanOrEqual(999);
    }
  });

  it('respects precision/scale for high-precision fields', () => {
    // NUMERIC(12,8) allows max 9999.99999999
    const field = makeField({ type: 'Edm.Decimal', precision: 12, scale: 8 });
    for (let i = 0; i < 100; i++) {
      const value = generateFieldValue(field, {}, i) as number;
      expect(value).toBeGreaterThanOrEqual(0);
      expect(value).toBeLessThanOrEqual(9999);
    }
  });

  it('generates Date values', () => {
    const field = makeField({ type: 'Edm.Date' });
    const value = generateFieldValue(field, {}, 0);
    expect(typeof value).toBe('string');
    expect(value).toMatch(/^\d{4}-\d{2}-\d{2}$/);
  });

  it('generates DateTimeOffset values', () => {
    const field = makeField({ type: 'Edm.DateTimeOffset' });
    const value = generateFieldValue(field, {}, 0);
    expect(typeof value).toBe('string');
    expect(value).toMatch(/^\d{4}-\d{2}-\d{2}T/);
  });

  it('generates TimeOfDay values', () => {
    const field = makeField({ type: 'Edm.TimeOfDay' });
    const value = generateFieldValue(field, {}, 0);
    expect(typeof value).toBe('string');
    expect(value).toMatch(/^\d{2}:\d{2}:\d{2}$/);
  });

  it('generates Guid values', () => {
    const field = makeField({ type: 'Edm.Guid' });
    const value = generateFieldValue(field, {}, 0);
    expect(typeof value).toBe('string');
    expect(value).toMatch(/^[\da-f]{8}-[\da-f]{4}-[\da-f]{4}-[\da-f]{4}-[\da-f]{12}$/);
  });

  it('generates enum values from lookups', () => {
    const field = makeField({ type: 'org.reso.metadata.enums.StandardStatus' });
    const value = generateFieldValue(field, SAMPLE_LOOKUPS, 0);
    expect(['Active', 'Pending', 'Closed']).toContain(value);
  });

  it('returns undefined for enum without lookups', () => {
    const field = makeField({ type: 'org.reso.metadata.enums.UnknownEnum' });
    const value = generateFieldValue(field, {}, 0);
    expect(value).toBeUndefined();
  });

  it('skips key fields', () => {
    const field = makeField({ fieldName: 'ListingKey' });
    const value = generateFieldValue(field, {}, 0);
    expect(value).toBeUndefined();
  });

  it('skips ModificationTimestamp', () => {
    const field = makeField({ fieldName: 'ModificationTimestamp', type: 'Edm.DateTimeOffset' });
    const value = generateFieldValue(field, {}, 0);
    expect(value).toBeUndefined();
  });

  it('generates collection values', () => {
    const field = makeField({
      fieldName: 'Appliances',
      type: 'Collection(org.reso.metadata.enums.Appliances)',
      isCollection: true
    });
    const lookups: Record<string, ReadonlyArray<ResoLookup>> = {
      'org.reso.metadata.enums.Appliances': [
        { lookupName: 'org.reso.metadata.enums.Appliances', lookupValue: 'Dishwasher', type: 'Edm.Int32', annotations: [] },
        { lookupName: 'org.reso.metadata.enums.Appliances', lookupValue: 'Dryer', type: 'Edm.Int32', annotations: [] },
        { lookupName: 'org.reso.metadata.enums.Appliances', lookupValue: 'Oven', type: 'Edm.Int32', annotations: [] }
      ]
    };
    const value = generateFieldValue(field, lookups, 0);
    expect(Array.isArray(value)).toBe(true);
    expect((value as string[]).length).toBeGreaterThan(0);
    for (const v of value as string[]) {
      expect(['Dishwasher', 'Dryer', 'Oven']).toContain(v);
    }
  });
});

describe('generateRecord', () => {
  it('produces a record with populated fields', () => {
    const fields: ResoField[] = [
      makeField({ fieldName: 'Name', type: 'Edm.String', maxLength: 50 }),
      makeField({ fieldName: 'Price', type: 'Edm.Decimal', scale: 2 }),
      makeField({ fieldName: 'Active', type: 'Edm.Boolean' })
    ];
    const record = generateRecord(fields, {}, 0, 1.0);
    expect(record.Name).toBeDefined();
    expect(typeof record.Name).toBe('string');
    expect(record.Price).toBeDefined();
    expect(typeof record.Price).toBe('number');
    expect(record.Active).toBeDefined();
    expect(typeof record.Active).toBe('boolean');
  });

  it('skips key fields in generated records', () => {
    const fields: ResoField[] = [
      makeField({ fieldName: 'ListingKey', type: 'Edm.String' }),
      makeField({ fieldName: 'City', type: 'Edm.String' })
    ];
    const record = generateRecord(fields, {}, 0, 1.0);
    expect(record.ListingKey).toBeUndefined();
    expect(record.City).toBeDefined();
  });
});

describe('generateRecords', () => {
  it('generates the requested number of records', () => {
    const fields: ResoField[] = [
      makeField({ fieldName: 'Name', type: 'Edm.String', maxLength: 50, nullable: false }),
      makeField({ fieldName: 'Count', type: 'Edm.Int32', nullable: false })
    ];
    const records = generateRecords(fields, {}, 5);
    expect(records).toHaveLength(5);
    for (const record of records) {
      expect(record.Name).toBeDefined();
      expect(record.Count).toBeDefined();
    }
  });
});
