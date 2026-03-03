import { describe, expect, it } from 'vitest';
import { isEnumType, isIntegerEdmType, isNumericEdmType } from '../src/metadata/helpers.js';
import type { ResoField } from '../src/metadata/types.js';
import { validateRecord } from '../src/metadata/validate.js';

const makeField = (overrides: Partial<ResoField> & Pick<ResoField, 'fieldName' | 'type'>): ResoField => ({
  resourceName: 'Property',
  nullable: true,
  annotations: [],
  ...overrides
});

/** Base record satisfying all required address fields for Property. */
const VALID_ADDRESS = { City: 'Austin', StateOrProvince: 'TX', PostalCode: '78701', Country: 'US' };

const fields: ResoField[] = [
  makeField({ fieldName: 'ListingKey', type: 'Edm.String', maxLength: 255 }),
  makeField({ fieldName: 'ListPrice', type: 'Edm.Decimal', precision: 14, scale: 2 }),
  makeField({ fieldName: 'BedroomsTotal', type: 'Edm.Int32' }),
  makeField({ fieldName: 'LivingAreaTotal', type: 'Edm.Int64' }),
  makeField({ fieldName: 'City', type: 'Edm.String' }),
  makeField({ fieldName: 'StateOrProvince', type: 'Edm.String', maxLength: 2 }),
  makeField({ fieldName: 'PostalCode', type: 'Edm.String', maxLength: 10 }),
  makeField({ fieldName: 'Country', type: 'Edm.String', maxLength: 2 }),
  makeField({ fieldName: 'StandardStatus', type: 'org.reso.metadata.enums.StandardStatus' }),
  makeField({
    fieldName: 'AccessibilityFeatures',
    type: 'org.reso.metadata.enums.AccessibilityFeatures',
    isCollection: true
  }),
  makeField({ fieldName: 'ActiveYN', type: 'Edm.Boolean' }),
  makeField({ fieldName: 'CloseDate', type: 'Edm.Date' }),
  makeField({ fieldName: 'ModificationTimestamp', type: 'Edm.DateTimeOffset' }),
  makeField({ fieldName: 'LotSizeAcres', type: 'Edm.Double' }),
  makeField({ fieldName: 'Remarks', type: 'Edm.String', maxLength: 50 })
];

describe('validateRecord', () => {
  it('returns empty array for a valid payload', () => {
    const body = {
      ...VALID_ADDRESS,
      ListPrice: 250000,
      BedroomsTotal: 3
    };
    expect(validateRecord(body, fields)).toHaveLength(0);
  });

  it('returns required field failures for an empty body', () => {
    const failures = validateRecord({}, fields);
    expect(failures).toHaveLength(4);
    expect(failures.every(f => f.reason.includes('required'))).toBe(true);
  });

  it('detects unknown fields', () => {
    const failures = validateRecord({ ...VALID_ADDRESS, UnknownField: 'value' }, fields);
    expect(failures).toHaveLength(1);
    expect(failures[0].field).toBe('UnknownField');
    expect(failures[0].reason).toContain('not a recognized field');
  });

  it('skips OData annotations', () => {
    const failures = validateRecord({ ...VALID_ADDRESS, '@odata.context': 'something', ListPrice: 100 }, fields);
    expect(failures).toHaveLength(0);
  });

  it('allows null values for non-required fields', () => {
    expect(validateRecord({ ...VALID_ADDRESS, Remarks: null }, fields)).toHaveLength(0);
  });

  it('allows undefined values for non-required fields', () => {
    expect(validateRecord({ ...VALID_ADDRESS, Remarks: undefined }, fields)).toHaveLength(0);
  });

  it('allows empty string values for non-required fields', () => {
    expect(validateRecord({ ...VALID_ADDRESS, Remarks: '' }, fields)).toHaveLength(0);
  });

  describe('string validation', () => {
    it('accepts valid strings', () => {
      expect(validateRecord({ ...VALID_ADDRESS }, fields)).toHaveLength(0);
    });

    it('rejects non-string values for Edm.String fields', () => {
      const failures = validateRecord({ ...VALID_ADDRESS, Remarks: 12345 }, fields);
      expect(failures).toHaveLength(1);
      expect(failures[0].field).toBe('Remarks');
      expect(failures[0].reason).toBe('Must be text.');
    });

    it('enforces maxLength', () => {
      const failures = validateRecord({ ...VALID_ADDRESS, Remarks: 'x'.repeat(51) }, fields);
      expect(failures).toHaveLength(1);
      expect(failures[0].field).toBe('Remarks');
      expect(failures[0].reason).toContain('maximum length of 50');
      expect(failures[0].reason).toContain('currently 51');
    });

    it('allows strings at exactly maxLength', () => {
      expect(validateRecord({ ...VALID_ADDRESS, Remarks: 'x'.repeat(50) }, fields)).toHaveLength(0);
    });
  });

  describe('numeric validation', () => {
    it('accepts valid decimals', () => {
      expect(validateRecord({ ...VALID_ADDRESS, ListPrice: 250000.5 }, fields)).toHaveLength(0);
    });

    it('accepts valid doubles', () => {
      expect(validateRecord({ ...VALID_ADDRESS, LotSizeAcres: 1.5 }, fields)).toHaveLength(0);
    });

    it('rejects non-number values for numeric fields', () => {
      const failures = validateRecord({ ...VALID_ADDRESS, ListPrice: 'not-a-number' }, fields);
      expect(failures).toHaveLength(1);
      expect(failures[0].field).toBe('ListPrice');
      expect(failures[0].reason).toBe('Must be a number.');
    });

    it('rejects negative numbers', () => {
      const failures = validateRecord({ ...VALID_ADDRESS, ListPrice: -100 }, fields);
      expect(failures).toHaveLength(1);
      expect(failures[0].field).toBe('ListPrice');
      expect(failures[0].reason).toContain('greater than or equal to 0');
    });
  });

  describe('integer validation', () => {
    it('accepts valid integers for Int32', () => {
      expect(validateRecord({ ...VALID_ADDRESS, BedroomsTotal: 3 }, fields)).toHaveLength(0);
    });

    it('accepts valid integers for Int64', () => {
      expect(validateRecord({ ...VALID_ADDRESS, LivingAreaTotal: 2500 }, fields)).toHaveLength(0);
    });

    it('rejects non-number values for integer fields', () => {
      const failures = validateRecord({ ...VALID_ADDRESS, BedroomsTotal: 'three' }, fields);
      expect(failures).toHaveLength(1);
      expect(failures[0].field).toBe('BedroomsTotal');
      expect(failures[0].reason).toBe('Must be a whole number.');
    });

    it('rejects decimal values for integer fields', () => {
      const failures = validateRecord({ ...VALID_ADDRESS, BedroomsTotal: 3.5 }, fields);
      expect(failures).toHaveLength(1);
      expect(failures[0].field).toBe('BedroomsTotal');
      expect(failures[0].reason).toBe('Must be a whole number (no decimals).');
    });

    it('rejects negative integers', () => {
      const failures = validateRecord({ ...VALID_ADDRESS, BedroomsTotal: -1 }, fields);
      expect(failures).toHaveLength(1);
      expect(failures[0].field).toBe('BedroomsTotal');
      expect(failures[0].reason).toContain('greater than or equal to 0');
    });
  });

  describe('boolean validation', () => {
    it('accepts true', () => {
      expect(validateRecord({ ...VALID_ADDRESS, ActiveYN: true }, fields)).toHaveLength(0);
    });

    it('accepts false', () => {
      expect(validateRecord({ ...VALID_ADDRESS, ActiveYN: false }, fields)).toHaveLength(0);
    });

    it('rejects non-boolean values', () => {
      const failures = validateRecord({ ...VALID_ADDRESS, ActiveYN: 'yes' }, fields);
      expect(failures).toHaveLength(1);
      expect(failures[0].field).toBe('ActiveYN');
      expect(failures[0].reason).toBe('Must be true or false.');
    });
  });

  describe('date validation', () => {
    it('accepts valid date strings', () => {
      expect(validateRecord({ ...VALID_ADDRESS, CloseDate: '2024-01-15' }, fields)).toHaveLength(0);
    });

    it('rejects non-string values for Edm.Date', () => {
      const failures = validateRecord({ ...VALID_ADDRESS, CloseDate: 20240115 }, fields);
      expect(failures).toHaveLength(1);
      expect(failures[0].field).toBe('CloseDate');
      expect(failures[0].reason).toBe('Must be a date (YYYY-MM-DD).');
    });
  });

  describe('datetime validation', () => {
    it('accepts valid datetime strings', () => {
      expect(validateRecord({ ...VALID_ADDRESS, ModificationTimestamp: '2024-01-15T10:30:00Z' }, fields)).toHaveLength(0);
    });

    it('rejects non-string values for Edm.DateTimeOffset', () => {
      const failures = validateRecord({ ...VALID_ADDRESS, ModificationTimestamp: 1705312200 }, fields);
      expect(failures).toHaveLength(1);
      expect(failures[0].field).toBe('ModificationTimestamp');
      expect(failures[0].reason).toBe('Must be a date and time.');
    });
  });

  describe('enum validation', () => {
    it('accepts string enum values', () => {
      expect(validateRecord({ ...VALID_ADDRESS, StandardStatus: 'Active' }, fields)).toHaveLength(0);
    });

    it('accepts numeric enum values', () => {
      expect(validateRecord({ ...VALID_ADDRESS, StandardStatus: 1 }, fields)).toHaveLength(0);
    });

    it('rejects non-string/number values for enum fields', () => {
      const failures = validateRecord({ ...VALID_ADDRESS, StandardStatus: true }, fields);
      expect(failures).toHaveLength(1);
      expect(failures[0].field).toBe('StandardStatus');
      expect(failures[0].reason).toBe('Please select a valid option from the list.');
    });
  });

  describe('collection validation', () => {
    it('accepts arrays for collection fields', () => {
      expect(validateRecord({ ...VALID_ADDRESS, AccessibilityFeatures: ['Wheelchair', 'Ramp'] }, fields)).toHaveLength(0);
    });

    it('accepts empty arrays', () => {
      expect(validateRecord({ ...VALID_ADDRESS, AccessibilityFeatures: [] }, fields)).toHaveLength(0);
    });

    it('rejects non-array values for collection fields', () => {
      const failures = validateRecord({ ...VALID_ADDRESS, AccessibilityFeatures: 'not-an-array' }, fields);
      expect(failures).toHaveLength(1);
      expect(failures[0].field).toBe('AccessibilityFeatures');
      expect(failures[0].reason).toBe('Must be a list of values.');
    });
  });

  describe('multiple failures', () => {
    it('reports all failures at once', () => {
      const failures = validateRecord(
        {
          ...VALID_ADDRESS,
          UnknownField: 'value',
          BedroomsTotal: 'three',
          ActiveYN: 'yes'
        },
        fields
      );
      expect(failures).toHaveLength(3);
      const fieldNames = failures.map(f => f.field);
      expect(fieldNames).toContain('UnknownField');
      expect(fieldNames).toContain('BedroomsTotal');
      expect(fieldNames).toContain('ActiveYN');
    });
  });

  describe('valid complex payload', () => {
    it('passes with all field types populated correctly', () => {
      const body = {
        ...VALID_ADDRESS,
        ListingKey: 'abc-123',
        ListPrice: 350000.0,
        BedroomsTotal: 4,
        LivingAreaTotal: 2800,
        StandardStatus: 'Active',
        AccessibilityFeatures: ['Wheelchair'],
        ActiveYN: true,
        CloseDate: '2024-06-15',
        ModificationTimestamp: '2024-06-15T08:00:00Z',
        LotSizeAcres: 0.25,
        Remarks: 'Great property'
      };
      expect(validateRecord(body, fields)).toHaveLength(0);
    });
  });
});

describe('helpers', () => {
  describe('isEnumType', () => {
    it('returns false for Edm types', () => {
      expect(isEnumType('Edm.String')).toBe(false);
      expect(isEnumType('Edm.Int32')).toBe(false);
      expect(isEnumType('Edm.Boolean')).toBe(false);
    });

    it('returns true for enum types', () => {
      expect(isEnumType('org.reso.metadata.enums.StandardStatus')).toBe(true);
      expect(isEnumType('org.reso.metadata.enums.PropertyType')).toBe(true);
    });
  });

  describe('isNumericEdmType', () => {
    it('returns true for numeric Edm types', () => {
      expect(isNumericEdmType('Edm.Decimal')).toBe(true);
      expect(isNumericEdmType('Edm.Int32')).toBe(true);
      expect(isNumericEdmType('Edm.Int64')).toBe(true);
      expect(isNumericEdmType('Edm.Double')).toBe(true);
      expect(isNumericEdmType('Edm.Single')).toBe(true);
      expect(isNumericEdmType('Edm.Byte')).toBe(true);
    });

    it('returns false for non-numeric Edm types', () => {
      expect(isNumericEdmType('Edm.String')).toBe(false);
      expect(isNumericEdmType('Edm.Boolean')).toBe(false);
      expect(isNumericEdmType('Edm.Date')).toBe(false);
    });
  });

  describe('isIntegerEdmType', () => {
    it('returns true for integer types', () => {
      expect(isIntegerEdmType('Edm.Int32')).toBe(true);
      expect(isIntegerEdmType('Edm.Int64')).toBe(true);
      expect(isIntegerEdmType('Edm.Int16')).toBe(true);
      expect(isIntegerEdmType('Edm.Byte')).toBe(true);
    });

    it('returns false for non-integer numeric types', () => {
      expect(isIntegerEdmType('Edm.Decimal')).toBe(false);
      expect(isIntegerEdmType('Edm.Double')).toBe(false);
      expect(isIntegerEdmType('Edm.Single')).toBe(false);
    });
  });
});
