import { describe, expect, it } from 'vitest';
import type { ResoField } from '../src/metadata/types.js';
import { validateRequestBody } from '../src/odata/validation.js';

const makeField = (overrides: Partial<ResoField> & Pick<ResoField, 'fieldName' | 'type'>): ResoField => ({
  resourceName: 'Property',
  nullable: true,
  annotations: [],
  ...overrides
});

const fields: ResoField[] = [
  makeField({ fieldName: 'ListingKey', type: 'Edm.String', maxLength: 255 }),
  makeField({ fieldName: 'ListPrice', type: 'Edm.Decimal', precision: 14, scale: 2 }),
  makeField({ fieldName: 'BedroomsTotal', type: 'Edm.Int64' }),
  makeField({ fieldName: 'City', type: 'Edm.String' }),
  makeField({ fieldName: 'StateOrProvince', type: 'Edm.String' }),
  makeField({ fieldName: 'PostalCode', type: 'Edm.String' }),
  makeField({ fieldName: 'Country', type: 'Edm.String' }),
  makeField({ fieldName: 'StandardStatus', type: 'org.reso.metadata.enums.StandardStatus' }),
  makeField({
    fieldName: 'AccessibilityFeatures',
    type: 'org.reso.metadata.enums.AccessibilityFeatures',
    isCollection: true
  }),
  makeField({ fieldName: 'ActiveYN', type: 'Edm.Boolean' })
];

/** Baseline address fields to satisfy required-field business rules. */
const REQUIRED_ADDRESS = {
  City: 'Austin',
  StateOrProvince: 'TX',
  PostalCode: '78701',
  Country: 'US'
};

describe('validateRequestBody', () => {
  it('returns empty array for a valid payload', () => {
    const body = {
      ListPrice: 250000,
      BedroomsTotal: 3,
      ...REQUIRED_ADDRESS
    };
    const failures = validateRequestBody(body, fields);
    expect(failures).toHaveLength(0);
  });

  it('detects unknown fields', () => {
    const body = { UnknownField: 'value', ...REQUIRED_ADDRESS };
    const failures = validateRequestBody(body, fields);
    expect(failures).toHaveLength(1);
    expect(failures[0].field).toBe('UnknownField');
    expect(failures[0].reason).toContain('not a recognized field');
  });

  it('detects negative numeric values', () => {
    const body = { ListPrice: -100, ...REQUIRED_ADDRESS };
    const failures = validateRequestBody(body, fields);
    expect(failures).toHaveLength(1);
    expect(failures[0].field).toBe('ListPrice');
    expect(failures[0].reason).toContain('greater than or equal to 0');
  });

  it('skips OData annotations', () => {
    const body = { '@odata.context': 'something', ListPrice: 100, ...REQUIRED_ADDRESS };
    const failures = validateRequestBody(body, fields);
    expect(failures).toHaveLength(0);
  });

  it('allows null values for nullable fields', () => {
    const body = { ...REQUIRED_ADDRESS, BedroomsTotal: null };
    const failures = validateRequestBody(body, fields);
    expect(failures).toHaveLength(0);
  });

  it('validates collection fields must be arrays', () => {
    const body = { AccessibilityFeatures: 'not-an-array', ...REQUIRED_ADDRESS };
    const failures = validateRequestBody(body, fields);
    expect(failures).toHaveLength(1);
    expect(failures[0].field).toBe('AccessibilityFeatures');
  });

  it('validates string fields', () => {
    const body = { City: 12345, StateOrProvince: 'TX', PostalCode: '78701', Country: 'US' };
    const failures = validateRequestBody(body, fields);
    expect(failures).toHaveLength(1);
    expect(failures[0].field).toBe('City');
  });

  it('validates boolean fields', () => {
    const body = { ActiveYN: 'yes', ...REQUIRED_ADDRESS };
    const failures = validateRequestBody(body, fields);
    expect(failures).toHaveLength(1);
    expect(failures[0].field).toBe('ActiveYN');
  });

  it('accepts enum fields as strings', () => {
    const body = { StandardStatus: 'Active', ...REQUIRED_ADDRESS };
    const failures = validateRequestBody(body, fields);
    expect(failures).toHaveLength(0);
  });

  it('accepts collection fields as arrays', () => {
    const body = { AccessibilityFeatures: ['Wheelchair', 'Ramp'], ...REQUIRED_ADDRESS };
    const failures = validateRequestBody(body, fields);
    expect(failures).toHaveLength(0);
  });
});
