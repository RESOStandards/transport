import { describe, expect, it } from 'vitest';
import { getBusinessRules, getCrossFieldRules, validateBusinessRules } from '../src/business-rules/index.js';
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

/** Base record satisfying all required address fields for Member. */
const VALID_MEMBER_ADDRESS = { MemberCity: 'Austin', MemberStateOrProvince: 'TX', MemberPostalCode: '78701', MemberCountry: 'US' };

/** Base record satisfying all required address fields for Office. */
const VALID_OFFICE_ADDRESS = { OfficeCity: 'Austin', OfficeStateOrProvince: 'TX', OfficePostalCode: '78701', OfficeCountry: 'US' };

const PROPERTY_FIELDS: ReadonlyArray<ResoField> = [
  makeField({ fieldName: 'ListPrice', type: 'Edm.Decimal', precision: 14, scale: 2 }),
  makeField({ fieldName: 'ListPriceLow', type: 'Edm.Decimal', precision: 14, scale: 2 }),
  makeField({ fieldName: 'OriginalListPrice', type: 'Edm.Decimal', precision: 14, scale: 2 }),
  makeField({ fieldName: 'ClosePrice', type: 'Edm.Decimal', precision: 14, scale: 2 }),
  makeField({ fieldName: 'BedroomsTotal', type: 'Edm.Int32' }),
  makeField({ fieldName: 'BedroomsPossible', type: 'Edm.Int32' }),
  makeField({ fieldName: 'BathroomsTotalInteger', type: 'Edm.Int32' }),
  makeField({ fieldName: 'BathroomsFull', type: 'Edm.Int32' }),
  makeField({ fieldName: 'BathroomsHalf', type: 'Edm.Int32' }),
  makeField({ fieldName: 'BathroomsPartial', type: 'Edm.Int32' }),
  makeField({ fieldName: 'BathroomsOneQuarter', type: 'Edm.Int32' }),
  makeField({ fieldName: 'BathroomsThreeQuarter', type: 'Edm.Int32' }),
  makeField({ fieldName: 'MainLevelBathrooms', type: 'Edm.Int32' }),
  makeField({ fieldName: 'City', type: 'Edm.String', maxLength: 50 }),
  makeField({ fieldName: 'StateOrProvince', type: 'Edm.String', maxLength: 2 }),
  makeField({ fieldName: 'PostalCode', type: 'Edm.String', maxLength: 10 }),
  makeField({ fieldName: 'Country', type: 'Edm.String', maxLength: 2 })
];

describe('getBusinessRules', () => {
  it('returns rules for Property', () => {
    const rules = getBusinessRules('Property');
    expect(rules.length).toBeGreaterThan(0);
    expect(rules.some(r => r.fieldName === 'ListPrice')).toBe(true);
    expect(rules.some(r => r.fieldName === 'BedroomsTotal')).toBe(true);
    expect(rules.some(r => r.fieldName === 'BathroomsFull')).toBe(true);
  });

  it('returns empty array for unknown resource', () => {
    expect(getBusinessRules('UnknownResource')).toHaveLength(0);
  });

  it('returns rules for Member', () => {
    const rules = getBusinessRules('Member');
    expect(rules.length).toBeGreaterThan(0);
    expect(rules.some(r => r.fieldName === 'MemberCity' && r.required)).toBe(true);
  });

  it('returns rules for Office', () => {
    const rules = getBusinessRules('Office');
    expect(rules.length).toBeGreaterThan(0);
    expect(rules.some(r => r.fieldName === 'OfficeCity' && r.required)).toBe(true);
  });
});

describe('validateBusinessRules', () => {
  describe('ListPrice', () => {
    it('accepts 0', () => {
      expect(validateBusinessRules('Property', { ...VALID_ADDRESS, ListPrice: 0 })).toHaveLength(0);
    });

    it('accepts a normal price', () => {
      expect(validateBusinessRules('Property', { ...VALID_ADDRESS, ListPrice: 500000 })).toHaveLength(0);
    });

    it('accepts 999,999,999', () => {
      expect(validateBusinessRules('Property', { ...VALID_ADDRESS, ListPrice: 999_999_999 })).toHaveLength(0);
    });

    it('accepts exactly 1,000,000,000', () => {
      expect(validateBusinessRules('Property', { ...VALID_ADDRESS, ListPrice: 1_000_000_000 })).toHaveLength(0);
    });

    it('rejects values above 1,000,000,000', () => {
      const failures = validateBusinessRules('Property', { ...VALID_ADDRESS, ListPrice: 1_000_000_001 });
      expect(failures).toHaveLength(1);
      expect(failures[0].field).toBe('ListPrice');
      expect(failures[0].reason).toContain('at most');
    });

    it('rejects negative values', () => {
      const failures = validateBusinessRules('Property', { ...VALID_ADDRESS, ListPrice: -1 });
      expect(failures).toHaveLength(1);
      expect(failures[0].field).toBe('ListPrice');
      expect(failures[0].reason).toContain('at least');
    });
  });

  describe('bedroom fields', () => {
    it('accepts 0 bedrooms', () => {
      expect(validateBusinessRules('Property', { ...VALID_ADDRESS, BedroomsTotal: 0 })).toHaveLength(0);
    });

    it('accepts 50 bedrooms', () => {
      expect(validateBusinessRules('Property', { ...VALID_ADDRESS, BedroomsTotal: 50 })).toHaveLength(0);
    });

    it('accepts exactly 100 bedrooms', () => {
      expect(validateBusinessRules('Property', { ...VALID_ADDRESS, BedroomsTotal: 100 })).toHaveLength(0);
    });

    it('rejects 101 bedrooms', () => {
      const failures = validateBusinessRules('Property', { ...VALID_ADDRESS, BedroomsTotal: 101 });
      expect(failures).toHaveLength(1);
      expect(failures[0].field).toBe('BedroomsTotal');
    });

    it('rejects negative bedrooms', () => {
      const failures = validateBusinessRules('Property', { ...VALID_ADDRESS, BedroomsPossible: -1 });
      expect(failures).toHaveLength(1);
      expect(failures[0].field).toBe('BedroomsPossible');
    });
  });

  describe('bathroom fields', () => {
    it('accepts 0 bathrooms', () => {
      expect(validateBusinessRules('Property', { ...VALID_ADDRESS, BathroomsFull: 0 })).toHaveLength(0);
    });

    it('accepts exactly 100 bathrooms', () => {
      expect(validateBusinessRules('Property', { ...VALID_ADDRESS, BathroomsTotalInteger: 100 })).toHaveLength(0);
    });

    it('rejects 101 bathrooms', () => {
      const failures = validateBusinessRules('Property', { ...VALID_ADDRESS, BathroomsTotalInteger: 101 });
      expect(failures).toHaveLength(1);
      expect(failures[0].field).toBe('BathroomsTotalInteger');
    });

    it('validates MainLevelBathrooms', () => {
      const failures = validateBusinessRules('Property', { ...VALID_ADDRESS, MainLevelBathrooms: 200 });
      expect(failures).toHaveLength(1);
    });
  });

  describe('non-rule fields', () => {
    it('ignores fields without range rules', () => {
      expect(validateBusinessRules('Property', { ...VALID_ADDRESS, SomeOtherField: 'hello' })).toHaveLength(0);
    });

    it('ignores non-numeric values for range rules', () => {
      expect(validateBusinessRules('Property', { ...VALID_ADDRESS, ListPrice: 'not-a-number' })).toHaveLength(0);
    });
  });

  describe('non-Property resources', () => {
    it('returns no failures for Member with valid address', () => {
      expect(validateBusinessRules('Member', { ...VALID_MEMBER_ADDRESS, ListPrice: 999_999_999_999 })).toHaveLength(0);
    });
  });
});

describe('getCrossFieldRules', () => {
  it('returns cross-field rules for Property', () => {
    const rules = getCrossFieldRules('Property');
    expect(rules.length).toBeGreaterThan(0);
    expect(rules.some(r => r.name.includes('ListPrice'))).toBe(true);
    expect(rules.some(r => r.name.includes('Bathroom'))).toBe(true);
  });

  it('returns empty array for unknown resource', () => {
    expect(getCrossFieldRules('Member')).toHaveLength(0);
  });
});

describe('cross-field rules: ListPrice >= ListPriceLow', () => {
  it('passes when ListPrice > ListPriceLow', () => {
    const failures = validateBusinessRules('Property', { ...VALID_ADDRESS, ListPrice: 500000, ListPriceLow: 400000 });
    expect(failures.some(f => f.field === 'ListPrice' && f.reason.includes('ListPriceLow'))).toBe(false);
  });

  it('passes when ListPrice = ListPriceLow', () => {
    const failures = validateBusinessRules('Property', { ...VALID_ADDRESS, ListPrice: 500000, ListPriceLow: 500000 });
    expect(failures.some(f => f.field === 'ListPrice' && f.reason.includes('ListPriceLow'))).toBe(false);
  });

  it('fails when ListPrice < ListPriceLow', () => {
    const failures = validateBusinessRules('Property', { ...VALID_ADDRESS, ListPrice: 300000, ListPriceLow: 400000 });
    expect(failures.some(f => f.field === 'ListPrice' && f.reason.includes('ListPriceLow'))).toBe(true);
  });

  it('skips when ListPriceLow is absent', () => {
    const failures = validateBusinessRules('Property', { ...VALID_ADDRESS, ListPrice: 500000 });
    expect(failures.some(f => f.reason.includes('ListPriceLow'))).toBe(false);
  });

  it('skips when ListPrice is absent', () => {
    const failures = validateBusinessRules('Property', { ...VALID_ADDRESS, ListPriceLow: 400000 });
    expect(failures.some(f => f.reason.includes('ListPriceLow'))).toBe(false);
  });
});

describe('cross-field rules: BathroomsTotalInteger = sum of parts', () => {
  it('passes when total matches sum of all parts', () => {
    const failures = validateBusinessRules('Property', {
      ...VALID_ADDRESS,
      BathroomsTotalInteger: 8,
      BathroomsFull: 5,
      BathroomsHalf: 1,
      BathroomsPartial: 2,
      BathroomsOneQuarter: 0,
      BathroomsThreeQuarter: 0
    });
    expect(failures.some(f => f.field === 'BathroomsTotalInteger' && f.reason.includes('sum'))).toBe(false);
  });

  it('fails when total does not match sum of parts', () => {
    const failures = validateBusinessRules('Property', {
      ...VALID_ADDRESS,
      BathroomsTotalInteger: 5,
      BathroomsFull: 5,
      BathroomsHalf: 1,
      BathroomsPartial: 2
    });
    expect(failures.some(f => f.field === 'BathroomsTotalInteger' && f.reason.includes('sum'))).toBe(true);
  });

  it('sums only the parts that are present', () => {
    const failures = validateBusinessRules('Property', {
      ...VALID_ADDRESS,
      BathroomsTotalInteger: 3,
      BathroomsFull: 2,
      BathroomsHalf: 1
    });
    expect(failures.some(f => f.field === 'BathroomsTotalInteger' && f.reason.includes('sum'))).toBe(false);
  });

  it('skips when BathroomsTotalInteger is absent', () => {
    const failures = validateBusinessRules('Property', { ...VALID_ADDRESS, BathroomsFull: 5, BathroomsHalf: 1 });
    expect(failures.some(f => f.field === 'BathroomsTotalInteger')).toBe(false);
  });

  it('skips when no parts are present', () => {
    const failures = validateBusinessRules('Property', { ...VALID_ADDRESS, BathroomsTotalInteger: 5 });
    expect(failures.some(f => f.field === 'BathroomsTotalInteger' && f.reason.includes('sum'))).toBe(false);
  });
});

describe('required address field rules', () => {
  describe('Property', () => {
    it('passes when all required address fields are present', () => {
      const failures = validateBusinessRules('Property', VALID_ADDRESS);
      expect(failures).toHaveLength(0);
    });

    it('fails when City is missing', () => {
      const failures = validateBusinessRules('Property', { StateOrProvince: 'TX', PostalCode: '78701', Country: 'US' });
      expect(failures.some(f => f.field === 'City' && f.reason.includes('required'))).toBe(true);
    });

    it('fails when StateOrProvince is missing', () => {
      const failures = validateBusinessRules('Property', { City: 'Austin', PostalCode: '78701', Country: 'US' });
      expect(failures.some(f => f.field === 'StateOrProvince' && f.reason.includes('required'))).toBe(true);
    });

    it('fails when PostalCode is missing', () => {
      const failures = validateBusinessRules('Property', { City: 'Austin', StateOrProvince: 'TX', Country: 'US' });
      expect(failures.some(f => f.field === 'PostalCode' && f.reason.includes('required'))).toBe(true);
    });

    it('fails when Country is missing', () => {
      const failures = validateBusinessRules('Property', { City: 'Austin', StateOrProvince: 'TX', PostalCode: '78701' });
      expect(failures.some(f => f.field === 'Country' && f.reason.includes('required'))).toBe(true);
    });

    it('fails when required field is empty string', () => {
      const failures = validateBusinessRules('Property', { ...VALID_ADDRESS, City: '' });
      expect(failures.some(f => f.field === 'City' && f.reason.includes('required'))).toBe(true);
    });

    it('fails when required field is null', () => {
      const failures = validateBusinessRules('Property', { ...VALID_ADDRESS, City: null });
      expect(failures.some(f => f.field === 'City' && f.reason.includes('required'))).toBe(true);
    });

    it('reports all missing required fields at once', () => {
      const failures = validateBusinessRules('Property', {});
      const requiredFailures = failures.filter(f => f.reason.includes('required'));
      expect(requiredFailures).toHaveLength(4);
    });
  });

  describe('Member', () => {
    it('passes when all required address fields are present', () => {
      expect(validateBusinessRules('Member', VALID_MEMBER_ADDRESS)).toHaveLength(0);
    });

    it('fails when MemberCity is missing', () => {
      const { MemberCity: _, ...rest } = VALID_MEMBER_ADDRESS;
      const failures = validateBusinessRules('Member', rest);
      expect(failures.some(f => f.field === 'MemberCity' && f.reason.includes('required'))).toBe(true);
    });
  });

  describe('Office', () => {
    it('passes when all required address fields are present', () => {
      expect(validateBusinessRules('Office', VALID_OFFICE_ADDRESS)).toHaveLength(0);
    });

    it('fails when OfficeStateOrProvince is missing', () => {
      const { OfficeStateOrProvince: _, ...rest } = VALID_OFFICE_ADDRESS;
      const failures = validateBusinessRules('Office', rest);
      expect(failures.some(f => f.field === 'OfficeStateOrProvince' && f.reason.includes('required'))).toBe(true);
    });
  });
});

describe('validateRecord integration with business rules', () => {
  it('catches business rule violations during record validation', () => {
    const failures = validateRecord({ ...VALID_ADDRESS, ListPrice: 2_000_000_000 }, PROPERTY_FIELDS);
    expect(failures.some(f => f.field === 'ListPrice' && f.reason.includes('at most'))).toBe(true);
  });

  it('allows valid values through both type and business rule checks', () => {
    const failures = validateRecord({ ...VALID_ADDRESS, ListPrice: 500000, BedroomsTotal: 3, BathroomsFull: 2 }, PROPERTY_FIELDS);
    expect(failures).toHaveLength(0);
  });

  it('reports both type and business rule failures', () => {
    const failures = validateRecord({ ...VALID_ADDRESS, ListPrice: 2_000_000_000, BedroomsTotal: 101 }, PROPERTY_FIELDS);
    expect(failures.some(f => f.field === 'ListPrice')).toBe(true);
    expect(failures.some(f => f.field === 'BedroomsTotal')).toBe(true);
  });

  it('catches cross-field violations during record validation', () => {
    const failures = validateRecord({ ...VALID_ADDRESS, BathroomsTotalInteger: 3, BathroomsFull: 5, BathroomsHalf: 1 }, PROPERTY_FIELDS);
    expect(failures.some(f => f.field === 'BathroomsTotalInteger' && f.reason.includes('sum'))).toBe(true);
  });
});
