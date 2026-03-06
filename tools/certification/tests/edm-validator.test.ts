import { describe, expect, it } from 'vitest';
import { validateRecordAgainstMetadata, validateValueAgainstEdm } from '../src/test-runner/edm-validator.js';
import type { EntityType } from '../src/test-runner/types.js';

describe('validateValueAgainstEdm', () => {
  // ── String ──
  it('passes for Edm.String with string value', () => {
    const result = validateValueAgainstEdm('City', 'Denver', 'Edm.String', true);
    expect(result.status).toBe('pass');
  });

  it('fails for Edm.String with number value', () => {
    const result = validateValueAgainstEdm('City', 42, 'Edm.String', true);
    expect(result.status).toBe('fail');
  });

  // ── Boolean ──
  it('passes for Edm.Boolean with boolean value', () => {
    const result = validateValueAgainstEdm('Active', true, 'Edm.Boolean', true);
    expect(result.status).toBe('pass');
  });

  it('fails for Edm.Boolean with string value', () => {
    const result = validateValueAgainstEdm('Active', 'true', 'Edm.Boolean', true);
    expect(result.status).toBe('fail');
  });

  // ── Integers ──
  it('passes for Edm.Int32 with integer value', () => {
    const result = validateValueAgainstEdm('BedroomsTotal', 3, 'Edm.Int32', true);
    expect(result.status).toBe('pass');
  });

  it('fails for Edm.Int32 with float value', () => {
    const result = validateValueAgainstEdm('BedroomsTotal', 3.5, 'Edm.Int32', true);
    expect(result.status).toBe('fail');
  });

  it('passes for Edm.Int64 with number value', () => {
    const result = validateValueAgainstEdm('Sequence', 12345, 'Edm.Int64', false);
    expect(result.status).toBe('pass');
  });

  it('passes for Edm.Int64 with string value (large numbers)', () => {
    const result = validateValueAgainstEdm('Sequence', '9007199254740993', 'Edm.Int64', false);
    expect(result.status).toBe('pass');
  });

  // ── Decimals ──
  it('passes for Edm.Decimal with number value', () => {
    const result = validateValueAgainstEdm('ListPrice', 499999.99, 'Edm.Decimal', true);
    expect(result.status).toBe('pass');
  });

  it('fails for Edm.Decimal with string value', () => {
    const result = validateValueAgainstEdm('ListPrice', '499999.99', 'Edm.Decimal', true);
    expect(result.status).toBe('fail');
  });

  // ── DateTimeOffset ──
  it('passes for Edm.DateTimeOffset with ISO datetime string', () => {
    const result = validateValueAgainstEdm('ModificationTimestamp', '2025-01-15T10:30:00Z', 'Edm.DateTimeOffset', true);
    expect(result.status).toBe('pass');
  });

  it('passes for Edm.DateTimeOffset with fractional seconds', () => {
    const result = validateValueAgainstEdm('ModificationTimestamp', '2025-01-15T10:30:00.123Z', 'Edm.DateTimeOffset', true);
    expect(result.status).toBe('pass');
  });

  it('fails for Edm.DateTimeOffset with date-only string', () => {
    const result = validateValueAgainstEdm('ModificationTimestamp', '2025-01-15', 'Edm.DateTimeOffset', true);
    expect(result.status).toBe('fail');
  });

  // ── Date ──
  it('passes for Edm.Date with YYYY-MM-DD string', () => {
    const result = validateValueAgainstEdm('ListingContractDate', '2025-01-15', 'Edm.Date', true);
    expect(result.status).toBe('pass');
  });

  it('fails for Edm.Date with datetime string', () => {
    const result = validateValueAgainstEdm('ListingContractDate', '2025-01-15T10:30:00Z', 'Edm.Date', true);
    expect(result.status).toBe('fail');
  });

  // ── Guid ──
  it('passes for Edm.Guid with valid GUID', () => {
    const result = validateValueAgainstEdm('Id', '550e8400-e29b-41d4-a716-446655440000', 'Edm.Guid', false);
    expect(result.status).toBe('pass');
  });

  it('fails for Edm.Guid with invalid string', () => {
    const result = validateValueAgainstEdm('Id', 'not-a-guid', 'Edm.Guid', false);
    expect(result.status).toBe('fail');
  });

  // ── Nullable ──
  it('passes for null when nullable is true', () => {
    const result = validateValueAgainstEdm('City', null, 'Edm.String', true);
    expect(result.status).toBe('pass');
  });

  it('fails for null when nullable is false', () => {
    const result = validateValueAgainstEdm('ListingKey', null, 'Edm.String', false);
    expect(result.status).toBe('fail');
  });

  it('passes for undefined when nullable is true', () => {
    const result = validateValueAgainstEdm('City', undefined, 'Edm.String', true);
    expect(result.status).toBe('pass');
  });

  // ── Collection ──
  it('passes for Collection(Edm.String) with string array', () => {
    const result = validateValueAgainstEdm('Features', ['Pool', 'Garage'], 'Collection(Edm.String)', true);
    expect(result.status).toBe('pass');
  });

  it('fails for Collection(Edm.String) with non-array', () => {
    const result = validateValueAgainstEdm('Features', 'Pool', 'Collection(Edm.String)', true);
    expect(result.status).toBe('fail');
  });

  it('fails for Collection(Edm.String) with mixed array', () => {
    const result = validateValueAgainstEdm('Features', ['Pool', 42], 'Collection(Edm.String)', true);
    expect(result.status).toBe('fail');
  });

  it('passes for empty collection', () => {
    const result = validateValueAgainstEdm('Features', [], 'Collection(Edm.String)', true);
    expect(result.status).toBe('pass');
  });

  // ── Unknown types (enum types) ──
  it('skips validation for non-Edm types (enum types)', () => {
    const result = validateValueAgainstEdm('StandardStatus', 'Active', 'org.reso.metadata.enums.StandardStatus', true);
    expect(result.status).toBe('skip');
  });
});

describe('validateRecordAgainstMetadata', () => {
  const entityType: EntityType = {
    name: 'Property',
    keyProperties: ['ListingKey'],
    properties: [
      { name: 'ListingKey', type: 'Edm.String', nullable: false },
      { name: 'ListPrice', type: 'Edm.Decimal' },
      { name: 'City', type: 'Edm.String' },
      { name: 'BedroomsTotal', type: 'Edm.Int32' }
    ]
  };

  it('passes for a record with valid fields', () => {
    const record = { ListingKey: 'abc-123', ListPrice: 500000, City: 'Denver', BedroomsTotal: 3 };
    const results = validateRecordAgainstMetadata(record, entityType, false);
    expect(results.every(r => r.status === 'pass')).toBe(true);
  });

  it('warns for unknown fields in non-strict mode', () => {
    const record = { ListingKey: 'abc-123', UnknownField: 'value' };
    const results = validateRecordAgainstMetadata(record, entityType, false);
    const unknownResult = results.find(r => r.description.includes('UnknownField'));
    expect(unknownResult?.status).toBe('warn');
  });

  it('fails for unknown fields in strict mode', () => {
    const record = { ListingKey: 'abc-123', UnknownField: 'value' };
    const results = validateRecordAgainstMetadata(record, entityType, true);
    const unknownResult = results.find(r => r.description.includes('UnknownField'));
    expect(unknownResult?.status).toBe('fail');
  });

  it('skips OData annotation fields', () => {
    const record = { ListingKey: 'abc-123', '@odata.context': 'http://example.com/$metadata' };
    const results = validateRecordAgainstMetadata(record, entityType, true);
    expect(results.length).toBe(1); // Only ListingKey
    expect(results[0].status).toBe('pass');
  });

  it('fails for type mismatch', () => {
    const record = { ListingKey: 'abc-123', BedroomsTotal: 'three' };
    const results = validateRecordAgainstMetadata(record, entityType, false);
    const bedroomResult = results.find(r => r.description.includes('BedroomsTotal'));
    expect(bedroomResult?.status).toBe('fail');
  });
});
