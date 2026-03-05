import { describe, expect, it } from 'vitest';
import { filterToSqlite } from '../src/db/filter-to-sqlite.js';
import type { ResoField } from '../src/metadata/types.js';

/** Minimal field definition factory. */
const field = (fieldName: string, type = 'Edm.String', isCollection = false): ResoField => ({
  resourceName: 'Property',
  fieldName,
  type,
  isCollection,
  annotations: []
});

const fields: ReadonlyArray<ResoField> = [
  field('ListPrice', 'Edm.Decimal'),
  field('City'),
  field('StateOrProvince'),
  field('BedroomsTotal', 'Edm.Int32'),
  field('ListingKey'),
  field('ModificationTimestamp', 'Edm.DateTimeOffset'),
  field('StandardStatus'),
  field('PostalCode'),
  field('AccessibilityFeatures', 'Collection(Edm.String)', true)
];

describe('filterToSqlite', () => {
  describe('comparison operators', () => {
    it('translates eq', () => {
      const result = filterToSqlite("City eq 'Austin'", fields, 'p');
      expect(result.where).toBe(`p."City" = ?`);
      expect(result.values).toEqual(['Austin']);
    });

    it('translates ne (excludes null to match SQL semantics)', () => {
      const result = filterToSqlite("City ne 'Dallas'", fields, 'p');
      expect(result.where).toBe(`p."City" != ?`);
      expect(result.values).toEqual(['Dallas']);
    });

    it('translates gt', () => {
      const result = filterToSqlite('ListPrice gt 200000', fields, 'p');
      expect(result.where).toBe(`p."ListPrice" > ?`);
      expect(result.values).toEqual([200000]);
    });

    it('translates ge', () => {
      const result = filterToSqlite('ListPrice ge 200000', fields, 'p');
      expect(result.where).toBe(`p."ListPrice" >= ?`);
      expect(result.values).toEqual([200000]);
    });

    it('translates lt', () => {
      const result = filterToSqlite('ListPrice lt 500000', fields, 'p');
      expect(result.where).toBe(`p."ListPrice" < ?`);
      expect(result.values).toEqual([500000]);
    });

    it('translates le', () => {
      const result = filterToSqlite('ListPrice le 500000', fields, 'p');
      expect(result.where).toBe(`p."ListPrice" <= ?`);
      expect(result.values).toEqual([500000]);
    });
  });

  describe('null comparisons', () => {
    it('translates eq null to IS NULL', () => {
      const result = filterToSqlite('City eq null', fields, 'p');
      expect(result.where).toBe(`p."City" IS NULL`);
      expect(result.values).toEqual([]);
    });

    it('translates ne null to IS NOT NULL', () => {
      const result = filterToSqlite('City ne null', fields, 'p');
      expect(result.where).toBe(`p."City" IS NOT NULL`);
      expect(result.values).toEqual([]);
    });
  });

  describe('logical operators', () => {
    it('translates and', () => {
      const result = filterToSqlite("ListPrice gt 200000 and City eq 'Austin'", fields, 'p');
      expect(result.where).toBe(`(p."ListPrice" > ? AND p."City" = ?)`);
      expect(result.values).toEqual([200000, 'Austin']);
    });

    it('translates or', () => {
      const result = filterToSqlite("City eq 'Austin' or City eq 'Dallas'", fields, 'p');
      expect(result.where).toBe(`(p."City" = ? OR p."City" = ?)`);
      expect(result.values).toEqual(['Austin', 'Dallas']);
    });

    it('translates not', () => {
      const result = filterToSqlite("not City eq 'Austin'", fields, 'p');
      expect(result.where).toBe(`NOT (p."City" = ?)`);
      expect(result.values).toEqual(['Austin']);
    });
  });

  describe('string functions', () => {
    it('translates contains using LIKE', () => {
      const result = filterToSqlite("contains(City, 'Aus')", fields, 'p');
      expect(result.where).toBe(`p."City" LIKE ?`);
      expect(result.values).toEqual(['%Aus%']);
    });

    it('translates startswith using LIKE', () => {
      const result = filterToSqlite("startswith(City, 'A')", fields, 'p');
      expect(result.where).toBe(`p."City" LIKE ?`);
      expect(result.values).toEqual(['A%']);
    });

    it('translates endswith using LIKE', () => {
      const result = filterToSqlite("endswith(City, 'tin')", fields, 'p');
      expect(result.where).toBe(`p."City" LIKE ?`);
      expect(result.values).toEqual(['%tin']);
    });

    it('translates tolower', () => {
      const result = filterToSqlite("tolower(City) eq 'austin'", fields, 'p');
      expect(result.where).toBe(`LOWER(p."City") = ?`);
      expect(result.values).toEqual(['austin']);
    });

    it('translates toupper', () => {
      const result = filterToSqlite("toupper(City) eq 'AUSTIN'", fields, 'p');
      expect(result.where).toBe(`UPPER(p."City") = ?`);
      expect(result.values).toEqual(['AUSTIN']);
    });

    it('translates length', () => {
      const result = filterToSqlite('length(City) gt 5', fields, 'p');
      expect(result.where).toBe(`LENGTH(p."City") > ?`);
      expect(result.values).toEqual([5]);
    });

    it('translates trim', () => {
      const result = filterToSqlite("trim(City) eq 'Austin'", fields, 'p');
      expect(result.where).toBe(`TRIM(p."City") = ?`);
      expect(result.values).toEqual(['Austin']);
    });

    it('translates concat', () => {
      const result = filterToSqlite("concat(City, StateOrProvince) eq 'AustinTX'", fields, 'p');
      expect(result.where).toBe(`(p."City" || p."StateOrProvince") = ?`);
      expect(result.values).toEqual(['AustinTX']);
    });
  });

  describe('date functions', () => {
    it('translates year using strftime', () => {
      const result = filterToSqlite('year(ModificationTimestamp) eq 2024', fields, 'p');
      expect(result.where).toBe(`CAST(strftime('%Y', p."ModificationTimestamp") AS INTEGER) = ?`);
      expect(result.values).toEqual([2024]);
    });

    it('translates month using strftime', () => {
      const result = filterToSqlite('month(ModificationTimestamp) eq 6', fields, 'p');
      expect(result.where).toBe(`CAST(strftime('%m', p."ModificationTimestamp") AS INTEGER) = ?`);
      expect(result.values).toEqual([6]);
    });
  });

  describe('math functions', () => {
    it('translates round', () => {
      const result = filterToSqlite('round(ListPrice) eq 200000', fields, 'p');
      expect(result.where).toBe(`ROUND(p."ListPrice") = ?`);
      expect(result.values).toEqual([200000]);
    });

    it('translates floor', () => {
      const result = filterToSqlite('floor(ListPrice) gt 100000', fields, 'p');
      expect(result.where).toBe(`floor(p."ListPrice") > ?`);
      expect(result.values).toEqual([100000]);
    });

    it('translates ceiling', () => {
      const result = filterToSqlite('ceiling(ListPrice) lt 500000', fields, 'p');
      expect(result.where).toBe(`ceil(p."ListPrice") < ?`);
      expect(result.values).toEqual([500000]);
    });
  });

  describe('arithmetic operators', () => {
    it('translates add', () => {
      const result = filterToSqlite('ListPrice add 1000 gt 300000', fields, 'p');
      expect(result.where).toBe(`(p."ListPrice" + ?) > ?`);
      expect(result.values).toEqual([1000, 300000]);
    });

    it('translates sub', () => {
      const result = filterToSqlite('ListPrice sub 1000 lt 200000', fields, 'p');
      expect(result.where).toBe(`(p."ListPrice" - ?) < ?`);
      expect(result.values).toEqual([1000, 200000]);
    });
  });

  describe('complex expressions', () => {
    it('handles nested logical operators', () => {
      const result = filterToSqlite("ListPrice gt 200000 and (City eq 'Austin' or City eq 'Dallas')", fields, 'p');
      expect(result.where).toBe(`(p."ListPrice" > ? AND (p."City" = ? OR p."City" = ?))`);
      expect(result.values).toEqual([200000, 'Austin', 'Dallas']);
    });

    it('handles function in comparison', () => {
      const result = filterToSqlite("contains(City, 'Aus') and ListPrice gt 100000", fields, 'p');
      expect(result.where).toBe(`(p."City" LIKE ? AND p."ListPrice" > ?)`);
      expect(result.values).toEqual(['%Aus%', 100000]);
    });
  });

  describe('lambda expressions', () => {
    it('translates any() with equality using json_each', () => {
      const result = filterToSqlite("AccessibilityFeatures/any(v:v eq 'Elevator')", fields, 'p');
      expect(result.where).toBe(`EXISTS (SELECT 1 FROM json_each(p."AccessibilityFeatures") WHERE value = ?)`);
      expect(result.values).toEqual(['Elevator']);
    });

    it('translates all() with equality using NOT EXISTS', () => {
      const result = filterToSqlite("AccessibilityFeatures/all(v:v eq 'Elevator')", fields, 'p');
      expect(result.where).toBe(
        `(json_array_length(p."AccessibilityFeatures") > 0 AND NOT EXISTS (SELECT 1 FROM json_each(p."AccessibilityFeatures") WHERE value != ?))`
      );
      expect(result.values).toEqual(['Elevator']);
    });

    it('translates empty any() as non-empty check', () => {
      const result = filterToSqlite('AccessibilityFeatures/any()', fields, 'p');
      expect(result.where).toBe(`json_array_length(p."AccessibilityFeatures") > 0`);
      expect(result.values).toEqual([]);
    });
  });

  describe('error handling', () => {
    it('throws on unknown field', () => {
      expect(() => filterToSqlite("UnknownField eq 'test'", fields, 'p')).toThrow('Unknown field in $filter: UnknownField');
    });
  });
});
