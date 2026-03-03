import { describe, expect, it } from 'vitest';
import { filterToSql } from '../src/db/filter-to-sql.js';
import type { ResoField } from '../src/metadata/types.js';

/** Minimal field definition factory. */
const field = (fieldName: string, type = 'Edm.String'): ResoField => ({
  resourceName: 'Property',
  fieldName,
  type,
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
  field('PostalCode')
];

describe('filterToSql', () => {
  describe('comparison operators', () => {
    it('translates eq', () => {
      const result = filterToSql("City eq 'Austin'", fields, 'p');
      expect(result.where).toBe(`p."City" = $1`);
      expect(result.values).toEqual(['Austin']);
    });

    it('translates ne', () => {
      const result = filterToSql("City ne 'Dallas'", fields, 'p');
      expect(result.where).toBe(`p."City" != $1`);
      expect(result.values).toEqual(['Dallas']);
    });

    it('translates gt', () => {
      const result = filterToSql('ListPrice gt 200000', fields, 'p');
      expect(result.where).toBe(`p."ListPrice" > $1`);
      expect(result.values).toEqual([200000]);
    });

    it('translates ge', () => {
      const result = filterToSql('ListPrice ge 200000', fields, 'p');
      expect(result.where).toBe(`p."ListPrice" >= $1`);
      expect(result.values).toEqual([200000]);
    });

    it('translates lt', () => {
      const result = filterToSql('ListPrice lt 500000', fields, 'p');
      expect(result.where).toBe(`p."ListPrice" < $1`);
      expect(result.values).toEqual([500000]);
    });

    it('translates le', () => {
      const result = filterToSql('ListPrice le 500000', fields, 'p');
      expect(result.where).toBe(`p."ListPrice" <= $1`);
      expect(result.values).toEqual([500000]);
    });
  });

  describe('null comparisons', () => {
    it('translates eq null to IS NULL', () => {
      const result = filterToSql('City eq null', fields, 'p');
      expect(result.where).toBe(`p."City" IS NULL`);
      expect(result.values).toEqual([]);
    });

    it('translates ne null to IS NOT NULL', () => {
      const result = filterToSql('City ne null', fields, 'p');
      expect(result.where).toBe(`p."City" IS NOT NULL`);
      expect(result.values).toEqual([]);
    });
  });

  describe('logical operators', () => {
    it('translates and', () => {
      const result = filterToSql("ListPrice gt 200000 and City eq 'Austin'", fields, 'p');
      expect(result.where).toBe(`(p."ListPrice" > $1 AND p."City" = $2)`);
      expect(result.values).toEqual([200000, 'Austin']);
    });

    it('translates or', () => {
      const result = filterToSql("City eq 'Austin' or City eq 'Dallas'", fields, 'p');
      expect(result.where).toBe(`(p."City" = $1 OR p."City" = $2)`);
      expect(result.values).toEqual(['Austin', 'Dallas']);
    });

    it('translates not', () => {
      const result = filterToSql("not City eq 'Austin'", fields, 'p');
      expect(result.where).toBe(`NOT (p."City" = $1)`);
      expect(result.values).toEqual(['Austin']);
    });
  });

  describe('string functions', () => {
    it('translates contains', () => {
      const result = filterToSql("contains(City, 'Aus')", fields, 'p');
      expect(result.where).toBe(`p."City" ILIKE $1`);
      expect(result.values).toEqual(['%Aus%']);
    });

    it('translates startswith', () => {
      const result = filterToSql("startswith(City, 'A')", fields, 'p');
      expect(result.where).toBe(`p."City" ILIKE $1`);
      expect(result.values).toEqual(['A%']);
    });

    it('translates endswith', () => {
      const result = filterToSql("endswith(City, 'tin')", fields, 'p');
      expect(result.where).toBe(`p."City" ILIKE $1`);
      expect(result.values).toEqual(['%tin']);
    });

    it('translates tolower', () => {
      const result = filterToSql("tolower(City) eq 'austin'", fields, 'p');
      expect(result.where).toBe(`LOWER(p."City") = $1`);
      expect(result.values).toEqual(['austin']);
    });

    it('translates toupper', () => {
      const result = filterToSql("toupper(City) eq 'AUSTIN'", fields, 'p');
      expect(result.where).toBe(`UPPER(p."City") = $1`);
      expect(result.values).toEqual(['AUSTIN']);
    });

    it('translates length', () => {
      const result = filterToSql('length(City) gt 5', fields, 'p');
      expect(result.where).toBe(`LENGTH(p."City") > $1`);
      expect(result.values).toEqual([5]);
    });

    it('translates trim', () => {
      const result = filterToSql("trim(City) eq 'Austin'", fields, 'p');
      expect(result.where).toBe(`TRIM(p."City") = $1`);
      expect(result.values).toEqual(['Austin']);
    });

    it('translates concat', () => {
      const result = filterToSql("concat(City, StateOrProvince) eq 'AustinTX'", fields, 'p');
      expect(result.where).toBe(`(p."City" || p."StateOrProvince") = $1`);
      expect(result.values).toEqual(['AustinTX']);
    });
  });

  describe('date functions', () => {
    it('translates year', () => {
      const result = filterToSql('year(ModificationTimestamp) eq 2024', fields, 'p');
      expect(result.where).toBe(`EXTRACT(YEAR FROM p."ModificationTimestamp") = $1`);
      expect(result.values).toEqual([2024]);
    });

    it('translates month', () => {
      const result = filterToSql('month(ModificationTimestamp) eq 6', fields, 'p');
      expect(result.where).toBe(`EXTRACT(MONTH FROM p."ModificationTimestamp") = $1`);
      expect(result.values).toEqual([6]);
    });
  });

  describe('math functions', () => {
    it('translates round', () => {
      const result = filterToSql('round(ListPrice) eq 200000', fields, 'p');
      expect(result.where).toBe(`ROUND(p."ListPrice") = $1`);
      expect(result.values).toEqual([200000]);
    });

    it('translates floor', () => {
      const result = filterToSql('floor(ListPrice) gt 100000', fields, 'p');
      expect(result.where).toBe(`FLOOR(p."ListPrice") > $1`);
      expect(result.values).toEqual([100000]);
    });

    it('translates ceiling', () => {
      const result = filterToSql('ceiling(ListPrice) lt 500000', fields, 'p');
      expect(result.where).toBe(`CEIL(p."ListPrice") < $1`);
      expect(result.values).toEqual([500000]);
    });
  });

  describe('arithmetic operators', () => {
    it('translates add', () => {
      const result = filterToSql('ListPrice add 1000 gt 300000', fields, 'p');
      expect(result.where).toBe(`(p."ListPrice" + $1) > $2`);
      expect(result.values).toEqual([1000, 300000]);
    });

    it('translates sub', () => {
      const result = filterToSql('ListPrice sub 1000 lt 200000', fields, 'p');
      expect(result.where).toBe(`(p."ListPrice" - $1) < $2`);
      expect(result.values).toEqual([1000, 200000]);
    });
  });

  describe('complex expressions', () => {
    it('handles nested logical operators', () => {
      const result = filterToSql("ListPrice gt 200000 and (City eq 'Austin' or City eq 'Dallas')", fields, 'p');
      expect(result.where).toBe(`(p."ListPrice" > $1 AND (p."City" = $2 OR p."City" = $3))`);
      expect(result.values).toEqual([200000, 'Austin', 'Dallas']);
    });

    it('handles function in comparison', () => {
      const result = filterToSql("contains(City, 'Aus') and ListPrice gt 100000", fields, 'p');
      expect(result.where).toBe(`(p."City" ILIKE $1 AND p."ListPrice" > $2)`);
      expect(result.values).toEqual(['%Aus%', 100000]);
    });
  });

  describe('parameter indexing', () => {
    it('starts from custom param index', () => {
      const result = filterToSql("City eq 'Austin'", fields, 'p', 5);
      expect(result.where).toBe(`p."City" = $5`);
      expect(result.values).toEqual(['Austin']);
    });

    it('increments from custom start', () => {
      const result = filterToSql("City eq 'Austin' and ListPrice gt 200000", fields, 'p', 3);
      expect(result.where).toBe(`(p."City" = $3 AND p."ListPrice" > $4)`);
      expect(result.values).toEqual(['Austin', 200000]);
    });
  });

  describe('error handling', () => {
    it('throws on unknown field', () => {
      expect(() => filterToSql("UnknownField eq 'test'", fields, 'p')).toThrow('Unknown field in $filter: UnknownField');
    });
  });
});
