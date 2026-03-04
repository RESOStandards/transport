import { describe, expect, it } from 'vitest';
import { filterToMongo } from '../src/db/filter-to-mongo.js';
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

describe('filterToMongo', () => {
  describe('comparison operators', () => {
    it('translates eq', () => {
      const result = filterToMongo("City eq 'Austin'", fields);
      expect(result.query).toEqual({ City: 'Austin' });
    });

    it('translates ne', () => {
      const result = filterToMongo("City ne 'Dallas'", fields);
      expect(result.query).toEqual({ City: { $ne: 'Dallas' } });
    });

    it('translates gt', () => {
      const result = filterToMongo('ListPrice gt 200000', fields);
      expect(result.query).toEqual({ ListPrice: { $gt: 200000 } });
    });

    it('translates ge', () => {
      const result = filterToMongo('ListPrice ge 200000', fields);
      expect(result.query).toEqual({ ListPrice: { $gte: 200000 } });
    });

    it('translates lt', () => {
      const result = filterToMongo('ListPrice lt 500000', fields);
      expect(result.query).toEqual({ ListPrice: { $lt: 500000 } });
    });

    it('translates le', () => {
      const result = filterToMongo('ListPrice le 500000', fields);
      expect(result.query).toEqual({ ListPrice: { $lte: 500000 } });
    });
  });

  describe('null comparisons', () => {
    it('translates eq null', () => {
      const result = filterToMongo('City eq null', fields);
      expect(result.query).toEqual({ City: null });
    });

    it('translates ne null', () => {
      const result = filterToMongo('City ne null', fields);
      expect(result.query).toEqual({ City: { $ne: null } });
    });
  });

  describe('in operator', () => {
    it('translates in with collection', () => {
      const result = filterToMongo("City in ('Austin', 'Dallas')", fields);
      expect(result.query).toEqual({ City: { $in: ['Austin', 'Dallas'] } });
    });
  });

  describe('logical operators', () => {
    it('translates and', () => {
      const result = filterToMongo("ListPrice gt 200000 and City eq 'Austin'", fields);
      expect(result.query).toEqual({
        $and: [{ ListPrice: { $gt: 200000 } }, { City: 'Austin' }]
      });
    });

    it('translates or', () => {
      const result = filterToMongo("City eq 'Austin' or City eq 'Dallas'", fields);
      expect(result.query).toEqual({
        $or: [{ City: 'Austin' }, { City: 'Dallas' }]
      });
    });

    it('translates not', () => {
      const result = filterToMongo("not City eq 'Austin'", fields);
      expect(result.query).toEqual({ $nor: [{ City: 'Austin' }] });
    });
  });

  describe('string functions', () => {
    it('translates contains', () => {
      const result = filterToMongo("contains(City, 'Aus')", fields);
      expect(result.query).toEqual({ City: { $regex: 'Aus', $options: 'i' } });
    });

    it('translates startswith', () => {
      const result = filterToMongo("startswith(City, 'A')", fields);
      expect(result.query).toEqual({ City: { $regex: '^A', $options: 'i' } });
    });

    it('translates endswith', () => {
      const result = filterToMongo("endswith(City, 'tin')", fields);
      expect(result.query).toEqual({ City: { $regex: 'tin$', $options: 'i' } });
    });

    it('translates matchesPattern', () => {
      const result = filterToMongo("matchesPattern(City, '^A.*n$')", fields);
      expect(result.query).toEqual({ City: { $regex: '^A.*n$' } });
    });

    it('escapes regex special characters in contains', () => {
      const result = filterToMongo("contains(City, 'A.B')", fields);
      expect(result.query).toEqual({ City: { $regex: 'A\\.B', $options: 'i' } });
    });

    it('translates tolower with $expr', () => {
      const result = filterToMongo("tolower(City) eq 'austin'", fields);
      expect(result.query).toEqual({
        $expr: { $eq: [{ $toLower: '$City' }, 'austin'] }
      });
    });

    it('translates toupper with $expr', () => {
      const result = filterToMongo("toupper(City) eq 'AUSTIN'", fields);
      expect(result.query).toEqual({
        $expr: { $eq: [{ $toUpper: '$City' }, 'AUSTIN'] }
      });
    });

    it('translates length with $expr', () => {
      const result = filterToMongo('length(City) gt 5', fields);
      expect(result.query).toEqual({
        $expr: { $gt: [{ $strLenCP: '$City' }, 5] }
      });
    });

    it('translates trim with $expr', () => {
      const result = filterToMongo("trim(City) eq 'Austin'", fields);
      expect(result.query).toEqual({
        $expr: { $eq: [{ $trim: { input: '$City' } }, 'Austin'] }
      });
    });

    it('translates concat with $expr', () => {
      const result = filterToMongo("concat(City, StateOrProvince) eq 'AustinTX'", fields);
      expect(result.query).toEqual({
        $expr: { $eq: [{ $concat: ['$City', '$StateOrProvince'] }, 'AustinTX'] }
      });
    });
  });

  describe('date functions', () => {
    it('translates year with $expr', () => {
      const result = filterToMongo('year(ModificationTimestamp) eq 2024', fields);
      expect(result.query).toEqual({
        $expr: { $eq: [{ $year: '$ModificationTimestamp' }, 2024] }
      });
    });

    it('translates month with $expr', () => {
      const result = filterToMongo('month(ModificationTimestamp) eq 6', fields);
      expect(result.query).toEqual({
        $expr: { $eq: [{ $month: '$ModificationTimestamp' }, 6] }
      });
    });
  });

  describe('math functions', () => {
    it('translates round with $expr', () => {
      const result = filterToMongo('round(ListPrice) eq 200000', fields);
      expect(result.query).toEqual({
        $expr: { $eq: [{ $round: ['$ListPrice', 0] }, 200000] }
      });
    });

    it('translates floor with $expr', () => {
      const result = filterToMongo('floor(ListPrice) gt 100000', fields);
      expect(result.query).toEqual({
        $expr: { $gt: [{ $floor: '$ListPrice' }, 100000] }
      });
    });

    it('translates ceiling with $expr', () => {
      const result = filterToMongo('ceiling(ListPrice) lt 500000', fields);
      expect(result.query).toEqual({
        $expr: { $lt: [{ $ceil: '$ListPrice' }, 500000] }
      });
    });
  });

  describe('arithmetic operators', () => {
    it('translates add with $expr', () => {
      const result = filterToMongo('ListPrice add 1000 gt 300000', fields);
      expect(result.query).toEqual({
        $expr: { $gt: [{ $add: ['$ListPrice', 1000] }, 300000] }
      });
    });

    it('translates sub with $expr', () => {
      const result = filterToMongo('ListPrice sub 1000 lt 200000', fields);
      expect(result.query).toEqual({
        $expr: { $lt: [{ $subtract: ['$ListPrice', 1000] }, 200000] }
      });
    });

    it('translates mul with $expr', () => {
      const result = filterToMongo('ListPrice mul 2 gt 400000', fields);
      expect(result.query).toEqual({
        $expr: { $gt: [{ $multiply: ['$ListPrice', 2] }, 400000] }
      });
    });
  });

  describe('complex expressions', () => {
    it('handles nested logical operators', () => {
      const result = filterToMongo("ListPrice gt 200000 and (City eq 'Austin' or City eq 'Dallas')", fields);
      expect(result.query).toEqual({
        $and: [{ ListPrice: { $gt: 200000 } }, { $or: [{ City: 'Austin' }, { City: 'Dallas' }] }]
      });
    });

    it('handles function in comparison with logical', () => {
      const result = filterToMongo("contains(City, 'Aus') and ListPrice gt 100000", fields);
      expect(result.query).toEqual({
        $and: [{ City: { $regex: 'Aus', $options: 'i' } }, { ListPrice: { $gt: 100000 } }]
      });
    });
  });

  describe('error handling', () => {
    it('throws on unknown field', () => {
      expect(() => filterToMongo("UnknownField eq 'test'", fields)).toThrow('Unknown field in $filter: UnknownField');
    });
  });
});
