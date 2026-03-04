import { describe, expect, it } from 'vitest';
import { astToFilterString, parseFilter } from '../src/index.js';

/** Round-trip helper: parse then serialize and compare to expected canonical form. */
const roundTrip = (input: string, expected?: string) => {
  const ast = parseFilter(input);
  const result = astToFilterString(ast);
  expect(result).toBe(expected ?? input);
};

describe('astToFilterString', () => {
  describe('comparison operators', () => {
    it('eq with string literal', () => roundTrip("City eq 'Austin'"));
    it('ne with string literal', () => roundTrip("City ne 'Dallas'"));
    it('gt with number', () => roundTrip('ListPrice gt 200000'));
    it('ge with number', () => roundTrip('ListPrice ge 100000'));
    it('lt with number', () => roundTrip('ListPrice lt 500000'));
    it('le with number', () => roundTrip('ListPrice le 300000'));
    it('eq with null', () => roundTrip('City eq null'));
    it('ne with null', () => roundTrip('City ne null'));
    it('eq with boolean true', () => roundTrip('IsActive eq true'));
    it('eq with boolean false', () => roundTrip('IsActive eq false'));
    it('has operator', () => roundTrip("Permissions has 'ReadWrite'"));
  });

  describe('in operator', () => {
    it('in with string list', () => roundTrip("City in ('Austin', 'Dallas', 'Houston')"));
    it('in with number list', () => roundTrip('Status in (1, 2, 3)'));
  });

  describe('logical operators', () => {
    it('and', () => roundTrip("City eq 'Austin' and ListPrice gt 200000"));
    it('or', () => roundTrip("City eq 'Austin' or City eq 'Dallas'"));
    it('chained and', () => roundTrip('A eq 1 and B eq 2 and C eq 3'));
    it('mixed and/or', () => roundTrip('A eq 1 and B eq 2 or C eq 3'));
    it('not', () => roundTrip("not (City eq 'Austin')"));
  });

  describe('arithmetic operators', () => {
    it('add', () => roundTrip('ListPrice add 1000 gt 300000'));
    it('sub', () => roundTrip('ListPrice sub 1000 lt 200000'));
    it('mul', () => roundTrip('Width mul Height gt 1000'));
    it('div', () => roundTrip('Total div Count eq 5'));
    it('mod', () => roundTrip('Value mod 2 eq 0'));
  });

  describe('string functions', () => {
    it('contains', () => roundTrip("contains(City,'Aus')"));
    it('startswith', () => roundTrip("startswith(City,'A')"));
    it('endswith', () => roundTrip("endswith(City,'tin')"));
    it('length', () => roundTrip('length(City) gt 5'));
    it('tolower', () => roundTrip("tolower(City) eq 'austin'"));
    it('toupper', () => roundTrip("toupper(City) eq 'AUSTIN'"));
    it('trim', () => roundTrip("trim(City) eq 'Austin'"));
    it('concat', () => roundTrip("concat(FirstName,' ',LastName) eq 'John Doe'", "concat(FirstName,' ',LastName) eq 'John Doe'"));
    it('indexof', () => roundTrip("indexof(City,'us') gt 0"));
    it('substring', () => roundTrip("substring(City,0,3) eq 'Aus'"));
  });

  describe('date/time functions', () => {
    it('year', () => roundTrip('year(Created) eq 2024'));
    it('month', () => roundTrip('month(Created) eq 6'));
    it('day', () => roundTrip('day(Created) eq 15'));
    it('hour', () => roundTrip('hour(Created) eq 12'));
    it('minute', () => roundTrip('minute(Created) eq 30'));
    it('second', () => roundTrip('second(Created) eq 0'));
  });

  describe('math functions', () => {
    it('round', () => roundTrip('round(ListPrice) eq 200000'));
    it('floor', () => roundTrip('floor(ListPrice) eq 200000'));
    it('ceiling', () => roundTrip('ceiling(ListPrice) eq 200000'));
  });

  describe('literal types', () => {
    it('date literal', () => roundTrip('Created eq 2024-01-15'));
    it('datetimeoffset literal', () => roundTrip('Modified eq 2024-01-15T10:30:00Z'));
    it('guid literal', () => roundTrip('ListingKey eq 550e8400-e29b-41d4-a716-446655440000'));
    it('string with escaped quotes', () => roundTrip("Name eq 'O''Brien'"));
  });

  describe('lambda expressions', () => {
    it('any with predicate', () => roundTrip("Tags/any(t:t eq 'luxury')"));
    it('all with predicate', () => roundTrip("Tags/all(t:t eq 'verified')"));
    it('any with or predicate', () => roundTrip("Tags/any(t:t eq 'A' or t eq 'B')"));
    it('empty any', () => roundTrip('Tags/any()'));
    it('nested property path in lambda', () => roundTrip('Orders/any(o:o/Amount gt 100)'));
  });

  describe('complex expressions', () => {
    it('multiple conditions with mixed operators', () => {
      roundTrip("City eq 'Austin' and ListPrice gt 200000 and contains(Description,'pool')");
    });

    it('nested function in comparison', () => {
      roundTrip("tolower(City) eq 'austin' and ListPrice gt 100000");
    });

    it('arithmetic in comparison', () => {
      roundTrip('ListPrice add 5000 gt 300000 and Bedrooms ge 3');
    });
  });
});
