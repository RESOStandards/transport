import { describe, expect, it } from 'vitest';
import { LexerError, ParseError, parseFilter, tokenize } from '../src/index.js';
import type {
  ArithmeticExpr,
  CollectionExpr,
  ComparisonExpr,
  FunctionCallExpr,
  LambdaExpr,
  LiteralExpr,
  LogicalExpr,
  NotExpr,
  PropertyExpr
} from '../src/index.js';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const asComparison = (expr: unknown): ComparisonExpr => {
  expect((expr as ComparisonExpr).type).toBe('comparison');
  return expr as ComparisonExpr;
};

const asLogical = (expr: unknown): LogicalExpr => {
  expect((expr as LogicalExpr).type).toBe('logical');
  return expr as LogicalExpr;
};

const asProperty = (expr: unknown): PropertyExpr => {
  expect((expr as PropertyExpr).type).toBe('property');
  return expr as PropertyExpr;
};

const asLiteral = (expr: unknown): LiteralExpr => {
  expect((expr as LiteralExpr).type).toBe('literal');
  return expr as LiteralExpr;
};

const asFunction = (expr: unknown): FunctionCallExpr => {
  expect((expr as FunctionCallExpr).type).toBe('function');
  return expr as FunctionCallExpr;
};

const asLambda = (expr: unknown): LambdaExpr => {
  expect((expr as LambdaExpr).type).toBe('lambda');
  return expr as LambdaExpr;
};

const asCollection = (expr: unknown): CollectionExpr => {
  expect((expr as CollectionExpr).type).toBe('collection');
  return expr as CollectionExpr;
};

// ---------------------------------------------------------------------------
// Tokenizer
// ---------------------------------------------------------------------------

describe('tokenize', () => {
  it('tokenizes a simple comparison', () => {
    const tokens = tokenize('ListPrice gt 200000');
    expect(tokens).toHaveLength(3);
    expect(tokens[0]).toEqual({ type: 'property', value: 'ListPrice', position: 0 });
    expect(tokens[1]).toEqual({ type: 'comparison_op', value: 'gt', position: 10 });
    expect(tokens[2]).toEqual({ type: 'literal_number', value: '200000', position: 13 });
  });

  it('tokenizes string literals with single quotes', () => {
    const tokens = tokenize("City eq 'Austin'");
    expect(tokens).toHaveLength(3);
    expect(tokens[2]).toEqual({ type: 'literal_string', value: 'Austin', position: 8 });
  });

  it('handles escaped single quotes in strings', () => {
    const tokens = tokenize("City eq 'O''Brien'");
    expect(tokens[2]).toEqual({ type: 'literal_string', value: "O'Brien", position: 8 });
  });

  it('tokenizes boolean and null literals', () => {
    const tokens = tokenize('Active eq true');
    expect(tokens[2]).toEqual({ type: 'literal_boolean', value: 'true', position: 10 });

    const tokens2 = tokenize('City eq null');
    expect(tokens2[2]).toEqual({ type: 'literal_null', value: 'null', position: 8 });
  });

  it('tokenizes function calls', () => {
    const tokens = tokenize("contains(City, 'Aus')");
    expect(tokens[0]).toEqual({ type: 'function', value: 'contains', position: 0 });
    expect(tokens[1]).toEqual({ type: 'lparen', value: '(', position: 8 });
  });

  it('tokenizes negative numbers', () => {
    const tokens = tokenize('Price gt -100');
    expect(tokens[2]).toEqual({ type: 'literal_number', value: '-100', position: 9 });
  });

  it('tokenizes decimal numbers', () => {
    const tokens = tokenize('Price eq 99.95');
    expect(tokens[2]).toEqual({ type: 'literal_number', value: '99.95', position: 9 });
  });

  it('tokenizes date literals', () => {
    const tokens = tokenize('BirthDate eq 2024-01-15');
    expect(tokens[2]).toEqual({ type: 'literal_date', value: '2024-01-15', position: 13 });
  });

  it('tokenizes datetimeoffset literals with Z', () => {
    const tokens = tokenize('ModifiedAt ge 2024-01-15T12:00:00Z');
    expect(tokens[2]).toEqual({
      type: 'literal_datetimeoffset',
      value: '2024-01-15T12:00:00Z',
      position: 14
    });
  });

  it('tokenizes datetimeoffset literals with offset', () => {
    const tokens = tokenize('ModifiedAt ge 2024-01-15T12:00:00+05:00');
    expect(tokens[2]).toEqual({
      type: 'literal_datetimeoffset',
      value: '2024-01-15T12:00:00+05:00',
      position: 14
    });
  });

  it('tokenizes datetimeoffset literals with fractional seconds', () => {
    const tokens = tokenize('ModifiedAt lt 2026-03-04T13:02:21.582Z');
    expect(tokens[2]).toEqual({
      type: 'literal_datetimeoffset',
      value: '2026-03-04T13:02:21.582Z',
      position: 14
    });
  });

  it('tokenizes guid literals', () => {
    const tokens = tokenize('ListingId eq 01234567-89ab-cdef-0123-456789abcdef');
    expect(tokens[2]).toEqual({
      type: 'literal_guid',
      value: '01234567-89ab-cdef-0123-456789abcdef',
      position: 13
    });
  });

  it('tokenizes duration literals', () => {
    const tokens = tokenize("Duration eq duration'P1DT2H30M'");
    expect(tokens[2]).toEqual({
      type: 'literal_duration',
      value: 'P1DT2H30M',
      position: 12
    });
  });

  it('tokenizes enum literals', () => {
    const tokens = tokenize("Status eq org.reso.metadata.StandardStatus'Active'");
    expect(tokens[2]).toEqual({
      type: 'literal_enum',
      value: "org.reso.metadata.StandardStatus'Active'",
      position: 10
    });
  });

  it('tokenizes has operator', () => {
    const tokens = tokenize("Style has Namespace.Color'Yellow'");
    expect(tokens[1]).toEqual({ type: 'comparison_op', value: 'has', position: 6 });
  });

  it('tokenizes in operator', () => {
    const tokens = tokenize("Name in ('Milk','Cheese')");
    expect(tokens[1]).toEqual({ type: 'comparison_op', value: 'in', position: 5 });
  });

  it('tokenizes divby operator', () => {
    const tokens = tokenize('Price divby 2');
    expect(tokens[1]).toEqual({ type: 'arithmetic_op', value: 'divby', position: 6 });
  });

  it('tokenizes slash for lambda paths', () => {
    const tokens = tokenize('Orders/any(o:o/Amount gt 100)');
    expect(tokens[1]).toEqual({ type: 'slash', value: '/', position: 6 });
    expect(tokens[2]).toEqual({ type: 'lambda_op', value: 'any', position: 7 });
  });

  it('tokenizes colon for lambda variable binding', () => {
    const tokens = tokenize('Orders/any(o:o/Amount gt 100)');
    const colonToken = tokens.find(t => t.type === 'colon');
    expect(colonToken).toBeDefined();
    expect(colonToken?.value).toBe(':');
  });

  it('throws on unterminated string', () => {
    expect(() => tokenize("City eq 'unterminated")).toThrow(LexerError);
  });

  it('throws on unexpected character', () => {
    expect(() => tokenize('City eq @invalid')).toThrow(LexerError);
  });
});

// ---------------------------------------------------------------------------
// Parser — simple comparisons
// ---------------------------------------------------------------------------

describe('parseFilter — comparisons', () => {
  it('parses property gt number', () => {
    const ast = parseFilter('ListPrice gt 200000');
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('gt');
    expect(asProperty(cmp.left).name).toBe('ListPrice');
    expect(asLiteral(cmp.right).value).toBe(200000);
    expect(asLiteral(cmp.right).dataType).toBe('number');
  });

  it('parses property eq string', () => {
    const ast = parseFilter("City eq 'Austin'");
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('eq');
    expect(asProperty(cmp.left).name).toBe('City');
    expect(asLiteral(cmp.right).value).toBe('Austin');
    expect(asLiteral(cmp.right).dataType).toBe('string');
  });

  it('parses property eq null', () => {
    const ast = parseFilter('City eq null');
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('eq');
    expect(asLiteral(cmp.right).value).toBeNull();
    expect(asLiteral(cmp.right).dataType).toBe('null');
  });

  it('parses property ne boolean', () => {
    const ast = parseFilter('Active ne false');
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('ne');
    expect(asLiteral(cmp.right).value).toBe(false);
    expect(asLiteral(cmp.right).dataType).toBe('boolean');
  });

  it('parses all comparison operators', () => {
    for (const op of ['eq', 'ne', 'gt', 'ge', 'lt', 'le'] as const) {
      const ast = parseFilter(`Price ${op} 100`);
      expect(asComparison(ast).operator).toBe(op);
    }
  });

  it('parses decimal number', () => {
    const ast = parseFilter('Price eq 99.95');
    const cmp = asComparison(ast);
    expect(asLiteral(cmp.right).value).toBe(99.95);
  });

  it('parses negative number', () => {
    const ast = parseFilter('Price gt -100');
    const cmp = asComparison(ast);
    expect(asLiteral(cmp.right).value).toBe(-100);
  });
});

// ---------------------------------------------------------------------------
// Parser — logical operators
// ---------------------------------------------------------------------------

describe('parseFilter — logical operators', () => {
  it('parses and', () => {
    const ast = parseFilter("ListPrice gt 200000 and City eq 'Austin'");
    const logical = asLogical(ast);
    expect(logical.operator).toBe('and');
    expect(asComparison(logical.left).operator).toBe('gt');
    expect(asComparison(logical.right).operator).toBe('eq');
  });

  it('parses or', () => {
    const ast = parseFilter("City eq 'Austin' or City eq 'Dallas'");
    const logical = asLogical(ast);
    expect(logical.operator).toBe('or');
  });

  it('and binds tighter than or', () => {
    // A or B and C -> A or (B and C)
    const ast = parseFilter('A eq 1 or B eq 2 and C eq 3');
    const orExpr = asLogical(ast);
    expect(orExpr.operator).toBe('or');
    // left is just A eq 1
    expect(asComparison(orExpr.left).operator).toBe('eq');
    // right is B eq 2 and C eq 3
    const andExpr = asLogical(orExpr.right);
    expect(andExpr.operator).toBe('and');
  });

  it('parses chained and', () => {
    const ast = parseFilter('A eq 1 and B eq 2 and C eq 3');
    // Should be left-associative: (A eq 1 and B eq 2) and C eq 3
    const outer = asLogical(ast);
    expect(outer.operator).toBe('and');
    const inner = asLogical(outer.left);
    expect(inner.operator).toBe('and');
  });
});

// ---------------------------------------------------------------------------
// Parser — not
// ---------------------------------------------------------------------------

describe('parseFilter — not', () => {
  it('parses not comparison', () => {
    const ast = parseFilter('not Active eq true');
    const notExpr = ast as { type: 'not'; operand: unknown };
    expect(notExpr.type).toBe('not');
    const cmp = asComparison(notExpr.operand);
    expect(cmp.operator).toBe('eq');
  });

  it('parses not with function call', () => {
    const ast = parseFilter("not contains(City, 'Austin')");
    const notExpr = ast as { type: 'not'; operand: unknown };
    expect(notExpr.type).toBe('not');
    const fn = notExpr.operand as { type: 'function'; name: string };
    expect(fn.type).toBe('function');
    expect(fn.name).toBe('contains');
  });
});

// ---------------------------------------------------------------------------
// Parser — function calls (original 3)
// ---------------------------------------------------------------------------

describe('parseFilter — function calls', () => {
  it('parses contains', () => {
    const ast = parseFilter("contains(City, 'Aus')");
    const fn = ast as FunctionCallExpr;
    expect(fn.type).toBe('function');
    expect(fn.name).toBe('contains');
    expect(fn.args).toHaveLength(2);
    expect(asProperty(fn.args[0]).name).toBe('City');
    expect(asLiteral(fn.args[1]).value).toBe('Aus');
  });

  it('parses startswith', () => {
    const ast = parseFilter("startswith(City, 'A')");
    const fn = ast as FunctionCallExpr;
    expect(fn.name).toBe('startswith');
  });

  it('parses endswith', () => {
    const ast = parseFilter("endswith(City, 'tin')");
    const fn = ast as FunctionCallExpr;
    expect(fn.name).toBe('endswith');
  });

  it('parses function call in comparison', () => {
    const ast = parseFilter("contains(City, 'Aus') eq true");
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('eq');
    const fn = cmp.left as FunctionCallExpr;
    expect(fn.name).toBe('contains');
  });
});

// ---------------------------------------------------------------------------
// Parser — arithmetic
// ---------------------------------------------------------------------------

describe('parseFilter — arithmetic', () => {
  it('parses add in comparison', () => {
    const ast = parseFilter('ListPrice add 1000 gt 300000');
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('gt');
    const arith = cmp.left as ArithmeticExpr;
    expect(arith.type).toBe('arithmetic');
    expect(arith.operator).toBe('add');
    expect(asProperty(arith.left).name).toBe('ListPrice');
    expect(asLiteral(arith.right).value).toBe(1000);
  });

  it('parses chained arithmetic', () => {
    const ast = parseFilter('A add B mul C eq 10');
    const cmp = asComparison(ast);
    // Arithmetic is left-associative: (A add B) mul C
    const outer = cmp.left as ArithmeticExpr;
    expect(outer.operator).toBe('mul');
  });
});

// ---------------------------------------------------------------------------
// Parser — parenthesized grouping
// ---------------------------------------------------------------------------

describe('parseFilter — parenthesized grouping', () => {
  it('parses simple grouping', () => {
    const ast = parseFilter('(ListPrice gt 100000)');
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('gt');
  });

  it('parses grouping that changes precedence', () => {
    // Without parens: A or B and C -> A or (B and C)
    // With parens: (A or B) and C
    const ast = parseFilter('(A eq 1 or B eq 2) and C eq 3');
    const andExpr = asLogical(ast);
    expect(andExpr.operator).toBe('and');
    const orExpr = asLogical(andExpr.left);
    expect(orExpr.operator).toBe('or');
  });

  it('parses nested grouping', () => {
    const ast = parseFilter('((A eq 1))');
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('eq');
  });

  it('parses complex nested expression', () => {
    const ast = parseFilter("(ListPrice gt 100000 or ListPrice lt 50000) and City eq 'Austin'");
    const andExpr = asLogical(ast);
    expect(andExpr.operator).toBe('and');
    const orExpr = asLogical(andExpr.left);
    expect(orExpr.operator).toBe('or');
    expect(asComparison(andExpr.right).operator).toBe('eq');
  });
});

// ---------------------------------------------------------------------------
// Parser — error cases
// ---------------------------------------------------------------------------

describe('parseFilter — error cases', () => {
  it('throws on empty string', () => {
    expect(() => parseFilter('')).toThrow(ParseError);
  });

  it('throws on incomplete expression', () => {
    expect(() => parseFilter('ListPrice gt')).toThrow(ParseError);
  });

  it('throws on trailing tokens', () => {
    expect(() => parseFilter('ListPrice gt 100 200')).toThrow(ParseError);
  });

  it('throws on mismatched parentheses', () => {
    expect(() => parseFilter('(ListPrice gt 100')).toThrow(ParseError);
  });

  it('throws on unexpected token', () => {
    expect(() => parseFilter('gt gt gt')).toThrow(ParseError);
  });

  it('error includes position info', () => {
    try {
      parseFilter('City eq ');
      expect.fail('Should have thrown');
    } catch (err) {
      expect(err).toBeInstanceOf(ParseError);
      expect((err as ParseError).position).toBeGreaterThanOrEqual(0);
    }
  });
});

// ---------------------------------------------------------------------------
// Parser — edge cases
// ---------------------------------------------------------------------------

describe('parseFilter — edge cases', () => {
  it('parses property names with dots (navigation)', () => {
    const ast = parseFilter("Address.City eq 'Austin'");
    const cmp = asComparison(ast);
    expect(asProperty(cmp.left).name).toBe('Address.City');
  });

  it('parses property names with underscores', () => {
    const ast = parseFilter('list_price gt 0');
    const cmp = asComparison(ast);
    expect(asProperty(cmp.left).name).toBe('list_price');
  });

  it('handles extra whitespace', () => {
    const ast = parseFilter('  ListPrice   gt   200000  ');
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('gt');
  });

  it('handles boolean comparison as standalone function result', () => {
    const ast = parseFilter("contains(Name, 'test')");
    const fn = ast as FunctionCallExpr;
    expect(fn.type).toBe('function');
    expect(fn.name).toBe('contains');
  });
});

// ---------------------------------------------------------------------------
// Parser — has operator
// ---------------------------------------------------------------------------

describe('parseFilter — has operator', () => {
  it('parses Style has enum value', () => {
    const ast = parseFilter("Style has Namespace.Color'Yellow'");
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('has');
    expect(asProperty(cmp.left).name).toBe('Style');
    const lit = asLiteral(cmp.right);
    expect(lit.dataType).toBe('enum');
    expect(lit.value).toBe("Namespace.Color'Yellow'");
  });
});

// ---------------------------------------------------------------------------
// Parser — in operator
// ---------------------------------------------------------------------------

describe('parseFilter — in operator', () => {
  it('parses Name in collection of strings', () => {
    const ast = parseFilter("Name in ('Milk','Cheese')");
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('in');
    expect(asProperty(cmp.left).name).toBe('Name');
    const coll = asCollection(cmp.right);
    expect(coll.items).toHaveLength(2);
    expect(asLiteral(coll.items[0]).value).toBe('Milk');
    expect(asLiteral(coll.items[1]).value).toBe('Cheese');
  });

  it('parses Status in collection of multiple strings', () => {
    const ast = parseFilter("Status in ('Active','Pending','Sold')");
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('in');
    const coll = asCollection(cmp.right);
    expect(coll.items).toHaveLength(3);
    expect(asLiteral(coll.items[0]).value).toBe('Active');
    expect(asLiteral(coll.items[1]).value).toBe('Pending');
    expect(asLiteral(coll.items[2]).value).toBe('Sold');
  });
});

// ---------------------------------------------------------------------------
// Parser — divby operator
// ---------------------------------------------------------------------------

describe('parseFilter — divby operator', () => {
  it('parses Price divby 2 gt 3.5', () => {
    const ast = parseFilter('Price divby 2 gt 3.5');
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('gt');
    const arith = cmp.left as ArithmeticExpr;
    expect(arith.type).toBe('arithmetic');
    expect(arith.operator).toBe('divby');
    expect(asProperty(arith.left).name).toBe('Price');
    expect(asLiteral(arith.right).value).toBe(2);
    expect(asLiteral(cmp.right).value).toBe(3.5);
  });
});

// ---------------------------------------------------------------------------
// Parser — string functions (all 11)
// ---------------------------------------------------------------------------

describe('parseFilter — string functions', () => {
  it('parses length(Name) gt 10', () => {
    const ast = parseFilter('length(Name) gt 10');
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('gt');
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('length');
    expect(fn.args).toHaveLength(1);
    expect(asProperty(fn.args[0]).name).toBe('Name');
    expect(asLiteral(cmp.right).value).toBe(10);
  });

  it("parses indexof(Name, 'ilk') eq 1", () => {
    const ast = parseFilter("indexof(Name, 'ilk') eq 1");
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('indexof');
    expect(fn.args).toHaveLength(2);
    expect(asProperty(fn.args[0]).name).toBe('Name');
    expect(asLiteral(fn.args[1]).value).toBe('ilk');
    expect(asLiteral(cmp.right).value).toBe(1);
  });

  it("parses substring(Name, 1) eq 'ilk'", () => {
    const ast = parseFilter("substring(Name, 1) eq 'ilk'");
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('substring');
    expect(fn.args).toHaveLength(2);
  });

  it("parses substring(Name, 1, 2) eq 'il'", () => {
    const ast = parseFilter("substring(Name, 1, 2) eq 'il'");
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('substring');
    expect(fn.args).toHaveLength(3);
  });

  it("parses tolower(Name) eq 'milk'", () => {
    const ast = parseFilter("tolower(Name) eq 'milk'");
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('tolower');
    expect(fn.args).toHaveLength(1);
    expect(asLiteral(cmp.right).value).toBe('milk');
  });

  it("parses toupper(Name) eq 'MILK'", () => {
    const ast = parseFilter("toupper(Name) eq 'MILK'");
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('toupper');
    expect(fn.args).toHaveLength(1);
    expect(asLiteral(cmp.right).value).toBe('MILK');
  });

  it("parses trim(Name) eq 'Milk'", () => {
    const ast = parseFilter("trim(Name) eq 'Milk'");
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('trim');
    expect(fn.args).toHaveLength(1);
    expect(asLiteral(cmp.right).value).toBe('Milk');
  });

  it("parses concat(FirstName, LastName) eq 'JohnDoe'", () => {
    const ast = parseFilter("concat(FirstName, LastName) eq 'JohnDoe'");
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('concat');
    expect(fn.args).toHaveLength(2);
    expect(asProperty(fn.args[0]).name).toBe('FirstName');
    expect(asProperty(fn.args[1]).name).toBe('LastName');
    expect(asLiteral(cmp.right).value).toBe('JohnDoe');
  });

  it("parses matchesPattern(Name, '^Mi')", () => {
    const ast = parseFilter("matchesPattern(Name, '^Mi')");
    const fn = asFunction(ast);
    expect(fn.name).toBe('matchesPattern');
    expect(fn.args).toHaveLength(2);
    expect(asProperty(fn.args[0]).name).toBe('Name');
    expect(asLiteral(fn.args[1]).value).toBe('^Mi');
  });

  it('parses contains in comparison context', () => {
    const ast = parseFilter("contains(City, 'Aus') eq true");
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('eq');
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('contains');
  });

  it('parses startswith in comparison context', () => {
    const ast = parseFilter("startswith(City, 'A') eq true");
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('startswith');
  });

  it('parses endswith in comparison context', () => {
    const ast = parseFilter("endswith(City, 'tin') eq true");
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('endswith');
  });
});

// ---------------------------------------------------------------------------
// Parser — date/time functions (all 14)
// ---------------------------------------------------------------------------

describe('parseFilter — date/time functions', () => {
  it('parses year(BirthDate) eq 1990', () => {
    const ast = parseFilter('year(BirthDate) eq 1990');
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('year');
    expect(fn.args).toHaveLength(1);
    expect(asProperty(fn.args[0]).name).toBe('BirthDate');
    expect(asLiteral(cmp.right).value).toBe(1990);
  });

  it('parses month(BirthDate) eq 12', () => {
    const ast = parseFilter('month(BirthDate) eq 12');
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('month');
    expect(fn.args).toHaveLength(1);
    expect(asLiteral(cmp.right).value).toBe(12);
  });

  it('parses day(BirthDate) eq 25', () => {
    const ast = parseFilter('day(BirthDate) eq 25');
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('day');
    expect(fn.args).toHaveLength(1);
    expect(asLiteral(cmp.right).value).toBe(25);
  });

  it('parses hour(StartTime) eq 14', () => {
    const ast = parseFilter('hour(StartTime) eq 14');
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('hour');
    expect(fn.args).toHaveLength(1);
    expect(asLiteral(cmp.right).value).toBe(14);
  });

  it('parses minute(StartTime) eq 30', () => {
    const ast = parseFilter('minute(StartTime) eq 30');
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('minute');
    expect(fn.args).toHaveLength(1);
    expect(asLiteral(cmp.right).value).toBe(30);
  });

  it('parses second(StartTime) eq 0', () => {
    const ast = parseFilter('second(StartTime) eq 0');
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('second');
    expect(fn.args).toHaveLength(1);
    expect(asLiteral(cmp.right).value).toBe(0);
  });

  it('parses fractionalseconds(StartTime) eq 0', () => {
    const ast = parseFilter('fractionalseconds(StartTime) eq 0');
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('fractionalseconds');
    expect(fn.args).toHaveLength(1);
  });

  it('parses totalseconds(Duration) eq 3600', () => {
    const ast = parseFilter('totalseconds(Duration) eq 3600');
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('totalseconds');
    expect(fn.args).toHaveLength(1);
  });

  it('parses date(CreatedAt) eq 2024-01-15', () => {
    const ast = parseFilter('date(CreatedAt) eq 2024-01-15');
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('date');
    expect(fn.args).toHaveLength(1);
    const lit = asLiteral(cmp.right);
    expect(lit.dataType).toBe('date');
    expect(lit.value).toBe('2024-01-15');
  });

  it('parses time(CreatedAt) eq 12:30:00', () => {
    const ast = parseFilter('time(CreatedAt) eq 12:30:00');
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('time');
    expect(fn.args).toHaveLength(1);
    const lit = asLiteral(cmp.right);
    expect(lit.dataType).toBe('timeofday');
    expect(lit.value).toBe('12:30:00');
  });

  it('parses totaloffsetminutes(CreatedAt) eq 60', () => {
    const ast = parseFilter('totaloffsetminutes(CreatedAt) eq 60');
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('totaloffsetminutes');
    expect(fn.args).toHaveLength(1);
  });

  it('parses now() gt StartTime', () => {
    const ast = parseFilter('now() gt StartTime');
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('gt');
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('now');
    expect(fn.args).toHaveLength(0);
    expect(asProperty(cmp.right).name).toBe('StartTime');
  });

  it('parses maxdatetime() as zero-arg function', () => {
    const ast = parseFilter('maxdatetime() gt StartTime');
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('maxdatetime');
    expect(fn.args).toHaveLength(0);
  });

  it('parses mindatetime() as zero-arg function', () => {
    const ast = parseFilter('mindatetime() lt EndTime');
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('mindatetime');
    expect(fn.args).toHaveLength(0);
  });
});

// ---------------------------------------------------------------------------
// Parser — math functions
// ---------------------------------------------------------------------------

describe('parseFilter — math functions', () => {
  it('parses round(Price) eq 100', () => {
    const ast = parseFilter('round(Price) eq 100');
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('round');
    expect(fn.args).toHaveLength(1);
    expect(asProperty(fn.args[0]).name).toBe('Price');
    expect(asLiteral(cmp.right).value).toBe(100);
  });

  it('parses floor(Price) eq 99', () => {
    const ast = parseFilter('floor(Price) eq 99');
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('floor');
    expect(fn.args).toHaveLength(1);
    expect(asLiteral(cmp.right).value).toBe(99);
  });

  it('parses ceiling(Price) eq 100', () => {
    const ast = parseFilter('ceiling(Price) eq 100');
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('ceiling');
    expect(fn.args).toHaveLength(1);
    expect(asLiteral(cmp.right).value).toBe(100);
  });
});

// ---------------------------------------------------------------------------
// Parser — type functions (cast/isof)
// ---------------------------------------------------------------------------

describe('parseFilter — type functions', () => {
  it('parses cast(Price, Edm.Int32)', () => {
    const ast = parseFilter('cast(Price, Edm.Int32) eq 100');
    const cmp = asComparison(ast);
    const fn = asFunction(cmp.left);
    expect(fn.name).toBe('cast');
    expect(fn.args).toHaveLength(2);
    expect(asProperty(fn.args[0]).name).toBe('Price');
    expect(asProperty(fn.args[1]).name).toBe('Edm.Int32');
  });

  it('parses isof(Namespace.DerivedType)', () => {
    const ast = parseFilter('isof(Namespace.DerivedType)');
    const fn = asFunction(ast);
    expect(fn.name).toBe('isof');
    expect(fn.args).toHaveLength(1);
    expect(asProperty(fn.args[0]).name).toBe('Namespace.DerivedType');
  });
});

// ---------------------------------------------------------------------------
// Parser — lambda operators
// ---------------------------------------------------------------------------

describe('parseFilter — lambda operators', () => {
  it("parses Tags/any(t:t eq 'Featured')", () => {
    const ast = parseFilter("Tags/any(t:t eq 'Featured')");
    const lambda = asLambda(ast);
    expect(lambda.operator).toBe('any');
    expect(lambda.variable).toBe('t');
    expect(asProperty(lambda.source).name).toBe('Tags');
    const pred = asComparison(lambda.predicate);
    expect(pred.operator).toBe('eq');
    expect(asProperty(pred.left).name).toBe('t');
    expect(asLiteral(pred.right).value).toBe('Featured');
  });

  it('parses Tags/any() — empty lambda', () => {
    const ast = parseFilter('Tags/any()');
    const lambda = asLambda(ast);
    expect(lambda.operator).toBe('any');
    expect(lambda.variable).toBe('');
    expect(asProperty(lambda.source).name).toBe('Tags');
    // Empty lambda has a true literal as predicate
    const pred = asLiteral(lambda.predicate);
    expect(pred.value).toBe(true);
  });

  it('parses Orders/all(o:o/Amount gt 0)', () => {
    const ast = parseFilter('Orders/all(o:o/Amount gt 0)');
    const lambda = asLambda(ast);
    expect(lambda.operator).toBe('all');
    expect(lambda.variable).toBe('o');
    expect(asProperty(lambda.source).name).toBe('Orders');
    const pred = asComparison(lambda.predicate);
    expect(pred.operator).toBe('gt');
    expect(asProperty(pred.left).name).toBe('o/Amount');
    expect(asLiteral(pred.right).value).toBe(0);
  });

  it("parses nested lambdas: Items/any(i:i/Tags/all(t:t eq 'sale'))", () => {
    const ast = parseFilter("Items/any(i:i/Tags/all(t:t eq 'sale'))");
    const outerLambda = asLambda(ast);
    expect(outerLambda.operator).toBe('any');
    expect(outerLambda.variable).toBe('i');
    expect(asProperty(outerLambda.source).name).toBe('Items');

    const innerLambda = asLambda(outerLambda.predicate);
    expect(innerLambda.operator).toBe('all');
    expect(innerLambda.variable).toBe('t');
    // The source for the inner lambda is i/Tags
    expect(asProperty(innerLambda.source).name).toBe('i/Tags');

    const pred = asComparison(innerLambda.predicate);
    expect(pred.operator).toBe('eq');
    expect(asProperty(pred.left).name).toBe('t');
    expect(asLiteral(pred.right).value).toBe('sale');
  });
});

// ---------------------------------------------------------------------------
// Parser — date/time literals
// ---------------------------------------------------------------------------

describe('parseFilter — date/time literals', () => {
  it('parses BirthDate eq 2024-01-15', () => {
    const ast = parseFilter('BirthDate eq 2024-01-15');
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('eq');
    const lit = asLiteral(cmp.right);
    expect(lit.dataType).toBe('date');
    expect(lit.value).toBe('2024-01-15');
  });

  it('parses ModificationTimestamp ge 2024-01-15T12:00:00Z', () => {
    const ast = parseFilter('ModificationTimestamp ge 2024-01-15T12:00:00Z');
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('ge');
    const lit = asLiteral(cmp.right);
    expect(lit.dataType).toBe('datetimeoffset');
    expect(lit.value).toBe('2024-01-15T12:00:00Z');
  });
});

// ---------------------------------------------------------------------------
// Parser — GUID literals
// ---------------------------------------------------------------------------

describe('parseFilter — guid literals', () => {
  it('parses ListingId eq 01234567-89ab-cdef-0123-456789abcdef', () => {
    const ast = parseFilter('ListingId eq 01234567-89ab-cdef-0123-456789abcdef');
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('eq');
    const lit = asLiteral(cmp.right);
    expect(lit.dataType).toBe('guid');
    expect(lit.value).toBe('01234567-89ab-cdef-0123-456789abcdef');
  });
});

// ---------------------------------------------------------------------------
// Parser — duration literals
// ---------------------------------------------------------------------------

describe('parseFilter — duration literals', () => {
  it("parses Duration eq duration'P1DT2H30M'", () => {
    const ast = parseFilter("Duration eq duration'P1DT2H30M'");
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('eq');
    const lit = asLiteral(cmp.right);
    expect(lit.dataType).toBe('duration');
    expect(lit.value).toBe('P1DT2H30M');
  });
});

// ---------------------------------------------------------------------------
// Parser — enum literals
// ---------------------------------------------------------------------------

describe('parseFilter — enum literals', () => {
  it("parses Status eq org.reso.metadata.StandardStatus'Active'", () => {
    const ast = parseFilter("Status eq org.reso.metadata.StandardStatus'Active'");
    const cmp = asComparison(ast);
    expect(cmp.operator).toBe('eq');
    const lit = asLiteral(cmp.right);
    expect(lit.dataType).toBe('enum');
    expect(lit.value).toBe("org.reso.metadata.StandardStatus'Active'");
  });
});
