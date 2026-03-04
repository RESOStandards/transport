/**
 * Translates an OData $filter expression into a MongoDB query document.
 *
 * Uses the shared @reso/odata-filter-parser library to parse the filter string
 * into an AST, then walks the tree to produce a MongoDB query.
 *
 * Two translation modes:
 * - Query mode (toQuery): produces { field: { $op: value } } for top-level
 *   filters. Preferred because MongoDB can use indexes on these operators.
 * - Expression mode (toExpr): produces aggregation expressions for $expr
 *   contexts. Used when comparisons involve functions or arithmetic.
 */

import { parseFilter } from '@reso/odata-filter-parser';
import type { FilterExpression } from '@reso/odata-filter-parser';
import type { ResoField } from '../metadata/types.js';

/** Result of translating a $filter expression to a MongoDB query. */
export interface FilterMongoResult {
  /** MongoDB query document (for use with find(), countDocuments(), etc.). */
  readonly query: Record<string, unknown>;
}

/** Escape special regex characters in a string for use in $regex. */
const escapeRegex = (str: string): string => str.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');

/** Map OData comparison operators to MongoDB query operators. */
const COMPARISON_OPS: Readonly<Record<string, string>> = {
  ne: '$ne',
  gt: '$gt',
  ge: '$gte',
  lt: '$lt',
  le: '$lte'
};

/** Map OData arithmetic operators to MongoDB aggregation operators. */
const ARITHMETIC_OPS: Readonly<Record<string, string>> = {
  add: '$add',
  sub: '$subtract',
  mul: '$multiply',
  div: '$divide',
  mod: '$mod',
  divby: '$divide'
};

/** Map OData comparison operators to MongoDB aggregation $expr operators. */
const EXPR_COMPARISON_OPS: Readonly<Record<string, string>> = {
  eq: '$eq',
  ne: '$ne',
  gt: '$gt',
  ge: '$gte',
  lt: '$lt',
  le: '$lte'
};

/**
 * Check whether an expression is "simple" — a direct property reference or
 * literal — and can use native MongoDB query operators for index efficiency.
 * Functions and arithmetic require the $expr aggregation path.
 */
const isSimpleExpr = (expr: FilterExpression): boolean => expr.type === 'property' || expr.type === 'literal' || expr.type === 'collection';

/**
 * Translate an OData $filter string to a MongoDB query document.
 *
 * @param filterString - Raw OData $filter expression
 * @param fields - Field definitions for the resource (for validation)
 * @returns MongoDB query document
 */
export const filterToMongo = (filterString: string, fields: ReadonlyArray<ResoField>): FilterMongoResult => {
  const ast = parseFilter(filterString);
  const fieldNames = new Set(fields.map(f => f.fieldName));

  /**
   * Translate an AST node to a MongoDB query document fragment.
   * Used for top-level filters and inside $and/$or arrays.
   */
  const toQuery = (expr: FilterExpression): Record<string, unknown> => {
    switch (expr.type) {
      case 'comparison': {
        // If either side involves functions or arithmetic, use $expr
        if (
          (!isSimpleExpr(expr.left) && expr.left.type !== 'comparison') ||
          (!isSimpleExpr(expr.right) && expr.right.type !== 'comparison')
        ) {
          // Check: simple property on left, literal on right — can still use native ops
          if (expr.left.type === 'property' && isSimpleExpr(expr.right)) {
            return comparisonToNativeQuery(expr);
          }
          // Fall back to $expr for complex expressions
          return { $expr: comparisonToExpr(expr) };
        }
        return comparisonToNativeQuery(expr);
      }

      case 'logical':
        return {
          [expr.operator === 'and' ? '$and' : '$or']: [toQuery(expr.left), toQuery(expr.right)]
        };

      case 'not':
        return { $nor: [toQuery(expr.operand)] };

      case 'function': {
        // Boolean-valued functions used as standalone filters (e.g., contains(City, 'Aus'))
        // These are typically used in a comparison context, but OData allows them standalone
        // meaning "function result is truthy"
        const queryResult = functionToQuery(expr.name, expr.args);
        if (queryResult) return queryResult;
        // If the function doesn't have a direct query translation, wrap in $expr
        return { $expr: toExpr(expr) };
      }

      default:
        throw new Error(`Unsupported filter expression at query level: ${expr.type}`);
    }
  };

  /**
   * Translate a comparison node to native MongoDB query operators.
   * Produces { field: value } for eq, { field: { $op: value } } for others.
   */
  const comparisonToNativeQuery = (expr: FilterExpression & { readonly type: 'comparison' }): Record<string, unknown> => {
    const op = expr.operator;

    // Handle null comparisons
    if (expr.right.type === 'literal' && expr.right.value === null) {
      const field = getPropertyName(expr.left);
      return op === 'eq' ? { [field]: null } : { [field]: { $ne: null } };
    }
    if (expr.left.type === 'literal' && expr.left.value === null) {
      const field = getPropertyName(expr.right);
      return op === 'eq' ? { [field]: null } : { [field]: { $ne: null } };
    }

    // eq → direct value match (most index-friendly)
    if (op === 'eq') {
      const field = getPropertyName(expr.left);
      const value = toLiteralValue(expr.right);
      return { [field]: value };
    }

    // in → $in array
    if (op === 'in') {
      const field = getPropertyName(expr.left);
      const items = expr.right.type === 'collection' ? expr.right.items.map(toLiteralValue) : [toLiteralValue(expr.right)];
      return { [field]: { $in: items } };
    }

    // has → bitwise AND (use $expr with $bitAnd)
    if (op === 'has') {
      return { $expr: { $eq: [{ $bitAnd: [toExpr(expr.left), toExpr(expr.right)] }, toExpr(expr.right)] } };
    }

    // Standard comparison: gt, ge, lt, le, ne
    const mongoOp = COMPARISON_OPS[op];
    if (!mongoOp) {
      throw new Error(`Unsupported comparison operator: ${op}`);
    }
    const field = getPropertyName(expr.left);
    const value = toLiteralValue(expr.right);
    return { [field]: { [mongoOp]: value } };
  };

  /**
   * Translate a comparison to a MongoDB aggregation expression for $expr.
   * Used when one side involves functions or arithmetic.
   */
  const comparisonToExpr = (expr: FilterExpression & { readonly type: 'comparison' }): Record<string, unknown> => {
    const op = expr.operator;

    if (op === 'has') {
      return { $eq: [{ $bitAnd: [toExpr(expr.left), toExpr(expr.right)] }, toExpr(expr.right)] };
    }

    if (op === 'in') {
      return { $in: [toExpr(expr.left), toExpr(expr.right)] };
    }

    const exprOp = EXPR_COMPARISON_OPS[op];
    if (!exprOp) {
      throw new Error(`Unsupported comparison operator for $expr: ${op}`);
    }
    return { [exprOp]: [toExpr(expr.left), toExpr(expr.right)] };
  };

  /**
   * Translate an AST node to a MongoDB aggregation expression.
   * Used inside $expr contexts for functions and arithmetic.
   */
  const toExpr = (expr: FilterExpression): unknown => {
    switch (expr.type) {
      case 'property': {
        validateField(expr.name);
        return `$${expr.name}`;
      }

      case 'literal':
        return expr.value;

      case 'arithmetic': {
        const mongoOp = ARITHMETIC_OPS[expr.operator];
        if (!mongoOp) {
          throw new Error(`Unsupported arithmetic operator: ${expr.operator}`);
        }
        return { [mongoOp]: [toExpr(expr.left), toExpr(expr.right)] };
      }

      case 'function':
        return functionToExpr(expr.name, expr.args);

      case 'comparison':
        return comparisonToExpr(expr);

      case 'collection':
        return expr.items.map(item => toExpr(item));

      case 'logical':
        return {
          [expr.operator === 'and' ? '$and' : '$or']: [toExpr(expr.left), toExpr(expr.right)]
        };

      case 'not':
        return { $not: [toExpr(expr.operand)] };

      default:
        throw new Error(`Unsupported expression in $expr context: ${(expr as FilterExpression).type}`);
    }
  };

  /**
   * Translate a boolean-returning OData function to a native MongoDB query.
   * Returns null if the function doesn't have a direct query translation.
   */
  const functionToQuery = (name: string, args: ReadonlyArray<FilterExpression>): Record<string, unknown> | null => {
    switch (name) {
      case 'contains': {
        const field = getPropertyName(args[0]!);
        const value = getLiteralString(args[1]!);
        return { [field]: { $regex: escapeRegex(value), $options: 'i' } };
      }
      case 'startswith': {
        const field = getPropertyName(args[0]!);
        const value = getLiteralString(args[1]!);
        return { [field]: { $regex: `^${escapeRegex(value)}`, $options: 'i' } };
      }
      case 'endswith': {
        const field = getPropertyName(args[0]!);
        const value = getLiteralString(args[1]!);
        return { [field]: { $regex: `${escapeRegex(value)}$`, $options: 'i' } };
      }
      case 'matchesPattern': {
        const field = getPropertyName(args[0]!);
        const pattern = getLiteralString(args[1]!);
        return { [field]: { $regex: pattern } };
      }
      default:
        return null;
    }
  };

  /**
   * Translate an OData function to a MongoDB aggregation expression.
   * Used inside $expr for non-boolean functions (length, tolower, year, etc.).
   */
  const functionToExpr = (name: string, args: ReadonlyArray<FilterExpression>): unknown => {
    switch (name) {
      // String functions
      case 'contains':
        return { $regexMatch: { input: toExpr(args[0]!), regex: escapeRegex(getLiteralString(args[1]!)), options: 'i' } };
      case 'startswith':
        return { $regexMatch: { input: toExpr(args[0]!), regex: `^${escapeRegex(getLiteralString(args[1]!))}`, options: 'i' } };
      case 'endswith':
        return { $regexMatch: { input: toExpr(args[0]!), regex: `${escapeRegex(getLiteralString(args[1]!))}$`, options: 'i' } };
      case 'length':
        return { $strLenCP: toExpr(args[0]!) };
      case 'indexof':
        return { $indexOfCP: [toExpr(args[0]!), toExpr(args[1]!)] };
      case 'substring': {
        const str = toExpr(args[0]!);
        const start = toExpr(args[1]!);
        if (args.length >= 3) {
          return { $substrCP: [str, start, toExpr(args[2]!)] };
        }
        // No length → rest of string. Use a large number.
        return { $substrCP: [str, start, 10000] };
      }
      case 'tolower':
        return { $toLower: toExpr(args[0]!) };
      case 'toupper':
        return { $toUpper: toExpr(args[0]!) };
      case 'trim':
        return { $trim: { input: toExpr(args[0]!) } };
      case 'concat':
        return { $concat: [toExpr(args[0]!), toExpr(args[1]!)] };
      case 'matchesPattern':
        return { $regexMatch: { input: toExpr(args[0]!), regex: getLiteralString(args[1]!) } };

      // Date/time functions
      case 'year':
        return { $year: toExpr(args[0]!) };
      case 'month':
        return { $month: toExpr(args[0]!) };
      case 'day':
        return { $dayOfMonth: toExpr(args[0]!) };
      case 'hour':
        return { $hour: toExpr(args[0]!) };
      case 'minute':
        return { $minute: toExpr(args[0]!) };
      case 'second':
        return { $second: toExpr(args[0]!) };
      case 'fractionalseconds':
        return { $divide: [{ $millisecond: toExpr(args[0]!) }, 1000] };
      case 'date':
        return { $dateTrunc: { date: toExpr(args[0]!), unit: 'day' } };
      case 'time':
        // MongoDB doesn't have a direct time extraction; use string formatting
        return { $dateToString: { format: '%H:%M:%S', date: toExpr(args[0]!) } };
      case 'now':
        return '$$NOW';
      case 'maxdatetime':
        return new Date('9999-12-31T23:59:59.999Z');
      case 'mindatetime':
        return new Date('0001-01-01T00:00:00Z');

      // Math functions
      case 'round':
        return { $round: [toExpr(args[0]!), 0] };
      case 'floor':
        return { $floor: toExpr(args[0]!) };
      case 'ceiling':
        return { $ceil: toExpr(args[0]!) };

      default:
        throw new Error(`Unsupported filter function: ${name}`);
    }
  };

  /** Extract a property name from an expression, validating it exists. */
  const getPropertyName = (expr: FilterExpression): string => {
    if (expr.type !== 'property') {
      throw new Error(`Expected property reference, got ${expr.type}`);
    }
    validateField(expr.name);
    return expr.name;
  };

  /** Validate that a field name exists in the resource's field definitions. */
  const validateField = (name: string): void => {
    if (!fieldNames.has(name)) {
      throw new Error(`Unknown field in $filter: ${name}`);
    }
  };

  /** Extract a literal value from an expression. */
  const toLiteralValue = (expr: FilterExpression): unknown => {
    if (expr.type === 'literal') return expr.value;
    if (expr.type === 'property') {
      validateField(expr.name);
      // When used as a value in native query, this shouldn't happen —
      // property-to-property comparisons need $expr
      throw new Error('Property-to-property comparisons require $expr (not yet supported in native query mode)');
    }
    throw new Error(`Expected literal value, got ${expr.type}`);
  };

  /** Extract a string literal from an expression. */
  const getLiteralString = (expr: FilterExpression): string => {
    if (expr.type === 'literal' && typeof expr.value === 'string') return expr.value;
    throw new Error('Expected string literal');
  };

  const query = toQuery(ast);
  return { query };
};
