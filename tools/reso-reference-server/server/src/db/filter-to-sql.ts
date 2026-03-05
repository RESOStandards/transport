/**
 * Translates an OData $filter expression into a parameterized SQL WHERE clause.
 *
 * Uses the shared @reso/odata-filter-parser library to parse the filter string
 * into an AST, then walks the tree to produce SQL with parameterized values.
 *
 * All user-provided values are parameterized ($1, $2, ...) to prevent SQL injection.
 */

import { parseFilter } from '@reso/odata-filter-parser';
import type { FilterExpression, LambdaExpr } from '@reso/odata-filter-parser';
import type { ResoField } from '../metadata/types.js';

/** Result of translating a $filter expression to SQL. */
export interface FilterSqlResult {
  /** SQL WHERE clause fragment (without the "WHERE" keyword). */
  readonly where: string;
  /** Parameterized values corresponding to $1, $2, ... placeholders. */
  readonly values: ReadonlyArray<unknown>;
}

/** Map OData comparison operators to SQL operators. */
const COMPARISON_OPS: Readonly<Record<string, string>> = {
  eq: '=',
  ne: '!=',
  gt: '>',
  ge: '>=',
  lt: '<',
  le: '<='
};

/**
 * Translate an OData $filter string to a parameterized SQL WHERE clause.
 *
 * @param filterString - Raw OData $filter expression
 * @param fields - Field definitions for the resource (for validation)
 * @param tableAlias - SQL table alias to prefix column references
 * @param startParamIndex - Starting parameter index (for composing with other params)
 * @returns SQL WHERE fragment and parameter values
 */
export const filterToSql = (
  filterString: string,
  fields: ReadonlyArray<ResoField>,
  tableAlias: string,
  startParamIndex = 1
): FilterSqlResult => {
  const ast = parseFilter(filterString);
  const fieldNames = new Set(fields.map(f => f.fieldName));
  const values: unknown[] = [];
  let paramIndex = startParamIndex;

  const addParam = (value: unknown): string => {
    values.push(value);
    return `$${paramIndex++}`;
  };

  const toSql = (expr: FilterExpression): string => {
    switch (expr.type) {
      case 'comparison': {
        const left = toSql(expr.left);
        const right = toSql(expr.right);
        const op = expr.operator;

        // Handle null comparisons: eq null → IS NULL, ne null → IS NOT NULL
        if (expr.right.type === 'literal' && expr.right.value === null) {
          return op === 'eq' ? `${left} IS NULL` : `${left} IS NOT NULL`;
        }
        if (expr.left.type === 'literal' && expr.left.value === null) {
          return op === 'eq' ? `${right} IS NULL` : `${right} IS NOT NULL`;
        }

        // "has" operator for enum flags: bitwise AND check
        if (op === 'has') {
          return `(${left} & ${right}) = ${right}`;
        }

        // "in" operator: membership test
        if (op === 'in') {
          return `${left} IN (${right})`;
        }

        const sqlOp = COMPARISON_OPS[op];
        if (!sqlOp) {
          throw new Error(`Unsupported comparison operator: ${op}`);
        }
        return `${left} ${sqlOp} ${right}`;
      }

      case 'logical':
        return `(${toSql(expr.left)} ${expr.operator.toUpperCase()} ${toSql(expr.right)})`;

      case 'not':
        return `NOT (${toSql(expr.operand)})`;

      case 'arithmetic': {
        const aLeft = toSql(expr.left);
        const aRight = toSql(expr.right);
        const arithOps: Record<string, string> = {
          add: '+',
          sub: '-',
          mul: '*',
          div: '/',
          mod: '%',
          divby: '/'
        };
        const arithOp = arithOps[expr.operator];
        if (!arithOp) {
          throw new Error(`Unsupported arithmetic operator: ${expr.operator}`);
        }
        // divby uses floating-point division
        if (expr.operator === 'divby') {
          return `(CAST(${aLeft} AS DOUBLE PRECISION) / ${aRight})`;
        }
        return `(${aLeft} ${arithOp} ${aRight})`;
      }

      case 'function':
        return functionToSql(expr.name, expr.args);

      case 'literal': {
        if (expr.value === null) return 'NULL';
        if (typeof expr.value === 'boolean') return expr.value ? 'TRUE' : 'FALSE';
        return addParam(expr.value);
      }

      case 'property': {
        if (!fieldNames.has(expr.name)) {
          throw new Error(`Unknown field in $filter: ${expr.name}`);
        }
        return `${tableAlias}."${expr.name}"`;
      }

      case 'lambda': {
        // Translate any()/all() on JSONB array columns using PostgreSQL containment operators.
        // Pattern: Field/any(v:v eq 'value') → "Field" @> '["value"]'::jsonb
        // Pattern: Field/all(v:v eq 'value') → "Field" <@ '["value"]'::jsonb AND jsonb_array_length("Field") > 0
        // Pattern: Field/any() → jsonb_array_length("Field") > 0
        const source = expr.source;
        if (source.type !== 'property') {
          throw new Error('Lambda source must be a property reference');
        }
        if (!fieldNames.has(source.name)) {
          throw new Error(`Unknown field in $filter: ${source.name}`);
        }
        const col = `${tableAlias}."${source.name}"`;

        // Empty lambda: any() means "collection is non-empty"
        if (!expr.variable && expr.predicate.type === 'literal' && expr.predicate.value === true) {
          return `jsonb_array_length(${col}) > 0`;
        }

        // Extract comparison value(s) from the predicate.
        // Supports simple equality: v eq 'value'
        // Supports logical or: v eq 'a' or v eq 'b'
        const extractValues = (pred: FilterExpression): unknown[] => {
          if (pred.type === 'comparison' && pred.operator === 'eq') {
            // One side is the lambda variable, the other is a literal
            const literal = pred.left.type === 'literal' ? pred.left : pred.right.type === 'literal' ? pred.right : null;
            if (literal?.type === 'literal') return [literal.value];
          }
          if (pred.type === 'logical' && pred.operator === 'or') {
            return [...extractValues(pred.left), ...extractValues(pred.right)];
          }
          if (pred.type === 'logical' && pred.operator === 'and') {
            return [...extractValues(pred.left), ...extractValues(pred.right)];
          }
          throw new Error('Unsupported lambda predicate — expected simple equality comparisons');
        };

        const vals = extractValues(expr.predicate);

        if (expr.operator === 'any') {
          // any(v:v eq 'a') → "Field" @> '["a"]'::jsonb
          // any(v:v eq 'a' or v eq 'b') → ("Field" @> '["a"]'::jsonb OR "Field" @> '["b"]'::jsonb)
          if (vals.length === 1) {
            const param = addParam(JSON.stringify(vals));
            return `${col} @> ${param}::jsonb`;
          }
          const clauses = vals.map(v => {
            const param = addParam(JSON.stringify([v]));
            return `${col} @> ${param}::jsonb`;
          });
          return `(${clauses.join(' OR ')})`;
        }

        // all(v:v eq 'a') → every element equals 'a'
        // all(v:v eq 'a' or v eq 'b') → every element is 'a' or 'b'
        const param = addParam(JSON.stringify(vals));
        return `(jsonb_array_length(${col}) > 0 AND ${col} <@ ${param}::jsonb)`;
      }

      case 'collection':
        // Render as a comma-separated list of values (for "in" operator)
        return expr.items.map(toSql).join(', ');

      default:
        throw new Error(`Unsupported filter expression type: ${(expr as FilterExpression).type}`);
    }
  };

  /** Translate OData filter functions to PostgreSQL equivalents. */
  const functionToSql = (name: string, args: ReadonlyArray<FilterExpression>): string => {
    switch (name) {
      // String functions
      case 'contains': {
        const field = toSql(args[0]!);
        const value = args[1]!;
        if (value.type === 'literal' && typeof value.value === 'string') {
          return `${field} ILIKE ${addParam(`%${value.value}%`)}`;
        }
        return `${field} ILIKE '%' || ${toSql(value)} || '%'`;
      }
      case 'startswith': {
        const field = toSql(args[0]!);
        const value = args[1]!;
        if (value.type === 'literal' && typeof value.value === 'string') {
          return `${field} ILIKE ${addParam(`${value.value}%`)}`;
        }
        return `${field} ILIKE ${toSql(value)} || '%'`;
      }
      case 'endswith': {
        const field = toSql(args[0]!);
        const value = args[1]!;
        if (value.type === 'literal' && typeof value.value === 'string') {
          return `${field} ILIKE ${addParam(`%${value.value}`)}`;
        }
        return `${field} ILIKE '%' || ${toSql(value)}`;
      }
      case 'length':
        return `LENGTH(${toSql(args[0]!)})`;
      case 'indexof': {
        // OData indexof is 0-based; PostgreSQL POSITION is 1-based
        return `(POSITION(${toSql(args[1]!)} IN ${toSql(args[0]!)}) - 1)`;
      }
      case 'substring': {
        const str = toSql(args[0]!);
        // OData substring is 0-based; PostgreSQL SUBSTRING is 1-based
        const start = toSql(args[1]!);
        if (args.length >= 3) {
          const len = toSql(args[2]!);
          return `SUBSTRING(${str} FROM (${start}) + 1 FOR ${len})`;
        }
        return `SUBSTRING(${str} FROM (${start}) + 1)`;
      }
      case 'tolower':
        return `LOWER(${toSql(args[0]!)})`;
      case 'toupper':
        return `UPPER(${toSql(args[0]!)})`;
      case 'trim':
        return `TRIM(${toSql(args[0]!)})`;
      case 'concat':
        return `(${toSql(args[0]!)} || ${toSql(args[1]!)})`;
      case 'matchesPattern':
        return `${toSql(args[0]!)} ~ ${toSql(args[1]!)}`;

      // Date/time functions
      case 'year':
        return `EXTRACT(YEAR FROM ${toSql(args[0]!)})`;
      case 'month':
        return `EXTRACT(MONTH FROM ${toSql(args[0]!)})`;
      case 'day':
        return `EXTRACT(DAY FROM ${toSql(args[0]!)})`;
      case 'hour':
        return `EXTRACT(HOUR FROM ${toSql(args[0]!)})`;
      case 'minute':
        return `EXTRACT(MINUTE FROM ${toSql(args[0]!)})`;
      case 'second':
        return `EXTRACT(SECOND FROM ${toSql(args[0]!)})`;
      case 'fractionalseconds':
        return `(EXTRACT(MICROSECONDS FROM ${toSql(args[0]!)}) / 1000000.0)`;
      case 'date':
        return `CAST(${toSql(args[0]!)} AS DATE)`;
      case 'time':
        return `CAST(${toSql(args[0]!)} AS TIME)`;
      case 'now':
        return 'NOW()';
      case 'maxdatetime':
        return "'9999-12-31T23:59:59.999Z'::TIMESTAMPTZ";
      case 'mindatetime':
        return "'0001-01-01T00:00:00Z'::TIMESTAMPTZ";

      // Math functions
      case 'round':
        return `ROUND(${toSql(args[0]!)})`;
      case 'floor':
        return `FLOOR(${toSql(args[0]!)})`;
      case 'ceiling':
        return `CEIL(${toSql(args[0]!)})`;

      default:
        throw new Error(`Unsupported filter function: ${name}`);
    }
  };

  const where = toSql(ast);
  return { where, values };
};
