/**
 * Translates an OData $filter expression into a parameterized SQLite WHERE clause.
 *
 * Uses the shared @reso/odata-filter-parser library to parse the filter string
 * into an AST, then walks the tree to produce SQLite SQL with parameterized values.
 *
 * All user-provided values are parameterized (?) to prevent SQL injection.
 */

import { parseFilter } from '@reso/odata-filter-parser';
import type { FilterExpression } from '@reso/odata-filter-parser';
import type { ResoField } from '../metadata/types.js';
import type { FilterSqlResult } from './filter-to-sql.js';

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
 * Translate an OData $filter string to a parameterized SQLite WHERE clause.
 *
 * @param filterString - Raw OData $filter expression
 * @param fields - Field definitions for the resource (for validation)
 * @param tableAlias - SQL table alias to prefix column references
 * @param _startParamIndex - Unused for SQLite (kept for API compatibility)
 * @returns SQL WHERE fragment and parameter values
 */
export const filterToSqlite = (
  filterString: string,
  fields: ReadonlyArray<ResoField>,
  tableAlias: string,
  _startParamIndex = 1
): FilterSqlResult => {
  const ast = parseFilter(filterString);
  const fieldNames = new Set(fields.map(f => f.fieldName));
  const values: unknown[] = [];

  const addParam = (value: unknown): string => {
    values.push(value);
    return '?';
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
          return `(CAST(${aLeft} AS REAL) / ${aRight})`;
        }
        return `(${aLeft} ${arithOp} ${aRight})`;
      }

      case 'function':
        return functionToSqlite(expr.name, expr.args);

      case 'literal': {
        if (expr.value === null) return 'NULL';
        if (typeof expr.value === 'boolean') return expr.value ? '1' : '0';
        return addParam(expr.value);
      }

      case 'property': {
        if (!fieldNames.has(expr.name)) {
          throw new Error(`Unknown field in $filter: ${expr.name}`);
        }
        return `${tableAlias}."${expr.name}"`;
      }

      case 'lambda': {
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
          return `json_array_length(${col}) > 0`;
        }

        // Extract comparison value(s) from the predicate.
        const extractValues = (pred: FilterExpression): unknown[] => {
          if (pred.type === 'comparison' && pred.operator === 'eq') {
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
          // any(v:v eq 'a') → EXISTS (SELECT 1 FROM json_each(col) WHERE value = ?)
          if (vals.length === 1) {
            const param = addParam(vals[0]);
            return `EXISTS (SELECT 1 FROM json_each(${col}) WHERE value = ${param})`;
          }
          // any(v:v eq 'a' or v eq 'b') → EXISTS (SELECT 1 FROM json_each(col) WHERE value IN (?, ?))
          const params = vals.map(v => addParam(v));
          return `EXISTS (SELECT 1 FROM json_each(${col}) WHERE value IN (${params.join(', ')}))`;
        }

        // all(v:v eq 'a') → non-empty AND no element differs
        if (vals.length === 1) {
          const param = addParam(vals[0]);
          return `(json_array_length(${col}) > 0 AND NOT EXISTS (SELECT 1 FROM json_each(${col}) WHERE value != ${param}))`;
        }
        const params = vals.map(v => addParam(v));
        return `(json_array_length(${col}) > 0 AND NOT EXISTS (SELECT 1 FROM json_each(${col}) WHERE value NOT IN (${params.join(', ')})))`;
      }

      case 'collection':
        return expr.items.map(toSql).join(', ');

      default:
        throw new Error(`Unsupported filter expression type: ${(expr as FilterExpression).type}`);
    }
  };

  /** Translate OData filter functions to SQLite equivalents. */
  const functionToSqlite = (name: string, args: ReadonlyArray<FilterExpression>): string => {
    switch (name) {
      // String functions
      case 'contains': {
        const field = toSql(args[0]!);
        const value = args[1]!;
        if (value.type === 'literal' && typeof value.value === 'string') {
          return `${field} LIKE ${addParam(`%${value.value}%`)}`;
        }
        return `${field} LIKE '%' || ${toSql(value)} || '%'`;
      }
      case 'startswith': {
        const field = toSql(args[0]!);
        const value = args[1]!;
        if (value.type === 'literal' && typeof value.value === 'string') {
          return `${field} LIKE ${addParam(`${value.value}%`)}`;
        }
        return `${field} LIKE ${toSql(value)} || '%'`;
      }
      case 'endswith': {
        const field = toSql(args[0]!);
        const value = args[1]!;
        if (value.type === 'literal' && typeof value.value === 'string') {
          return `${field} LIKE ${addParam(`%${value.value}`)}`;
        }
        return `${field} LIKE '%' || ${toSql(value)}`;
      }
      case 'length':
        return `LENGTH(${toSql(args[0]!)})`;
      case 'indexof':
        // OData indexof is 0-based; SQLite instr is 1-based
        return `(instr(${toSql(args[0]!)}, ${toSql(args[1]!)}) - 1)`;
      case 'substring': {
        const str = toSql(args[0]!);
        // OData substring is 0-based; SQLite substr is 1-based
        const start = toSql(args[1]!);
        if (args.length >= 3) {
          const len = toSql(args[2]!);
          return `substr(${str}, (${start}) + 1, ${len})`;
        }
        return `substr(${str}, (${start}) + 1)`;
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
        return `${toSql(args[0]!)} REGEXP ${toSql(args[1]!)}`;

      // Date/time functions
      case 'year':
        return `CAST(strftime('%Y', ${toSql(args[0]!)}) AS INTEGER)`;
      case 'month':
        return `CAST(strftime('%m', ${toSql(args[0]!)}) AS INTEGER)`;
      case 'day':
        return `CAST(strftime('%d', ${toSql(args[0]!)}) AS INTEGER)`;
      case 'hour':
        return `CAST(strftime('%H', ${toSql(args[0]!)}) AS INTEGER)`;
      case 'minute':
        return `CAST(strftime('%M', ${toSql(args[0]!)}) AS INTEGER)`;
      case 'second':
        return `CAST(strftime('%S', ${toSql(args[0]!)}) AS INTEGER)`;
      case 'fractionalseconds':
        return `(CAST(strftime('%f', ${toSql(args[0]!)}) AS REAL) - CAST(strftime('%S', ${toSql(args[0]!)}) AS INTEGER))`;
      case 'date':
        return `date(${toSql(args[0]!)})`;
      case 'time':
        return `time(${toSql(args[0]!)})`;
      case 'now':
        // Use ISO 8601 format to match stored DateTimeOffset values (text comparison)
        return "strftime('%Y-%m-%dT%H:%M:%fZ', 'now')";
      case 'maxdatetime':
        return "'9999-12-31T23:59:59.999Z'";
      case 'mindatetime':
        return "'0001-01-01T00:00:00Z'";

      // Math functions
      case 'round':
        return `ROUND(${toSql(args[0]!)})`;
      case 'floor':
        return `floor(${toSql(args[0]!)})`;
      case 'ceiling':
        return `ceil(${toSql(args[0]!)})`;

      default:
        throw new Error(`Unsupported filter function: ${name}`);
    }
  };

  const where = toSql(ast);
  return { where, values };
};
