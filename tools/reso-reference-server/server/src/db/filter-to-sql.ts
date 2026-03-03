/**
 * Translates an OData $filter expression into a parameterized SQL WHERE clause.
 *
 * Uses the shared @reso/odata-filter-parser library to parse the filter string
 * into an AST, then walks the tree to produce SQL with parameterized values.
 *
 * All user-provided values are parameterized ($1, $2, ...) to prevent SQL injection.
 */

import { parseFilter } from "@reso/odata-filter-parser";
import type { FilterExpression } from "@reso/odata-filter-parser";
import type { ResoField } from "../metadata/types.js";

/** Result of translating a $filter expression to SQL. */
export interface FilterSqlResult {
  /** SQL WHERE clause fragment (without the "WHERE" keyword). */
  readonly where: string;
  /** Parameterized values corresponding to $1, $2, ... placeholders. */
  readonly values: ReadonlyArray<unknown>;
}

/** Map OData comparison operators to SQL operators. */
const COMPARISON_OPS: Readonly<Record<string, string>> = {
  eq: "=",
  ne: "!=",
  gt: ">",
  ge: ">=",
  lt: "<",
  le: "<=",
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
  startParamIndex: number = 1,
): FilterSqlResult => {
  const ast = parseFilter(filterString);
  const fieldNames = new Set(fields.map((f) => f.fieldName));
  const values: unknown[] = [];
  let paramIndex = startParamIndex;

  const addParam = (value: unknown): string => {
    values.push(value);
    return `$${paramIndex++}`;
  };

  const toSql = (expr: FilterExpression): string => {
    switch (expr.type) {
      case "comparison": {
        const left = toSql(expr.left);
        const right = toSql(expr.right);
        const op = expr.operator;

        // Handle null comparisons: eq null → IS NULL, ne null → IS NOT NULL
        if (expr.right.type === "literal" && expr.right.value === null) {
          return op === "eq"
            ? `${left} IS NULL`
            : `${left} IS NOT NULL`;
        }
        if (expr.left.type === "literal" && expr.left.value === null) {
          return op === "eq"
            ? `${right} IS NULL`
            : `${right} IS NOT NULL`;
        }

        // "has" operator for enum flags: bitwise AND check
        if (op === "has") {
          return `(${left} & ${right}) = ${right}`;
        }

        // "in" operator: membership test
        if (op === "in") {
          return `${left} IN (${right})`;
        }

        const sqlOp = COMPARISON_OPS[op];
        if (!sqlOp) {
          throw new Error(`Unsupported comparison operator: ${op}`);
        }
        return `${left} ${sqlOp} ${right}`;
      }

      case "logical":
        return `(${toSql(expr.left)} ${expr.operator.toUpperCase()} ${toSql(expr.right)})`;

      case "not":
        return `NOT (${toSql(expr.operand)})`;

      case "arithmetic": {
        const aLeft = toSql(expr.left);
        const aRight = toSql(expr.right);
        const arithOps: Record<string, string> = {
          add: "+",
          sub: "-",
          mul: "*",
          div: "/",
          mod: "%",
          divby: "/",
        };
        const arithOp = arithOps[expr.operator];
        if (!arithOp) {
          throw new Error(`Unsupported arithmetic operator: ${expr.operator}`);
        }
        // divby uses floating-point division
        if (expr.operator === "divby") {
          return `(CAST(${aLeft} AS DOUBLE PRECISION) / ${aRight})`;
        }
        return `(${aLeft} ${arithOp} ${aRight})`;
      }

      case "function":
        return functionToSql(expr.name, expr.args);

      case "literal": {
        if (expr.value === null) return "NULL";
        if (typeof expr.value === "boolean") return expr.value ? "TRUE" : "FALSE";
        return addParam(expr.value);
      }

      case "property": {
        if (!fieldNames.has(expr.name)) {
          throw new Error(`Unknown field in $filter: ${expr.name}`);
        }
        return `${tableAlias}."${expr.name}"`;
      }

      case "lambda":
        // Lambda expressions (any/all) are not supported in SQL translation yet
        throw new Error(
          `Lambda expressions (${expr.operator}) are not supported in server-side $filter`,
        );

      case "collection":
        // Render as a comma-separated list of values (for "in" operator)
        return expr.items.map(toSql).join(", ");

      default:
        throw new Error(`Unsupported filter expression type: ${(expr as FilterExpression).type}`);
    }
  };

  /** Translate OData filter functions to PostgreSQL equivalents. */
  const functionToSql = (
    name: string,
    args: ReadonlyArray<FilterExpression>,
  ): string => {
    switch (name) {
      // String functions
      case "contains": {
        const field = toSql(args[0]!);
        const value = args[1]!;
        if (value.type === "literal" && typeof value.value === "string") {
          return `${field} ILIKE ${addParam(`%${value.value}%`)}`;
        }
        return `${field} ILIKE '%' || ${toSql(value)} || '%'`;
      }
      case "startswith": {
        const field = toSql(args[0]!);
        const value = args[1]!;
        if (value.type === "literal" && typeof value.value === "string") {
          return `${field} ILIKE ${addParam(`${value.value}%`)}`;
        }
        return `${field} ILIKE ${toSql(value)} || '%'`;
      }
      case "endswith": {
        const field = toSql(args[0]!);
        const value = args[1]!;
        if (value.type === "literal" && typeof value.value === "string") {
          return `${field} ILIKE ${addParam(`%${value.value}`)}`;
        }
        return `${field} ILIKE '%' || ${toSql(value)}`;
      }
      case "length":
        return `LENGTH(${toSql(args[0]!)})`;
      case "indexof": {
        // OData indexof is 0-based; PostgreSQL POSITION is 1-based
        return `(POSITION(${toSql(args[1]!)} IN ${toSql(args[0]!)}) - 1)`;
      }
      case "substring": {
        const str = toSql(args[0]!);
        // OData substring is 0-based; PostgreSQL SUBSTRING is 1-based
        const start = toSql(args[1]!);
        if (args.length >= 3) {
          const len = toSql(args[2]!);
          return `SUBSTRING(${str} FROM (${start}) + 1 FOR ${len})`;
        }
        return `SUBSTRING(${str} FROM (${start}) + 1)`;
      }
      case "tolower":
        return `LOWER(${toSql(args[0]!)})`;
      case "toupper":
        return `UPPER(${toSql(args[0]!)})`;
      case "trim":
        return `TRIM(${toSql(args[0]!)})`;
      case "concat":
        return `(${toSql(args[0]!)} || ${toSql(args[1]!)})`;
      case "matchesPattern":
        return `${toSql(args[0]!)} ~ ${toSql(args[1]!)}`;

      // Date/time functions
      case "year":
        return `EXTRACT(YEAR FROM ${toSql(args[0]!)})`;
      case "month":
        return `EXTRACT(MONTH FROM ${toSql(args[0]!)})`;
      case "day":
        return `EXTRACT(DAY FROM ${toSql(args[0]!)})`;
      case "hour":
        return `EXTRACT(HOUR FROM ${toSql(args[0]!)})`;
      case "minute":
        return `EXTRACT(MINUTE FROM ${toSql(args[0]!)})`;
      case "second":
        return `EXTRACT(SECOND FROM ${toSql(args[0]!)})`;
      case "fractionalseconds":
        return `(EXTRACT(MICROSECONDS FROM ${toSql(args[0]!)}) / 1000000.0)`;
      case "date":
        return `CAST(${toSql(args[0]!)} AS DATE)`;
      case "time":
        return `CAST(${toSql(args[0]!)} AS TIME)`;
      case "now":
        return "NOW()";
      case "maxdatetime":
        return "'9999-12-31T23:59:59.999Z'::TIMESTAMPTZ";
      case "mindatetime":
        return "'0001-01-01T00:00:00Z'::TIMESTAMPTZ";

      // Math functions
      case "round":
        return `ROUND(${toSql(args[0]!)})`;
      case "floor":
        return `FLOOR(${toSql(args[0]!)})`;
      case "ceiling":
        return `CEIL(${toSql(args[0]!)})`;

      default:
        throw new Error(`Unsupported filter function: ${name}`);
    }
  };

  const where = toSql(ast);
  return { where, values };
};
