/**
 * OData query option validator — validates $select, $orderby, $filter, $top,
 * $skip against CSDL entity type metadata.
 *
 * Uses @reso/odata-filter-parser to parse and walk $filter AST for property
 * reference validation.
 */

import { parseFilter } from "@reso/odata-filter-parser";
import type { FilterExpression } from "@reso/odata-filter-parser";
import type { CsdlEntityType } from "../csdl/types.js";
import type { ODataQueryOptions } from "../types.js";

/** A single validation error. */
export interface QueryValidationError {
  readonly option: string;
  readonly message: string;
}

/** Result of query option validation. */
export interface QueryValidationResult {
  readonly valid: boolean;
  readonly errors: ReadonlyArray<QueryValidationError>;
}

/** Extract all property names referenced in a filter AST. */
const collectPropertyNames = (expr: FilterExpression): ReadonlyArray<string> => {
  switch (expr.type) {
    case "property":
      return [expr.name];
    case "comparison":
    case "logical":
    case "arithmetic":
      return [
        ...collectPropertyNames(expr.left),
        ...collectPropertyNames(expr.right),
      ];
    case "not":
      return collectPropertyNames(expr.operand);
    case "function":
      return expr.args.flatMap(collectPropertyNames);
    case "literal":
      return [];
    case "lambda":
      return [
        ...collectPropertyNames(expr.source),
        ...collectPropertyNames(expr.predicate),
      ];
    case "collection":
      return expr.items.flatMap(collectPropertyNames);
  }
};

/** Split $expand value on commas, respecting parenthesized nested options. */
const splitExpandParts = (expand: string): ReadonlyArray<string> => {
  const parts: string[] = [];
  let depth = 0;
  let start = 0;
  for (let i = 0; i < expand.length; i++) {
    if (expand[i] === "(") depth++;
    else if (expand[i] === ")") depth--;
    else if (expand[i] === "," && depth === 0) {
      parts.push(expand.slice(start, i).trim());
      start = i + 1;
    }
  }
  parts.push(expand.slice(start).trim());
  return parts.filter((p) => p.length > 0);
};

/**
 * Validate OData system query options against a CSDL entity type.
 *
 * Checks:
 * - $select fields exist in entity type
 * - $orderby fields exist in entity type
 * - $filter property references exist in entity type
 * - $top is a non-negative integer
 * - $skip is a non-negative integer
 */
export const validateQueryOptions = (
  options: ODataQueryOptions,
  entityType: CsdlEntityType,
): QueryValidationResult => {
  const errors: QueryValidationError[] = [];
  const propertyNames = new Set(entityType.properties.map((p) => p.name));
  const navPropertyNames = new Set(entityType.navigationProperties.map((np) => np.name));

  // Validate $select
  if (options.$select) {
    const fields = options.$select.split(",").map((f) => f.trim());
    for (const field of fields) {
      if (!propertyNames.has(field)) {
        errors.push({
          option: "$select",
          message: `Unknown field '${field}' in $select`,
        });
      }
    }
  }

  // Validate $orderby
  if (options.$orderby) {
    const clauses = options.$orderby.split(",").map((c) => c.trim());
    for (const clause of clauses) {
      const field = clause.split(/\s+/)[0];
      if (!propertyNames.has(field)) {
        errors.push({
          option: "$orderby",
          message: `Unknown field '${field}' in $orderby`,
        });
      }
    }
  }

  // Validate $filter
  if (options.$filter) {
    try {
      const ast = parseFilter(options.$filter);
      const filterProps = collectPropertyNames(ast);
      for (const prop of filterProps) {
        // Allow dotted paths (navigation property paths) — only check first segment
        const topLevel = prop.split(".")[0];
        if (!propertyNames.has(topLevel) && !navPropertyNames.has(topLevel)) {
          errors.push({
            option: "$filter",
            message: `Unknown property '${prop}' in $filter`,
          });
        }
      }
    } catch (err) {
      errors.push({
        option: "$filter",
        message: `Invalid $filter expression: ${err instanceof Error ? err.message : String(err)}`,
      });
    }
  }

  // Validate $expand
  if (options.$expand) {
    // Split on commas that are NOT inside parentheses (nested options)
    const expandParts = splitExpandParts(options.$expand);
    for (const part of expandParts) {
      // Strip nested query options: "Media($select=...)" → "Media"
      const navName = part.split("(")[0].trim();
      if (!navPropertyNames.has(navName)) {
        errors.push({
          option: "$expand",
          message: `Unknown navigation property '${navName}' in $expand`,
        });
      }
    }
  }

  // Validate $top
  if (options.$top !== undefined) {
    if (!Number.isInteger(options.$top) || options.$top < 0) {
      errors.push({
        option: "$top",
        message: "$top must be a non-negative integer",
      });
    }
  }

  // Validate $skip
  if (options.$skip !== undefined) {
    if (!Number.isInteger(options.$skip) || options.$skip < 0) {
      errors.push({
        option: "$skip",
        message: "$skip must be a non-negative integer",
      });
    }
  }

  return { valid: errors.length === 0, errors };
};
