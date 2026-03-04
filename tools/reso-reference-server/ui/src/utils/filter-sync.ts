/**
 * Two-way sync between OData $filter strings and the advanced search form state.
 *
 * - buildFilterString: form entries → OData $filter string
 * - parseFilterToEntries: OData $filter string → form entries
 */

import { LexerError, ParseError, parseFilter } from '@reso/odata-filter-parser';
import type { FilterExpression } from '@reso/odata-filter-parser';
import type { ResoField } from '../types.js';
import { isEnumType, isNumericEdmType } from '../types.js';

export interface FilterEntry {
  readonly field: string;
  readonly operator: string;
  readonly value: string;
}

export interface ParseFilterResult {
  readonly entries: Map<string, FilterEntry>;
  readonly hasUnrepresentable: boolean;
  readonly parseError: string | null;
  readonly errorPosition: number | null;
}

/** Builds an OData $filter string from a list of filter entries. */
export const buildFilterString = (entries: ReadonlyArray<FilterEntry>, fields: ReadonlyArray<ResoField>): string => {
  const fieldMap = new Map(fields.map(f => [f.fieldName, f]));
  const parts: string[] = [];

  for (const entry of entries) {
    if (!entry.value.trim()) continue;
    const field = fieldMap.get(entry.field);
    if (!field) continue;

    const isString = field.type === 'Edm.String' || isEnumType(field.type);
    const isNumeric = isNumericEdmType(field.type);
    const val = isString ? `'${entry.value.replace(/'/g, "''")}'` : isNumeric ? entry.value : `'${entry.value}'`;

    if (entry.operator === 'any' || entry.operator === 'all') {
      const values = entry.value.split('|').filter(v => v.trim());
      if (values.length === 0) continue;
      const quoted = values.map(v => `'${v.replace(/'/g, "''")}'`);
      if (entry.operator === 'any') {
        const inner = quoted.map(q => `x eq ${q}`).join(' or ');
        parts.push(`${entry.field}/any(x:${inner})`);
      } else {
        for (const q of quoted) {
          parts.push(`${entry.field}/any(x:x eq ${q})`);
        }
      }
    } else if (entry.operator === 'contains') {
      parts.push(`contains(${entry.field},${val})`);
    } else {
      parts.push(`${entry.field} ${entry.operator} ${val}`);
    }
  }

  return parts.join(' and ');
};

/** Extract string value from a literal AST node. */
const literalToString = (expr: FilterExpression): string | null => {
  if (expr.type !== 'literal') return null;
  if (expr.value === null) return 'null';
  return String(expr.value);
};

/** Flatten a top-level AND chain into individual clauses. */
const flattenAnd = (expr: FilterExpression): FilterExpression[] => {
  if (expr.type === 'logical' && expr.operator === 'and') {
    return [...flattenAnd(expr.left), ...flattenAnd(expr.right)];
  }
  return [expr];
};

/**
 * Try to extract literal values from an OR chain inside a lambda predicate.
 * Matches patterns like: x eq 'A' or x eq 'B' or x eq 'C'
 */
const extractLambdaOrValues = (expr: FilterExpression, variable: string): string[] | null => {
  if (expr.type === 'comparison' && expr.operator === 'eq') {
    if (expr.left.type === 'property' && expr.left.name === variable) {
      const val = literalToString(expr.right);
      return val !== null ? [val] : null;
    }
    return null;
  }
  if (expr.type === 'logical' && expr.operator === 'or') {
    const leftVals = extractLambdaOrValues(expr.left, variable);
    const rightVals = extractLambdaOrValues(expr.right, variable);
    if (leftVals && rightVals) return [...leftVals, ...rightVals];
    return null;
  }
  return null;
};

/**
 * Try to convert a single AST clause into a FilterEntry.
 * Returns null if the clause can't be represented by the form.
 */
const clauseToEntry = (expr: FilterExpression): FilterEntry | null => {
  // Simple comparison: Property op Literal
  if (expr.type === 'comparison' && expr.operator !== 'in' && expr.operator !== 'has') {
    if (expr.left.type === 'property') {
      const val = literalToString(expr.right);
      if (val !== null) {
        return { field: expr.left.name, operator: expr.operator, value: val };
      }
    }
    return null;
  }

  // contains(Property, Literal)
  if (expr.type === 'function' && expr.name === 'contains' && expr.args.length === 2) {
    if (expr.args[0].type === 'property') {
      const val = literalToString(expr.args[1]);
      if (val !== null) {
        return { field: expr.args[0].name, operator: 'contains', value: val };
      }
    }
    return null;
  }

  // Lambda: Field/any(x: x eq 'A' or x eq 'B')
  if (expr.type === 'lambda') {
    if (expr.source.type === 'property' && expr.variable) {
      const values = extractLambdaOrValues(expr.predicate, expr.variable);
      if (values) {
        return { field: expr.source.name, operator: expr.operator, value: values.join('|') };
      }
    }
    return null;
  }

  return null;
};

/**
 * Parse an OData $filter string into form-compatible FilterEntry map.
 *
 * Handles the subset of expressions the advanced search form can represent:
 * - Simple comparisons (field op value) joined with AND
 * - contains(field, value)
 * - field/any(x: x eq 'A' or x eq 'B')
 * - "all" pattern: multiple field/any(x:x eq val) AND'd together
 *
 * Expressions the form can't represent are flagged via hasUnrepresentable.
 */
export const parseFilterToEntries = (filterString: string, _fields: ReadonlyArray<ResoField>): ParseFilterResult => {
  const empty: ParseFilterResult = { entries: new Map(), hasUnrepresentable: false, parseError: null, errorPosition: null };
  if (!filterString.trim()) return empty;

  let ast: FilterExpression;
  try {
    ast = parseFilter(filterString);
  } catch (err) {
    if (err instanceof ParseError || err instanceof LexerError) {
      return { entries: new Map(), hasUnrepresentable: false, parseError: err.message, errorPosition: err.position };
    }
    return { entries: new Map(), hasUnrepresentable: false, parseError: 'Invalid filter expression', errorPosition: null };
  }

  const clauses = flattenAnd(ast);
  const entries = new Map<string, FilterEntry>();
  let hasUnrepresentable = false;

  // First pass: convert simple clauses
  // Second pass: detect "all" pattern (multiple any lambdas for same field)
  const lambdaAnyClauses = new Map<string, string[]>();

  for (const clause of clauses) {
    const entry = clauseToEntry(clause);
    if (!entry) {
      hasUnrepresentable = true;
      continue;
    }

    // Collect any-lambdas by field to detect the "all" pattern
    if (clause.type === 'lambda' && clause.operator === 'any') {
      const fieldName = entry.field;
      const values = entry.value.split('|');
      // Single-value any lambda might be part of an "all" pattern
      if (values.length === 1) {
        const existing = lambdaAnyClauses.get(fieldName);
        if (existing) {
          existing.push(values[0]);
        } else {
          lambdaAnyClauses.set(fieldName, [values[0]]);
        }
        continue;
      }
    }

    entries.set(entry.field, entry);
  }

  // Merge collected single-value any-lambdas into "all" entries
  for (const [field, values] of lambdaAnyClauses) {
    if (values.length > 1) {
      // Multiple any(x:x eq val) for same field → "all" pattern
      entries.set(field, { field, operator: 'all', value: values.join('|') });
    } else {
      // Single any(x:x eq val) → normal "any" entry
      entries.set(field, { field, operator: 'any', value: values[0] });
    }
  }

  return { entries, hasUnrepresentable, parseError: null, errorPosition: null };
};
