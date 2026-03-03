import type { ValidationFailure } from '../metadata/types.js';
import { PROPERTY_RULES } from './property-rules.js';
import type { FieldRule } from './types.js';

export type { FieldRule } from './types.js';

/** Rule registry keyed by resource name. */
const RULES_BY_RESOURCE: Readonly<Record<string, ReadonlyArray<FieldRule>>> = {
  Property: PROPERTY_RULES
};

/** Returns the business rules for a given resource (empty array if none). */
export const getBusinessRules = (resourceName: string): ReadonlyArray<FieldRule> => RULES_BY_RESOURCE[resourceName] ?? [];

/** Formats a default error message for a range violation. */
const formatRangeMessage = (rule: FieldRule, value: number): string => {
  if (rule.message) return rule.message;
  if (rule.min !== undefined && value < rule.min) {
    return `Must be at least ${rule.min.toLocaleString('en-US')}.`;
  }
  if (rule.max !== undefined && value > rule.max) {
    return `Must be at most ${rule.max.toLocaleString('en-US')}.`;
  }
  return 'Value is out of range.';
};

/**
 * Validates a record against business rules for the given resource.
 * Returns an array of failures (empty if valid).
 */
export const validateBusinessRules = (resourceName: string, body: Readonly<Record<string, unknown>>): ReadonlyArray<ValidationFailure> => {
  const rules = getBusinessRules(resourceName);
  if (rules.length === 0) return [];

  const ruleMap = new Map(rules.map(r => [r.fieldName, r]));
  const failures: ValidationFailure[] = [];

  for (const [key, value] of Object.entries(body)) {
    if (typeof value !== 'number') continue;

    const rule = ruleMap.get(key);
    if (!rule) continue;

    if (rule.min !== undefined && value < rule.min) {
      failures.push({ field: key, reason: formatRangeMessage(rule, value) });
    } else if (rule.max !== undefined && value > rule.max) {
      failures.push({ field: key, reason: formatRangeMessage(rule, value) });
    }
  }

  return failures;
};
