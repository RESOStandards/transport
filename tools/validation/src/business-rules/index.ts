import type { ValidationFailure } from '../metadata/types.js';
import { MEMBER_RULES } from './member-rules.js';
import { OFFICE_RULES } from './office-rules.js';
import { PROPERTY_CROSS_RULES, PROPERTY_RULES } from './property-rules.js';
import type { CrossFieldRule, FieldRule } from './types.js';

export type { CrossFieldRule, FieldRule } from './types.js';

/** Per-field rule registry keyed by resource name. */
const RULES_BY_RESOURCE: Readonly<Record<string, ReadonlyArray<FieldRule>>> = {
  Property: PROPERTY_RULES,
  Member: MEMBER_RULES,
  Office: OFFICE_RULES
};

/** Cross-field rule registry keyed by resource name. */
const CROSS_RULES_BY_RESOURCE: Readonly<Record<string, ReadonlyArray<CrossFieldRule>>> = {
  Property: PROPERTY_CROSS_RULES
};

/** Returns the per-field business rules for a given resource (empty array if none). */
export const getBusinessRules = (resourceName: string): ReadonlyArray<FieldRule> => RULES_BY_RESOURCE[resourceName] ?? [];

/** Returns the cross-field business rules for a given resource (empty array if none). */
export const getCrossFieldRules = (resourceName: string): ReadonlyArray<CrossFieldRule> => CROSS_RULES_BY_RESOURCE[resourceName] ?? [];

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
 * Checks per-field range rules first, then cross-field relationship rules.
 * When `skipRequired` is true (e.g., for PATCH partial updates), required-field checks are skipped.
 * Returns an array of failures (empty if valid).
 */
export const validateBusinessRules = (
  resourceName: string,
  body: Readonly<Record<string, unknown>>,
  skipRequired = false
): ReadonlyArray<ValidationFailure> => {
  const failures: ValidationFailure[] = [];

  // Per-field rules
  const rules = getBusinessRules(resourceName);
  if (rules.length > 0) {
    const exactRules = new Map<string, FieldRule>();
    const patternRules: FieldRule[] = [];
    for (const r of rules) {
      if (r.fieldPattern) patternRules.push(r);
      else exactRules.set(r.fieldName, r);
    }

    // Required field checks — iterate rules looking for missing fields
    // Skipped for partial updates (PATCH) where only supplied fields are validated
    if (!skipRequired) {
      for (const rule of rules) {
        if (!rule.required) continue;
        const value = body[rule.fieldName];
        if (value === undefined || value === null || value === '') {
          failures.push({ field: rule.fieldName, reason: rule.message ?? `${rule.fieldName} is required.` });
        }
      }
    }

    // Range checks — iterate body fields looking for out-of-range values
    for (const [key, value] of Object.entries(body)) {
      if (typeof value !== 'number') continue;
      // Check exact match first, then pattern rules
      const rule = exactRules.get(key) ?? patternRules.find(r => r.fieldPattern!.test(key));
      if (!rule) continue;
      if (rule.min !== undefined && value < rule.min) {
        failures.push({ field: key, reason: formatRangeMessage(rule, value) });
      } else if (rule.max !== undefined && value > rule.max) {
        failures.push({ field: key, reason: formatRangeMessage(rule, value) });
      }
    }
  }

  // Cross-field relationship rules
  const crossRules = getCrossFieldRules(resourceName);
  for (const crossRule of crossRules) {
    const failure = crossRule.validate(body);
    if (failure) failures.push(failure);
  }

  return failures;
};
