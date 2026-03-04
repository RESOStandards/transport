import { validateBusinessRules } from '../business-rules/index.js';
import { isEnumType, isIntegerEdmType, isNumericEdmType } from './helpers.js';
import type { ResoField, ValidationFailure } from './types.js';

/** Fields that legitimately hold negative numeric values (e.g., coordinates). */
const ALLOW_NEGATIVE_FIELDS = new Set(['Latitude', 'Longitude']);

/**
 * Validates a record against the resource's field definitions.
 *
 * Checks for:
 * - Unknown fields (not in metadata)
 * - Null/undefined/empty-string passthrough (no error)
 * - Negative numeric values
 * - MaxLength enforcement for strings
 * - Integer-only enforcement for Int types
 * - Basic type mismatches (string, number, boolean, date, enum, collection)
 *
 * Returns an array of failures (empty if valid).
 */
export const validateRecord = (
  body: Readonly<Record<string, unknown>>,
  fields: ReadonlyArray<ResoField>
): ReadonlyArray<ValidationFailure> => {
  const fieldMap = new Map(fields.map(f => [f.fieldName, f]));
  const failures: ValidationFailure[] = [];

  for (const [key, value] of Object.entries(body)) {
    // Skip OData annotations
    if (key.startsWith('@')) continue;

    const field = fieldMap.get(key);
    if (!field) {
      failures.push({
        field: key,
        reason: `'${key}' is not a recognized field. Check field name spelling or consult the metadata.`
      });
      continue;
    }

    // Skip null, undefined, and empty string values
    if (value === null || value === undefined || value === '') continue;

    // Check negative numerics (Latitude/Longitude are allowed to be negative)
    if (typeof value === 'number' && value < 0 && isNumericEdmType(field.type) && !ALLOW_NEGATIVE_FIELDS.has(key)) {
      failures.push({ field: key, reason: 'Value must be greater than or equal to 0.' });
      continue;
    }

    // MaxLength for strings
    if (typeof value === 'string' && field.maxLength && value.length > field.maxLength) {
      failures.push({ field: key, reason: `Exceeds maximum length of ${field.maxLength} characters (currently ${value.length}).` });
      continue;
    }

    // Basic type validation
    const typeError = validateFieldType(key, value, field);
    if (typeError) {
      failures.push(typeError);
    }
  }

  // Field-specific business rules (range constraints, etc.)
  // Skip fields that already have type-level failures to avoid duplicate errors.
  const resourceName = fields[0]?.resourceName;
  if (resourceName) {
    const failedFields = new Set(failures.map(f => f.field));
    const ruleFailures = validateBusinessRules(resourceName, body);
    for (const rf of ruleFailures) {
      if (!failedFields.has(rf.field)) failures.push(rf);
    }
  }

  return failures;
};

/** Validates a single field value against its expected Edm type. */
const validateFieldType = (fieldName: string, value: unknown, field: ResoField): ValidationFailure | undefined => {
  if (field.isCollection) {
    if (!Array.isArray(value)) {
      return { field: fieldName, reason: 'Must be a list of values.' };
    }
    return undefined;
  }

  if (isEnumType(field.type)) {
    if (typeof value !== 'string' && typeof value !== 'number') {
      return { field: fieldName, reason: 'Please select a valid option from the list.' };
    }
    return undefined;
  }

  switch (field.type) {
    case 'Edm.String':
      if (typeof value !== 'string') {
        return { field: fieldName, reason: 'Must be text.' };
      }
      break;
    case 'Edm.Boolean':
      if (typeof value !== 'boolean') {
        return { field: fieldName, reason: 'Must be true or false.' };
      }
      break;
    case 'Edm.Int64':
    case 'Edm.Int32':
    case 'Edm.Int16':
    case 'Edm.Byte':
      if (typeof value !== 'number') {
        return { field: fieldName, reason: 'Must be a whole number.' };
      }
      if (!Number.isInteger(value)) {
        return { field: fieldName, reason: 'Must be a whole number (no decimals).' };
      }
      break;
    case 'Edm.Decimal':
    case 'Edm.Double':
    case 'Edm.Single':
      if (typeof value !== 'number') {
        return { field: fieldName, reason: 'Must be a number.' };
      }
      break;
    case 'Edm.Date':
      if (typeof value !== 'string') {
        return { field: fieldName, reason: 'Must be a date (YYYY-MM-DD).' };
      }
      break;
    case 'Edm.DateTimeOffset':
      if (typeof value !== 'string') {
        return { field: fieldName, reason: 'Must be a date and time.' };
      }
      break;
  }

  return undefined;
};
