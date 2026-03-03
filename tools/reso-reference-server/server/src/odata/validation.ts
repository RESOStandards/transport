import type { ResoField } from "../metadata/types.js";
import { isEnumType } from "../metadata/loader.js";

/** A validation failure for a single field. */
export interface ValidationFailure {
  readonly field: string;
  readonly reason: string;
}

/**
 * Validates a request body against the resource's field definitions.
 *
 * Checks for:
 * - Unknown fields (not in metadata)
 * - Negative numeric values (convention for triggering 400 errors in test payloads)
 * - Basic type mismatches
 *
 * Returns an array of failures (empty if valid).
 */
export const validateRequestBody = (
  body: Readonly<Record<string, unknown>>,
  fields: ReadonlyArray<ResoField>,
): ReadonlyArray<ValidationFailure> => {
  const fieldMap = new Map(fields.map((f) => [f.fieldName, f]));
  const failures: ValidationFailure[] = [];

  for (const [key, value] of Object.entries(body)) {
    // Skip OData annotations
    if (key.startsWith("@")) continue;

    const field = fieldMap.get(key);
    if (!field) {
      failures.push({ field: key, reason: `Unknown field: ${key}` });
      continue;
    }

    if (value === null || value === undefined) continue;

    // Check negative numerics
    if (typeof value === "number" && value < 0 && isNumericEdmType(field.type)) {
      failures.push({
        field: key,
        reason: `${key} must be greater than or equal to 0`,
      });
      continue;
    }

    // Basic type validation
    const typeError = validateFieldType(key, value, field);
    if (typeError) {
      failures.push(typeError);
    }
  }

  return failures;
};

/** Checks if an Edm type is numeric. */
const isNumericEdmType = (type: string): boolean =>
  [
    "Edm.Decimal",
    "Edm.Int64",
    "Edm.Int32",
    "Edm.Int16",
    "Edm.Double",
    "Edm.Single",
    "Edm.Byte",
  ].includes(type);

/** Validates a single field value against its expected Edm type. */
const validateFieldType = (
  fieldName: string,
  value: unknown,
  field: ResoField,
): ValidationFailure | undefined => {
  if (field.isCollection) {
    if (!Array.isArray(value)) {
      return { field: fieldName, reason: `${fieldName} must be an array` };
    }
    return undefined;
  }

  if (isEnumType(field.type)) {
    // Enum fields accept strings (or numbers for integer enums)
    if (typeof value !== "string" && typeof value !== "number") {
      return { field: fieldName, reason: `${fieldName} must be a string or number` };
    }
    return undefined;
  }

  switch (field.type) {
    case "Edm.String":
      if (typeof value !== "string") {
        return { field: fieldName, reason: `${fieldName} must be a string` };
      }
      break;
    case "Edm.Boolean":
      if (typeof value !== "boolean") {
        return { field: fieldName, reason: `${fieldName} must be a boolean` };
      }
      break;
    case "Edm.Int64":
    case "Edm.Int32":
    case "Edm.Int16":
    case "Edm.Byte":
    case "Edm.Decimal":
    case "Edm.Double":
    case "Edm.Single":
      if (typeof value !== "number") {
        return { field: fieldName, reason: `${fieldName} must be a number` };
      }
      break;
    case "Edm.Date":
    case "Edm.DateTimeOffset":
      if (typeof value !== "string") {
        return { field: fieldName, reason: `${fieldName} must be a date string` };
      }
      break;
  }

  return undefined;
};
