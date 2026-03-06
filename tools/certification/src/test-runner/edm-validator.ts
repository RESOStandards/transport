/**
 * Lightweight Edm type validator for OData record payloads.
 *
 * Checks JS runtime values against Edm types from parsed metadata.
 * This is a simple type checker — full JSON Schema / ajv validation
 * is planned for DD compliance (#42).
 */

import type { EntityType, TestAssertion, TestStatus } from './types.js';

// ── Edm Type Patterns ──

const DATE_PATTERN = /^\d{4}-\d{2}-\d{2}$/;
const DATETIME_OFFSET_PATTERN = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/;
const TIME_OF_DAY_PATTERN = /^\d{2}:\d{2}:\d{2}/;
const GUID_PATTERN = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;
const DURATION_PATTERN = /^-?P(\d+D)?(T(\d+H)?(\d+M)?(\d+(\.\d+)?S)?)?$/;

/** Extracts the inner type from a Collection() type string. Returns null if not a collection. */
const extractCollectionType = (edmType: string): string | null => {
  if (edmType.startsWith('Collection(') && edmType.endsWith(')')) {
    return edmType.slice(11, -1);
  }
  return null;
};

/** Checks whether a JS value matches a primitive Edm type. Returns [valid, reason]. */
const checkPrimitiveType = (value: unknown, edmType: string): readonly [boolean, string] => {
  switch (edmType) {
    case 'Edm.String':
      return [typeof value === 'string', 'expected string'];

    case 'Edm.Boolean':
      return [typeof value === 'boolean', 'expected boolean'];

    case 'Edm.Int16':
    case 'Edm.Int32':
      return [typeof value === 'number' && Number.isInteger(value), 'expected integer'];

    case 'Edm.Int64':
      // OData allows Int64 as either number or string (for precision beyond Number.MAX_SAFE_INTEGER)
      return [typeof value === 'number' || typeof value === 'string', 'expected number or string'];

    case 'Edm.Decimal':
    case 'Edm.Double':
    case 'Edm.Single':
      return [typeof value === 'number', 'expected number'];

    case 'Edm.Byte':
    case 'Edm.SByte':
      return [typeof value === 'number' && Number.isInteger(value), 'expected integer'];

    case 'Edm.DateTimeOffset':
      return [typeof value === 'string' && DATETIME_OFFSET_PATTERN.test(value), 'expected ISO 8601 datetime'];

    case 'Edm.Date':
      return [typeof value === 'string' && DATE_PATTERN.test(value), 'expected YYYY-MM-DD'];

    case 'Edm.TimeOfDay':
      return [typeof value === 'string' && TIME_OF_DAY_PATTERN.test(value), 'expected HH:MM:SS'];

    case 'Edm.Guid':
      return [typeof value === 'string' && GUID_PATTERN.test(value), 'expected GUID'];

    case 'Edm.Binary':
      return [typeof value === 'string', 'expected base64 string'];

    case 'Edm.Duration':
      return [typeof value === 'string' && DURATION_PATTERN.test(value), 'expected ISO 8601 duration'];

    case 'Edm.Stream':
      return [typeof value === 'string', 'expected string (URL or base64)'];

    default:
      // Unknown Edm type (e.g., enum types) — skip validation
      return [true, ''];
  }
};

// ── Public API ──

/**
 * Validates a single field value against its Edm type.
 * Returns a TestAssertion with pass/fail/warn/skip status.
 */
export const validateValueAgainstEdm = (fieldName: string, value: unknown, edmType: string, nullable: boolean): TestAssertion => {
  // Null check
  if (value === null || value === undefined) {
    if (nullable) {
      return { description: `${fieldName}: null is valid (nullable)`, status: 'pass' };
    }
    return {
      description: `${fieldName}: null not allowed`,
      status: 'fail',
      expected: edmType,
      actual: 'null'
    };
  }

  // Collection type
  const innerType = extractCollectionType(edmType);
  if (innerType) {
    if (!Array.isArray(value)) {
      return {
        description: `${fieldName}: expected array for ${edmType}`,
        status: 'fail',
        expected: 'array',
        actual: typeof value
      };
    }
    // Check each element
    const badIndex = (value as ReadonlyArray<unknown>).findIndex(elem => {
      const [valid] = checkPrimitiveType(elem, innerType);
      return !valid;
    });
    if (badIndex >= 0) {
      return {
        description: `${fieldName}[${badIndex}]: invalid element type for ${innerType}`,
        status: 'fail',
        expected: innerType,
        actual: typeof (value as ReadonlyArray<unknown>)[badIndex]
      };
    }
    return { description: `${fieldName}: valid ${edmType}`, status: 'pass' };
  }

  // Primitive type
  const [valid, reason] = checkPrimitiveType(value, edmType);

  // Unknown Edm types (enum types, complex types) — warn but don't fail
  if (!edmType.startsWith('Edm.')) {
    return { description: `${fieldName}: skipped (non-primitive type ${edmType})`, status: 'skip' };
  }

  if (valid) {
    return { description: `${fieldName}: valid ${edmType}`, status: 'pass' };
  }

  return {
    description: `${fieldName}: type mismatch`,
    status: 'fail',
    expected: `${edmType} (${reason})`,
    actual: `${typeof value}: ${JSON.stringify(value)}`.slice(0, 200)
  };
};

/**
 * Validates all fields in a record against an EntityType's metadata.
 *
 * Checks:
 * 1. Each field in the record has a matching property in metadata (or warns/fails based on strict)
 * 2. Each field's value matches its declared Edm type
 *
 * OData annotations (keys starting with @) are skipped.
 * Runtime: O(n) where n = number of fields in the record.
 */
export const validateRecordAgainstMetadata = (
  record: Readonly<Record<string, unknown>>,
  entityType: EntityType,
  strict: boolean
): ReadonlyArray<TestAssertion> => {
  const propMap = new Map(entityType.properties.map(p => [p.name, p]));
  const results: TestAssertion[] = [];

  for (const [key, value] of Object.entries(record)) {
    // Skip OData annotations
    if (key.startsWith('@')) continue;

    const prop = propMap.get(key);
    if (!prop) {
      const status: TestStatus = strict ? 'fail' : 'warn';
      results.push({
        description: `${key}: not in metadata`,
        status,
        expected: '(field in metadata)',
        actual: key
      });
      continue;
    }

    // Nullable defaults to true per OData convention
    const nullable = prop.nullable !== false;
    results.push(validateValueAgainstEdm(key, value, prop.type, nullable));
  }

  return results;
};
