import type { ResoField, ResoLookup } from './types.js';

/** Fields that are server-generated or computed — skip during data generation. */
const SKIP_FIELDS = new Set([
  'ModificationTimestamp',
  'OriginalEntryTimestamp',
  'StatusChangeTimestamp',
  'PhotosChangeTimestamp',
  'MajorChangeType'
]);

/** Returns true if the type is an OData enum (non-Edm) type. */
export const isEnumType = (type: string): boolean => !type.startsWith('Edm.');

/** Returns true if the type is a numeric Edm type. */
const isIntType = (type: string): boolean => ['Edm.Int16', 'Edm.Int32', 'Edm.Int64', 'Edm.Byte'].includes(type);

/** Returns a random integer between min and max (inclusive). */
export const randomInt = (min: number, max: number): number => Math.floor(Math.random() * (max - min + 1)) + min;

/** Returns a random decimal between min and max with the given number of decimal places. */
export const randomDecimal = (min: number, max: number, decimals = 2): number =>
  Number((Math.random() * (max - min) + min).toFixed(decimals));

/** Returns a random element from an array. */
export const randomChoice = <T>(arr: ReadonlyArray<T>): T => arr[Math.floor(Math.random() * arr.length)];

/** Returns a random boolean. */
const randomBool = (): boolean => Math.random() > 0.5;

/** Generates a random date string within the last N days. */
const randomDate = (daysBack = 730): string => {
  const now = Date.now();
  const offset = Math.floor(Math.random() * daysBack * 24 * 60 * 60 * 1000);
  return new Date(now - offset).toISOString().split('T')[0];
};

/** Generates a random ISO 8601 datetime string within the last N days. */
const randomDateTimeOffset = (daysBack = 730): string => {
  const now = Date.now();
  const offset = Math.floor(Math.random() * daysBack * 24 * 60 * 60 * 1000);
  return new Date(now - offset).toISOString();
};

/** Generates a random time-of-day string (HH:MM:SS). */
const randomTimeOfDay = (): string => {
  const h = String(randomInt(0, 23)).padStart(2, '0');
  const m = String(randomInt(0, 59)).padStart(2, '0');
  return `${h}:${m}:00`;
};

/** Generates a random string value respecting maxLength. */
const randomString = (fieldName: string, index: number, maxLength?: number): string => {
  const val = `Sample ${fieldName} ${index + 1}`;
  if (maxLength && val.length > maxLength) {
    return val.slice(0, maxLength);
  }
  return val;
};

/** Generates a random lookup value from available lookups for a given type. */
const randomLookupValue = (type: string, lookups: Readonly<Record<string, ReadonlyArray<ResoLookup>>>): string | undefined => {
  const values = lookups[type];
  if (!values || values.length === 0) return undefined;
  return randomChoice(values).lookupValue;
};

/**
 * Generates a value for a single field based on its Edm type and constraints.
 * Returns undefined if the field should be skipped.
 */
export const generateFieldValue = (
  field: ResoField,
  lookups: Readonly<Record<string, ReadonlyArray<ResoLookup>>>,
  index: number
): unknown => {
  const { type, fieldName, maxLength, scale } = field;

  // Skip computed/server-generated fields
  if (SKIP_FIELDS.has(fieldName)) return undefined;

  // Skip key fields — server generates these
  if (fieldName.endsWith('Key') && fieldName.length > 3) return undefined;

  // Handle collection types
  if (field.isCollection || type.startsWith('Collection(')) {
    const innerType = type.replace(/^Collection\(/, '').replace(/\)$/, '');
    if (isEnumType(innerType)) {
      const values = lookups[innerType];
      if (values && values.length > 0) {
        const count = Math.min(randomInt(1, 3), values.length);
        const shuffled = [...values].sort(() => Math.random() - 0.5);
        return shuffled.slice(0, count).map(v => v.lookupValue);
      }
    }
    return [];
  }

  // Handle enum types
  if (isEnumType(type)) {
    return randomLookupValue(type, lookups);
  }

  // Handle primitive Edm types
  switch (type) {
    case 'Edm.String':
      return randomString(fieldName, index, maxLength);

    case 'Edm.Boolean':
      return randomBool();

    case 'Edm.Int16':
      return randomInt(0, 100);

    case 'Edm.Int32':
      return randomInt(0, 10000);

    case 'Edm.Int64':
      return randomInt(0, 100000);

    case 'Edm.Byte':
      return randomInt(0, 255);

    case 'Edm.Decimal':
    case 'Edm.Double':
    case 'Edm.Single': {
      const s = scale ?? 2;
      // Respect RESO metadata precision/scale: max integer digits = precision - scale
      const maxVal = field.precision ? 10 ** (field.precision - s) - 1 : 10000;
      return randomDecimal(0, maxVal, s);
    }

    case 'Edm.Date':
      return randomDate();

    case 'Edm.DateTimeOffset':
      return randomDateTimeOffset();

    case 'Edm.TimeOfDay':
      return randomTimeOfDay();

    case 'Edm.Guid':
      return crypto.randomUUID();

    default:
      // Unknown type — generate a string fallback
      if (isIntType(type)) return randomInt(0, 1000);
      return randomString(fieldName, index, maxLength);
  }
};

/** Default fill rate for nullable fields (0.0 to 1.0). */
const DEFAULT_FILL_RATE = 0.6;

/**
 * Generates a single record with values for non-key, non-computed fields.
 * Nullable fields are randomly included based on the fill rate to produce
 * realistic sparse records (real-world data rarely populates every field).
 * Resource-specific generators can override individual field values after this.
 */
export const generateRecord = (
  fields: ReadonlyArray<ResoField>,
  lookups: Readonly<Record<string, ReadonlyArray<ResoLookup>>>,
  index: number,
  fillRate = DEFAULT_FILL_RATE
): Record<string, unknown> => {
  const record: Record<string, unknown> = {};

  for (const field of fields) {
    // Randomly skip nullable fields to produce realistic sparse records
    if (field.nullable !== false && Math.random() > fillRate) continue;

    const value = generateFieldValue(field, lookups, index);
    if (value !== undefined) {
      record[field.fieldName] = value;
    }
  }

  return record;
};

/**
 * Generates multiple records for a resource.
 * This is the generic generator — resource-specific generators wrap this
 * and apply overrides for realistic domain-specific values.
 */
export const generateRecords = (
  fields: ReadonlyArray<ResoField>,
  lookups: Readonly<Record<string, ReadonlyArray<ResoLookup>>>,
  count: number
): ReadonlyArray<Record<string, unknown>> => Array.from({ length: count }, (_, i) => generateRecord(fields, lookups, i));
