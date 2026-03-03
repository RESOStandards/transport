import type { ResoField } from '../types';

/** Fields that represent currency values and should be formatted with a currency symbol. */
const CURRENCY_FIELDS = new Set(['ListPrice', 'OriginalListPrice', 'PreviousListPrice', 'ClosePrice', 'ListPriceLow']);

/** US dollar currency formatter. */
const currencyFormatter = new Intl.NumberFormat('en-US', {
  style: 'currency',
  currency: 'USD',
  minimumFractionDigits: 0,
  maximumFractionDigits: 2
});

/** US locale number formatter for decimals. */
const decimalFormatter = new Intl.NumberFormat('en-US', {
  minimumFractionDigits: 0,
  maximumFractionDigits: 2
});

/** US locale number formatter for integers. */
const integerFormatter = new Intl.NumberFormat('en-US', {
  maximumFractionDigits: 0
});

/** Returns true if the Edm type is an integer type. */
const isIntegerType = (type: string): boolean =>
  type === 'Edm.Int16' || type === 'Edm.Int32' || type === 'Edm.Int64' || type === 'Edm.Byte';

/** Returns true if the Edm type is a numeric type (integer or decimal/double). */
const isNumericType = (type: string): boolean =>
  isIntegerType(type) || type === 'Edm.Decimal' || type === 'Edm.Double' || type === 'Edm.Single';

/**
 * Coerces a value to a number if possible. OData APIs often return Edm.Decimal
 * values as JSON strings, so we need to parse them for formatting.
 */
const toNumber = (value: unknown): number | null => {
  if (typeof value === 'number') return value;
  if (typeof value === 'string') {
    const n = Number(value);
    return Number.isFinite(n) ? n : null;
  }
  return null;
};

/**
 * Formats a field value for display with locale-aware formatting.
 * Currency fields show USD symbols, numbers get thousands separators,
 * dates use locale formatting, and booleans show Yes/No.
 */
export const formatFieldValue = (value: unknown, field: ResoField | undefined): string => {
  if (value === null || value === undefined) return '\u2014';
  if (Array.isArray(value)) return value.join(', ');
  if (typeof value === 'boolean') return value ? 'Yes' : 'No';

  // Format numeric values (handles both number and string representations from OData)
  if (field && isNumericType(field.type)) {
    const n = toNumber(value);
    if (n !== null) {
      if (CURRENCY_FIELDS.has(field.fieldName)) return currencyFormatter.format(n);
      if (isIntegerType(field.type)) return integerFormatter.format(n);
      return decimalFormatter.format(n);
    }
  }

  if (field?.type === 'Edm.DateTimeOffset' && typeof value === 'string') {
    try {
      return new Date(value).toLocaleString();
    } catch {
      return String(value);
    }
  }

  return String(value);
};

/** All address-related field names used to compose a formatted address. */
export const ADDRESS_FIELDS = new Set([
  'StreetNumber',
  'StreetDirPrefix',
  'StreetName',
  'StreetSuffix',
  'StreetDirSuffix',
  'City',
  'StateOrProvince',
  'PostalCode',
  'UnparsedAddress'
]);

/** Address component fields in display order. */
const ADDRESS_PARTS: ReadonlyArray<{ readonly field: string; readonly separator: string }> = [
  { field: 'StreetNumber', separator: ' ' },
  { field: 'StreetDirPrefix', separator: ' ' },
  { field: 'StreetName', separator: ' ' },
  { field: 'StreetSuffix', separator: ' ' },
  { field: 'StreetDirSuffix', separator: ', ' },
  { field: 'City', separator: ', ' },
  { field: 'StateOrProvince', separator: ' ' },
  { field: 'PostalCode', separator: '' }
];

/**
 * Builds a formatted one-line address from a record's address fields.
 * Returns null if no address fields are populated.
 * Falls back to UnparsedAddress if structured fields are empty.
 */
export const formatAddress = (record: Readonly<Record<string, unknown>>): string | null => {
  // Try structured address first
  const parts: string[] = [];
  for (const { field, separator } of ADDRESS_PARTS) {
    const val = record[field];
    if (val !== null && val !== undefined && val !== '') {
      if (parts.length > 0) {
        // Append separator from the *previous* part
        parts[parts.length - 1] += separator;
      }
      parts.push(String(val));
    }
  }

  if (parts.length > 0) return parts.join('');

  // Fall back to UnparsedAddress
  const unparsed = record.UnparsedAddress;
  if (typeof unparsed === 'string' && unparsed.length > 0) return unparsed;

  return null;
};
