import type { FieldRule } from './types.js';

/** Per-field rules for the Office resource. */
export const OFFICE_RULES: ReadonlyArray<FieldRule> = [
  // Required address fields
  { fieldName: 'OfficeCity', required: true },
  { fieldName: 'OfficeStateOrProvince', required: true },
  { fieldName: 'OfficePostalCode', required: true },
  { fieldName: 'OfficeCountry', required: true }
];
