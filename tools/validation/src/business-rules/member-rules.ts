import type { FieldRule } from './types.js';

/** Per-field rules for the Member resource. */
export const MEMBER_RULES: ReadonlyArray<FieldRule> = [
  // Required address fields
  { fieldName: 'MemberCity', required: true },
  { fieldName: 'MemberStateOrProvince', required: true },
  { fieldName: 'MemberPostalCode', required: true },
  { fieldName: 'MemberCountry', required: true }
];
