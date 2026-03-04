import { generateRecord } from './field-generator.js';
import type { ResoField, ResoLookup } from './types.js';

/**
 * Generates records for Property child resources that link via ListingKey.
 * Works for PropertyRooms, PropertyGreenVerification, PropertyPowerProduction,
 * and PropertyUnitTypes.
 */
export const generatePropertyChildRecords = (
  fields: ReadonlyArray<ResoField>,
  lookups: Readonly<Record<string, ReadonlyArray<ResoLookup>>>,
  count: number,
  _parentResource?: string,
  parentKey?: string
): ReadonlyArray<Record<string, unknown>> =>
  Array.from({ length: count }, (_, i) => {
    const record = generateRecord(fields, lookups, i);
    if (parentKey) record.ListingKey = parentKey;
    return record;
  });
