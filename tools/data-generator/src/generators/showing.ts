import { generateRecord, randomChoice, randomInt } from './field-generator.js';
import type { ResoField, ResoLookup } from './types.js';

/**
 * Generates realistic Showing records linked to a parent resource.
 * Uses DD 2.0 field names: ShowingStartTimestamp, ShowingEndTimestamp.
 */
export const generateShowingRecords = (
  fields: ReadonlyArray<ResoField>,
  lookups: Readonly<Record<string, ReadonlyArray<ResoLookup>>>,
  count: number,
  _parentResource?: string,
  parentKey?: string
): ReadonlyArray<Record<string, unknown>> =>
  Array.from({ length: count }, (_, i) => {
    const record = generateRecord(fields, lookups, i);

    // Link to parent Property via ListingKey
    if (parentKey) record.ListingKey = parentKey;

    // Schedule showings in the near future
    const daysAhead = randomInt(1, 30);
    const startDate = new Date(Date.now() + daysAhead * 86400000);
    const hour = randomChoice([9, 10, 11, 13, 14, 15, 16]);
    startDate.setHours(hour, 0, 0, 0);

    const endDate = new Date(startDate);
    endDate.setHours(hour + 1);

    record.ShowingStartTimestamp = startDate.toISOString();
    record.ShowingEndTimestamp = endDate.toISOString();

    // Status
    const statusValues = lookups['org.reso.metadata.enums.ShowingStatus'];
    if (statusValues && statusValues.length > 0) {
      const active = statusValues.find(s => s.lookupValue === 'Active' || s.lookupValue === 'Confirmed');
      record.ShowingStatus = active ? active.lookupValue : randomChoice(statusValues).lookupValue;
    }

    return record;
  });
