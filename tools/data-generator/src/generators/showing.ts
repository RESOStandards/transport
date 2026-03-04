import { generateRecord, randomChoice, randomInt } from './field-generator.js';
import type { ResoField, ResoLookup } from './types.js';

const SHOWING_INSTRUCTIONS = [
  'Call listing agent 24 hours in advance.',
  'Use lockbox — code provided upon confirmed appointment.',
  'Contact showing service to schedule.',
  'Accompanied showings only — call agent.',
  'Go and show — lockbox on front door.'
];

/**
 * Generates realistic Showing records linked to a parent resource.
 * Sets ResourceName and ResourceRecordKey for the RESO FK convention.
 */
export const generateShowingRecords = (
  fields: ReadonlyArray<ResoField>,
  lookups: Readonly<Record<string, ReadonlyArray<ResoLookup>>>,
  count: number,
  parentResource?: string,
  parentKey?: string
): ReadonlyArray<Record<string, unknown>> =>
  Array.from({ length: count }, (_, i) => {
    const record = generateRecord(fields, lookups, i);

    // Link to parent via RESO FK convention
    if (parentResource) record.ResourceName = parentResource;
    if (parentKey) record.ResourceRecordKey = parentKey;
    if (parentKey) record.ListingKey = parentKey;

    // Schedule showings in the near future
    const daysAhead = randomInt(1, 30);
    const showingDate = new Date(Date.now() + daysAhead * 86400000);
    const hour = randomChoice([9, 10, 11, 13, 14, 15, 16]);
    showingDate.setHours(hour, 0, 0, 0);

    const endDate = new Date(showingDate);
    endDate.setHours(hour + 1);

    record.ShowingDate = showingDate.toISOString().split('T')[0];
    record.ShowingStartTime = showingDate.toISOString();
    record.ShowingEndTime = endDate.toISOString();

    // Status
    const statusValues = lookups['org.reso.metadata.enums.ShowingStatus'];
    if (statusValues && statusValues.length > 0) {
      const active = statusValues.find(s => s.lookupValue === 'Active' || s.lookupValue === 'Confirmed');
      record.ShowingStatus = active ? active.lookupValue : randomChoice(statusValues).lookupValue;
    }

    record.ShowingInstructions = randomChoice(SHOWING_INSTRUCTIONS);

    return record;
  });
