import { generateRecord, randomChoice, randomInt } from './field-generator.js';
import type { ResoField, ResoLookup } from './types.js';

const OPEN_HOUSE_REMARKS = [
  'Come see this beautiful home! Refreshments provided.',
  'Open house this weekend. All welcome!',
  'Tour this stunning property at your convenience.',
  'Drop by and explore this wonderful listing.',
  'Join us for an exclusive open house event.'
];

/**
 * Generates realistic OpenHouse records linked to a parent resource.
 * Sets ResourceName and ResourceRecordKey for the RESO FK convention.
 */
export const generateOpenHouseRecords = (
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

    // Schedule open houses in the near future
    const daysAhead = randomInt(1, 60);
    const startDate = new Date(Date.now() + daysAhead * 86400000);
    // Weekend preference — shift to Saturday if weekday
    const dayOfWeek = startDate.getDay();
    if (dayOfWeek > 0 && dayOfWeek < 6) {
      startDate.setDate(startDate.getDate() + (6 - dayOfWeek));
    }

    const startHour = randomChoice([10, 11, 12, 13, 14]);
    startDate.setHours(startHour, 0, 0, 0);

    const endDate = new Date(startDate);
    endDate.setHours(startHour + randomChoice([2, 3, 4]));

    record.OpenHouseDate = startDate.toISOString().split('T')[0];
    record.OpenHouseStartTime = startDate.toISOString();
    record.OpenHouseEndTime = endDate.toISOString();

    // Status
    const statusValues = lookups['org.reso.metadata.enums.OpenHouseStatus'];
    if (statusValues && statusValues.length > 0) {
      const active = statusValues.find(s => s.lookupValue === 'Active');
      record.OpenHouseStatus = active ? 'Active' : randomChoice(statusValues).lookupValue;
    }

    // Type
    const typeValues = lookups['org.reso.metadata.enums.OpenHouseType'];
    if (typeValues && typeValues.length > 0) {
      record.OpenHouseType = randomChoice(typeValues).lookupValue;
    }

    record.OpenHouseRemarks = randomChoice(OPEN_HOUSE_REMARKS);

    return record;
  });
