import { generateRecord, randomChoice, randomInt } from './field-generator.js';
import type { ResoField, ResoLookup } from './types.js';

const MEDIA_DESCRIPTIONS = [
  'Front exterior view',
  'Living room',
  'Kitchen',
  'Master bedroom',
  'Backyard',
  'Bathroom',
  'Dining room',
  'Garage',
  'Pool area',
  'Aerial view',
  'Street view',
  'Office space',
  'Family room',
  'Patio',
  'Laundry room',
  'Basement',
  'Attic',
  'Garden'
];

/**
 * Generates realistic Media records linked to a parent resource.
 * Sets ResourceName and ResourceRecordKey for the RESO FK convention.
 */
export const generateMediaRecords = (
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

    // Media-specific fields
    record.MediaURL = `https://picsum.photos/seed/${parentKey ?? 'media'}-${i}/800/600`;
    record.ShortDescription = randomChoice(MEDIA_DESCRIPTIONS);
    record.LongDescription = `${record.ShortDescription} of the property`;
    record.Order = i + 1;
    record.MediaObjectID = `IMG-${String(randomInt(100000, 999999))}`;

    // Media category — prefer Photo
    const categoryValues = lookups['org.reso.metadata.enums.MediaCategory'];
    if (categoryValues && categoryValues.length > 0) {
      const photo = categoryValues.find(c => c.lookupValue === 'Photo');
      // First image is always a Photo, rest can be mixed
      record.MediaCategory = i === 0 && photo ? 'Photo' : randomChoice(categoryValues).lookupValue;
    }

    // Media type
    record.MimeType = 'image/jpeg';

    return record;
  });
