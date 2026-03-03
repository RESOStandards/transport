import { describe, expect, it } from 'vitest';
import { getGenerator } from '../src/generators/index.js';
import { generateMediaRecords } from '../src/generators/media.js';
import { generateMemberRecords } from '../src/generators/member.js';
import { generateOfficeRecords } from '../src/generators/office.js';
import { generateOpenHouseRecords } from '../src/generators/open-house.js';
import { generatePropertyRecords } from '../src/generators/property.js';
import { generateShowingRecords } from '../src/generators/showing.js';
import type { ResoField, ResoLookup } from '../src/generators/types.js';

const makeField = (fieldName: string, type = 'Edm.String', overrides: Partial<ResoField> = {}): ResoField => ({
  resourceName: 'Property',
  fieldName,
  type,
  nullable: true,
  annotations: [],
  ...overrides
});

const PROPERTY_FIELDS: ResoField[] = [
  makeField('ListingKey'),
  makeField('ListPrice', 'Edm.Decimal', { scale: 2, precision: 14 }),
  makeField('StreetNumber'),
  makeField('StreetName', 'Edm.String', { maxLength: 50 }),
  makeField('City', 'Edm.String'),
  makeField('PostalCode', 'Edm.String', { maxLength: 10 }),
  makeField('BedroomsTotal', 'Edm.Int64'),
  makeField('BathroomsTotalInteger', 'Edm.Int64'),
  makeField('BathroomsFull', 'Edm.Int64'),
  makeField('BathroomsHalf', 'Edm.Int64'),
  makeField('LivingArea', 'Edm.Decimal', { scale: 2 }),
  makeField('LotSizeSquareFeet', 'Edm.Decimal', { scale: 2 }),
  makeField('YearBuilt', 'Edm.Int32'),
  makeField('Latitude', 'Edm.Decimal', { scale: 6, precision: 12 }),
  makeField('Longitude', 'Edm.Decimal', { scale: 6, precision: 12 }),
  makeField('StandardStatus', 'org.reso.metadata.enums.StandardStatus'),
  makeField('PropertyType', 'org.reso.metadata.enums.PropertyType'),
  makeField('PublicRemarks'),
  makeField('ListingContractDate', 'Edm.Date'),
  makeField('ModificationTimestamp', 'Edm.DateTimeOffset')
];

const MEMBER_FIELDS: ResoField[] = [
  makeField('MemberKey', 'Edm.String', { resourceName: 'Member' }),
  makeField('MemberFirstName', 'Edm.String', { resourceName: 'Member', maxLength: 50 }),
  makeField('MemberLastName', 'Edm.String', { resourceName: 'Member', maxLength: 50 }),
  makeField('MemberEmail', 'Edm.String', { resourceName: 'Member', maxLength: 80 }),
  makeField('MemberPreferredPhone', 'Edm.String', { resourceName: 'Member' }),
  makeField('MemberNationalAssociationId', 'Edm.String', { resourceName: 'Member' })
];

const OFFICE_FIELDS: ResoField[] = [
  makeField('OfficeKey', 'Edm.String', { resourceName: 'Office' }),
  makeField('OfficeName', 'Edm.String', { resourceName: 'Office', maxLength: 255 }),
  makeField('OfficePhone', 'Edm.String', { resourceName: 'Office' }),
  makeField('OfficeEmail', 'Edm.String', { resourceName: 'Office' }),
  makeField('OfficeAddress1', 'Edm.String', { resourceName: 'Office' }),
  makeField('OfficeCity', 'Edm.String', { resourceName: 'Office' }),
  makeField('OfficePostalCode', 'Edm.String', { resourceName: 'Office' })
];

const MEDIA_FIELDS: ResoField[] = [
  makeField('MediaKey', 'Edm.String', { resourceName: 'Media' }),
  makeField('MediaURL', 'Edm.String', { resourceName: 'Media', maxLength: 8000 }),
  makeField('ShortDescription', 'Edm.String', { resourceName: 'Media' }),
  makeField('Order', 'Edm.Int32', { resourceName: 'Media' }),
  makeField('MediaCategory', 'org.reso.metadata.enums.MediaCategory', { resourceName: 'Media' }),
  makeField('MimeType', 'Edm.String', { resourceName: 'Media' }),
  makeField('ResourceName', 'org.reso.metadata.enums.ResourceName', { resourceName: 'Media' }),
  makeField('ResourceRecordKey', 'Edm.String', { resourceName: 'Media' })
];

const OPEN_HOUSE_FIELDS: ResoField[] = [
  makeField('OpenHouseKey', 'Edm.String', { resourceName: 'OpenHouse' }),
  makeField('OpenHouseDate', 'Edm.Date', { resourceName: 'OpenHouse' }),
  makeField('OpenHouseStartTime', 'Edm.TimeOfDay', { resourceName: 'OpenHouse' }),
  makeField('OpenHouseEndTime', 'Edm.TimeOfDay', { resourceName: 'OpenHouse' }),
  makeField('OpenHouseRemarks', 'Edm.String', { resourceName: 'OpenHouse' }),
  makeField('ResourceName', 'org.reso.metadata.enums.ResourceName', { resourceName: 'OpenHouse' }),
  makeField('ResourceRecordKey', 'Edm.String', { resourceName: 'OpenHouse' })
];

const SHOWING_FIELDS: ResoField[] = [
  makeField('ShowingKey', 'Edm.String', { resourceName: 'Showing' }),
  makeField('ShowingDate', 'Edm.Date', { resourceName: 'Showing' }),
  makeField('ShowingStartTime', 'Edm.TimeOfDay', { resourceName: 'Showing' }),
  makeField('ShowingEndTime', 'Edm.TimeOfDay', { resourceName: 'Showing' }),
  makeField('ShowingInstructions', 'Edm.String', { resourceName: 'Showing' }),
  makeField('ResourceName', 'org.reso.metadata.enums.ResourceName', { resourceName: 'Showing' }),
  makeField('ResourceRecordKey', 'Edm.String', { resourceName: 'Showing' })
];

const SAMPLE_LOOKUPS: Record<string, ReadonlyArray<ResoLookup>> = {
  'org.reso.metadata.enums.StandardStatus': [
    { lookupName: 'org.reso.metadata.enums.StandardStatus', lookupValue: 'Active', type: 'Edm.Int32', annotations: [] },
    { lookupName: 'org.reso.metadata.enums.StandardStatus', lookupValue: 'Pending', type: 'Edm.Int32', annotations: [] }
  ],
  'org.reso.metadata.enums.MediaCategory': [
    { lookupName: 'org.reso.metadata.enums.MediaCategory', lookupValue: 'Photo', type: 'Edm.Int32', annotations: [] },
    { lookupName: 'org.reso.metadata.enums.MediaCategory', lookupValue: 'Video', type: 'Edm.Int32', annotations: [] }
  ]
};

describe('generatePropertyRecords', () => {
  it('generates the requested number of records', () => {
    const records = generatePropertyRecords(PROPERTY_FIELDS, SAMPLE_LOOKUPS, 3);
    expect(records).toHaveLength(3);
  });

  it('generates realistic addresses', () => {
    const records = generatePropertyRecords(PROPERTY_FIELDS, SAMPLE_LOOKUPS, 1);
    const record = records[0];
    expect(typeof record.StreetNumber).toBe('string');
    expect(typeof record.StreetName).toBe('string');
    expect(typeof record.UnparsedAddress).toBe('string');
    expect(typeof record.City).toBe('string');
    expect(typeof record.PostalCode).toBe('string');
  });

  it('generates realistic pricing', () => {
    const records = generatePropertyRecords(PROPERTY_FIELDS, SAMPLE_LOOKUPS, 1);
    const price = records[0].ListPrice as number;
    expect(price).toBeGreaterThanOrEqual(50000);
    expect(price).toBeLessThanOrEqual(10000000);
  });

  it('generates realistic property characteristics', () => {
    const records = generatePropertyRecords(PROPERTY_FIELDS, SAMPLE_LOOKUPS, 1);
    const record = records[0];
    expect(record.BedroomsTotal).toBeGreaterThanOrEqual(1);
    expect(record.BedroomsTotal).toBeLessThanOrEqual(6);
    expect(record.BathroomsTotalInteger).toBeGreaterThanOrEqual(1);
    expect(record.LivingArea as number).toBeGreaterThanOrEqual(500);
    expect(record.YearBuilt as number).toBeGreaterThanOrEqual(1950);
  });

  it('generates coordinates within US bounds', () => {
    const records = generatePropertyRecords(PROPERTY_FIELDS, SAMPLE_LOOKUPS, 1);
    const record = records[0];
    expect(record.Latitude as number).toBeGreaterThanOrEqual(25);
    expect(record.Latitude as number).toBeLessThanOrEqual(48);
    expect(record.Longitude as number).toBeGreaterThanOrEqual(-124);
    expect(record.Longitude as number).toBeLessThanOrEqual(-71);
  });

  it('skips ListingKey (server-generated)', () => {
    const records = generatePropertyRecords(PROPERTY_FIELDS, SAMPLE_LOOKUPS, 1);
    expect(records[0].ListingKey).toBeUndefined();
  });

  it('skips ModificationTimestamp (server-computed)', () => {
    const records = generatePropertyRecords(PROPERTY_FIELDS, SAMPLE_LOOKUPS, 1);
    expect(records[0].ModificationTimestamp).toBeUndefined();
  });

  it('includes PublicRemarks', () => {
    const records = generatePropertyRecords(PROPERTY_FIELDS, SAMPLE_LOOKUPS, 1);
    expect(typeof records[0].PublicRemarks).toBe('string');
    expect((records[0].PublicRemarks as string).length).toBeGreaterThan(0);
  });
});

describe('generateMemberRecords', () => {
  it('generates the requested number of records', () => {
    const records = generateMemberRecords(MEMBER_FIELDS, {}, 5);
    expect(records).toHaveLength(5);
  });

  it('generates realistic names', () => {
    const records = generateMemberRecords(MEMBER_FIELDS, {}, 1);
    const record = records[0];
    expect(typeof record.MemberFirstName).toBe('string');
    expect(typeof record.MemberLastName).toBe('string');
    expect(typeof record.MemberFullName).toBe('string');
    expect(record.MemberFullName as string).toContain(record.MemberFirstName as string);
  });

  it('generates valid email format', () => {
    const records = generateMemberRecords(MEMBER_FIELDS, {}, 1);
    expect(records[0].MemberEmail as string).toContain('@');
  });

  it('generates phone numbers', () => {
    const records = generateMemberRecords(MEMBER_FIELDS, {}, 1);
    expect(typeof records[0].MemberPreferredPhone).toBe('string');
    expect(records[0].MemberPreferredPhone as string).toMatch(/\d{3}-\d{3}-\d{4}/);
  });
});

describe('generateOfficeRecords', () => {
  it('generates the requested number of records', () => {
    const records = generateOfficeRecords(OFFICE_FIELDS, {}, 3);
    expect(records).toHaveLength(3);
  });

  it('generates office names', () => {
    const records = generateOfficeRecords(OFFICE_FIELDS, {}, 1);
    expect(typeof records[0].OfficeName).toBe('string');
    expect((records[0].OfficeName as string).length).toBeGreaterThan(0);
  });

  it('generates office phone and email', () => {
    const records = generateOfficeRecords(OFFICE_FIELDS, {}, 1);
    expect(typeof records[0].OfficePhone).toBe('string');
    expect(records[0].OfficeEmail as string).toContain('@');
  });
});

describe('generateMediaRecords', () => {
  it('generates the requested number of records', () => {
    const records = generateMediaRecords(MEDIA_FIELDS, SAMPLE_LOOKUPS, 5);
    expect(records).toHaveLength(5);
  });

  it('sets ResourceName and ResourceRecordKey for parent linkage', () => {
    const records = generateMediaRecords(MEDIA_FIELDS, SAMPLE_LOOKUPS, 2, 'Property', 'abc-123');
    for (const record of records) {
      expect(record.ResourceName).toBe('Property');
      expect(record.ResourceRecordKey).toBe('abc-123');
    }
  });

  it('generates sequential Order values', () => {
    const records = generateMediaRecords(MEDIA_FIELDS, SAMPLE_LOOKUPS, 3);
    expect(records[0].Order).toBe(1);
    expect(records[1].Order).toBe(2);
    expect(records[2].Order).toBe(3);
  });

  it('generates MediaURL values', () => {
    const records = generateMediaRecords(MEDIA_FIELDS, SAMPLE_LOOKUPS, 1);
    expect(typeof records[0].MediaURL).toBe('string');
    expect(records[0].MediaURL as string).toContain('https://');
  });

  it('sets MimeType', () => {
    const records = generateMediaRecords(MEDIA_FIELDS, SAMPLE_LOOKUPS, 1);
    expect(records[0].MimeType).toBe('image/jpeg');
  });
});

describe('generateOpenHouseRecords', () => {
  it('generates the requested number of records', () => {
    const records = generateOpenHouseRecords(OPEN_HOUSE_FIELDS, {}, 2);
    expect(records).toHaveLength(2);
  });

  it('sets ResourceName and ResourceRecordKey for parent linkage', () => {
    const records = generateOpenHouseRecords(OPEN_HOUSE_FIELDS, {}, 1, 'Property', 'prop-key-1');
    expect(records[0].ResourceName).toBe('Property');
    expect(records[0].ResourceRecordKey).toBe('prop-key-1');
  });

  it('generates future dates', () => {
    const records = generateOpenHouseRecords(OPEN_HOUSE_FIELDS, {}, 1);
    const date = new Date(records[0].OpenHouseDate as string);
    expect(date.getTime()).toBeGreaterThan(Date.now());
  });

  it('generates ISO 8601 datetime ranges', () => {
    const records = generateOpenHouseRecords(OPEN_HOUSE_FIELDS, {}, 1);
    expect(records[0].OpenHouseStartTime).toMatch(/^\d{4}-\d{2}-\d{2}T/);
    expect(records[0].OpenHouseEndTime).toMatch(/^\d{4}-\d{2}-\d{2}T/);
    // End time should be after start time
    const start = new Date(records[0].OpenHouseStartTime as string);
    const end = new Date(records[0].OpenHouseEndTime as string);
    expect(end.getTime()).toBeGreaterThan(start.getTime());
  });
});

describe('generateShowingRecords', () => {
  it('generates the requested number of records', () => {
    const records = generateShowingRecords(SHOWING_FIELDS, {}, 3);
    expect(records).toHaveLength(3);
  });

  it('sets ResourceName and ResourceRecordKey for parent linkage', () => {
    const records = generateShowingRecords(SHOWING_FIELDS, {}, 1, 'Property', 'prop-key-2');
    expect(records[0].ResourceName).toBe('Property');
    expect(records[0].ResourceRecordKey).toBe('prop-key-2');
  });

  it('generates showing instructions', () => {
    const records = generateShowingRecords(SHOWING_FIELDS, {}, 1);
    expect(typeof records[0].ShowingInstructions).toBe('string');
    expect((records[0].ShowingInstructions as string).length).toBeGreaterThan(0);
  });
});

describe('getGenerator', () => {
  it('returns Property generator for Property resource', () => {
    const gen = getGenerator('Property');
    const records = gen(PROPERTY_FIELDS, SAMPLE_LOOKUPS, 1);
    expect(records).toHaveLength(1);
    // Property generator sets StreetName
    expect(typeof records[0].StreetName).toBe('string');
  });

  it('returns generic generator for unknown resources', () => {
    const fields: ResoField[] = [
      makeField('CustomField', 'Edm.String', { resourceName: 'Custom', nullable: false }),
      makeField('CustomCount', 'Edm.Int32', { resourceName: 'Custom', nullable: false })
    ];
    const gen = getGenerator('CustomResource');
    const records = gen(fields, {}, 2);
    expect(records).toHaveLength(2);
    expect(records[0].CustomField).toBeDefined();
  });
});
