import { generateRecord, randomChoice, randomDecimal, randomInt } from './field-generator.js';
import type { ResoField, ResoLookup } from './types.js';

const STREET_NAMES = [
  'Main',
  'Oak',
  'Maple',
  'Cedar',
  'Elm',
  'Pine',
  'Washington',
  'Park',
  'Lake',
  'Hill',
  'Sunset',
  'River',
  'Spring',
  'Valley',
  'Highland',
  'Forest',
  'Meadow',
  'Willow',
  'Birch',
  'Cherry',
  'Walnut',
  'Magnolia'
];

const STREET_SUFFIXES = ['St', 'Ave', 'Blvd', 'Dr', 'Ln', 'Ct', 'Pl', 'Way', 'Rd', 'Cir'];

const CITY_NAMES = [
  'Springfield',
  'Fairview',
  'Madison',
  'Georgetown',
  'Arlington',
  'Salem',
  'Franklin',
  'Clinton',
  'Greenville',
  'Bristol',
  'Manchester',
  'Oxford',
  'Burlington',
  'Ashland',
  'Centerville'
];

const PROPERTY_TYPES = ['Residential', 'Commercial', 'Land', 'Farm'];

const PROPERTY_SUBTYPES = ['SingleFamilyResidence', 'Condominium', 'Townhouse', 'Apartment', 'ManufacturedHome', 'MultiFamily'];

/** Generates realistic Property records with domain-specific overrides. */
export const generatePropertyRecords = (
  fields: ReadonlyArray<ResoField>,
  lookups: Readonly<Record<string, ReadonlyArray<ResoLookup>>>,
  count: number
): ReadonlyArray<Record<string, unknown>> =>
  Array.from({ length: count }, (_, i) => {
    const record = generateRecord(fields, lookups, i);

    // Address overrides
    record.StreetNumber = String(randomInt(100, 9999));
    record.StreetName = randomChoice(STREET_NAMES);
    record.StreetSuffix = randomChoice(STREET_SUFFIXES);
    record.UnparsedAddress = `${record.StreetNumber} ${record.StreetName} ${record.StreetSuffix}`;
    record.City = randomChoice(CITY_NAMES);
    record.PostalCode = String(randomInt(10000, 99999));

    // Pricing — ListPrice >= ListPriceLow
    record.ListPrice = randomDecimal(50000, 10000000, 2);
    record.ListPriceLow = randomDecimal((record.ListPrice as number) * 0.8, record.ListPrice as number, 2);
    record.OriginalListPrice = record.ListPrice;

    // Bedrooms
    record.BedroomsTotal = randomInt(1, 6);

    // Bathrooms — generate parts first, then compute total
    record.BathroomsFull = randomInt(1, 4);
    record.BathroomsHalf = randomInt(0, 2);
    record.BathroomsPartial = Math.random() > 0.7 ? randomInt(0, 1) : 0;
    record.BathroomsOneQuarter = 0;
    record.BathroomsThreeQuarter = Math.random() > 0.8 ? randomInt(0, 1) : 0;
    record.BathroomsTotalInteger =
      (record.BathroomsFull as number) +
      (record.BathroomsHalf as number) +
      (record.BathroomsPartial as number) +
      (record.BathroomsOneQuarter as number) +
      (record.BathroomsThreeQuarter as number);
    record.LivingArea = randomDecimal(500, 8000, 2);
    record.LotSizeSquareFeet = randomDecimal(2000, 50000, 2);
    record.YearBuilt = randomInt(1950, 2024);

    // Geo coordinates (continental US bounds)
    record.Latitude = randomDecimal(25.0, 48.0, 6);
    record.Longitude = randomDecimal(-124.0, -71.0, 6);

    // Property type
    record.PropertyType = randomChoice(PROPERTY_TYPES);
    record.PropertySubType = randomChoice(PROPERTY_SUBTYPES);

    // Status — prefer Active for most generated listings
    const statusValues = lookups['org.reso.metadata.enums.StandardStatus'];
    if (statusValues && statusValues.length > 0) {
      const activeStatus = statusValues.find(s => s.lookupValue === 'Active');
      record.StandardStatus = activeStatus
        ? Math.random() > 0.3
          ? 'Active'
          : randomChoice(statusValues).lookupValue
        : randomChoice(statusValues).lookupValue;
    }

    // Dates
    const listDate = new Date(Date.now() - randomInt(1, 365) * 86400000);
    record.ListingContractDate = listDate.toISOString().split('T')[0];
    record.OnMarketDate = record.ListingContractDate;

    // Text fields
    record.PublicRemarks = `Beautiful ${record.BedroomsTotal}-bedroom home located at ${record.UnparsedAddress}, ${record.City}. Features ${record.LivingArea} sqft of living space built in ${record.YearBuilt}.`;

    return record;
  });
