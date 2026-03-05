import { generateRecord, randomChoice, randomDecimal, randomInt } from './field-generator.js';
import type { ResoField, ResoLookup } from './types.js';

/**
 * Effective property tax rates by US state (2024 data).
 * Source: Tax Foundation / American Community Survey.
 * Rates expressed as decimal fractions (e.g., 0.0179 = 1.79%).
 */
const STATE_TAX_RATES: Readonly<Record<string, number>> = {
  AL: 0.0037,
  AZ: 0.0043,
  AR: 0.0054,
  CA: 0.0069,
  CO: 0.0052,
  CT: 0.0136,
  DE: 0.0051,
  FL: 0.0076,
  GA: 0.0077,
  HI: 0.0031,
  ID: 0.0043,
  IL: 0.0179,
  IN: 0.0076,
  IA: 0.0125,
  KS: 0.012,
  KY: 0.0072,
  LA: 0.0056,
  ME: 0.009,
  MD: 0.009,
  MA: 0.0095,
  MI: 0.0113,
  MN: 0.0099,
  MS: 0.0054,
  MO: 0.0085,
  MT: 0.0059,
  NE: 0.0138,
  NV: 0.005,
  NH: 0.0135,
  NJ: 0.0168,
  NM: 0.0061,
  NY: 0.0123,
  NC: 0.0062,
  ND: 0.0094,
  OH: 0.0128,
  OK: 0.0078,
  OR: 0.0079,
  PA: 0.0114,
  RI: 0.01,
  SC: 0.0044,
  SD: 0.01,
  TN: 0.0046,
  TX: 0.0125,
  UT: 0.0045,
  VT: 0.014,
  VA: 0.0075,
  WA: 0.0074,
  WV: 0.0048,
  WI: 0.0119,
  WY: 0.0058
};

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

const US_STATES = [
  'AL',
  'AZ',
  'CA',
  'CO',
  'CT',
  'FL',
  'GA',
  'IL',
  'MA',
  'MD',
  'MI',
  'MN',
  'NC',
  'NJ',
  'NY',
  'OH',
  'OR',
  'PA',
  'RI',
  'TX',
  'VA',
  'WA'
];

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
    const streetSuffixValues = lookups['org.reso.metadata.enums.StreetSuffix'];
    record.StreetSuffix = streetSuffixValues?.length ? randomChoice(streetSuffixValues).lookupValue : randomChoice(STREET_SUFFIXES);
    record.UnparsedAddress = `${record.StreetNumber} ${record.StreetName} ${record.StreetSuffix}`;
    const cityValues = lookups['org.reso.metadata.enums.City'];
    record.City = cityValues?.length ? randomChoice(cityValues).lookupValue : randomChoice(CITY_NAMES);
    record.StateOrProvince = randomChoice(US_STATES);
    record.PostalCode = String(randomInt(10000, 99999));
    record.Country = 'US';

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

    // Property type — prefer lookup values, fall back to hardcoded
    const propertyTypeValues = lookups['org.reso.metadata.enums.PropertyType'];
    record.PropertyType = propertyTypeValues?.length ? randomChoice(propertyTypeValues).lookupValue : randomChoice(PROPERTY_TYPES);
    const propertySubTypeValues = lookups['org.reso.metadata.enums.PropertySubType'];
    record.PropertySubType = propertySubTypeValues?.length
      ? randomChoice(propertySubTypeValues).lookupValue
      : randomChoice(PROPERTY_SUBTYPES);

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

    // Taxes — calculated from ListPrice × state effective rate
    const taxRate = STATE_TAX_RATES[record.StateOrProvince as string] ?? 0.01;
    const listPrice = record.ListPrice as number;
    record.TaxAnnualAmount = randomDecimal(listPrice * taxRate * 0.9, listPrice * taxRate * 1.1, 2);
    record.TaxAssessedValue = Math.round(randomDecimal(listPrice * 0.7, listPrice * 0.95, 0));
    record.TaxYear = new Date().getFullYear() - randomInt(0, 1);

    // Expense fields (realistic monthly amounts)
    record.AssociationFee = randomDecimal(50, 800, 2);
    record.AssociationFee2 = Math.random() > 0.7 ? randomDecimal(25, 200, 2) : 0;
    record.InsuranceExpense = randomDecimal(50, 500, 2);
    record.ElectricExpense = randomDecimal(50, 400, 2);
    record.WaterSewerExpense = randomDecimal(20, 150, 2);
    record.TrashExpense = randomDecimal(10, 75, 2);
    record.CableTvExpense = randomDecimal(30, 200, 2);
    record.MaintenanceExpense = randomDecimal(50, 500, 2);
    record.OperatingExpense = randomDecimal(100, 2000, 2);
    record.OtherExpense = randomDecimal(0, 300, 2);

    // Dates
    const listDate = new Date(Date.now() - randomInt(1, 365) * 86400000);
    record.ListingContractDate = listDate.toISOString().split('T')[0];
    record.OnMarketDate = record.ListingContractDate;

    // Text fields
    record.PublicRemarks = `Beautiful ${record.BedroomsTotal}-bedroom home located at ${record.UnparsedAddress}, ${record.City}. Features ${record.LivingArea} sqft of living space built in ${record.YearBuilt}.`;

    return record;
  });
