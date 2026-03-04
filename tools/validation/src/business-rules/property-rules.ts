import type { CrossFieldRule, FieldRule } from './types.js';

/** Maximum allowed value for price fields. */
const MAX_PRICE = 1_000_000_000;

/** Maximum allowed value for bedroom/bathroom count fields. */
const MAX_ROOM_COUNT = 100;

/** Maximum allowed value for expense, fee, and amount fields. */
const MAX_EXPENSE = 10_000;

const priceRule = (fieldName: string): FieldRule => ({
  fieldName,
  min: 0.01,
  max: MAX_PRICE
});

const roomCountRule = (fieldName: string): FieldRule => ({
  fieldName,
  min: 0,
  max: MAX_ROOM_COUNT
});

const requiredRule = (fieldName: string): FieldRule => ({
  fieldName,
  required: true
});

/**
 * Pattern matching expense, fee, and amount fields by name suffix.
 * Matches: CableTvExpense, AssociationFee, AssociationFee2, TaxAnnualAmount, etc.
 * Does NOT match: LotSizeSquareFeet (ends with "Feet", not "Fee").
 */
const EXPENSE_FEE_AMOUNT_PATTERN = /(?:Expense|Amount|Fee\d?)$/;

/** Per-field rules for the Property resource. */
export const PROPERTY_RULES: ReadonlyArray<FieldRule> = [
  // Required address fields
  requiredRule('City'),
  requiredRule('StateOrProvince'),
  requiredRule('PostalCode'),
  requiredRule('Country'),

  // Price fields (must be > $0)
  priceRule('ListPrice'),
  priceRule('OriginalListPrice'),
  priceRule('PreviousListPrice'),
  priceRule('ClosePrice'),
  priceRule('ListPriceLow'),

  // Expense, fee, and amount fields (pattern-matched)
  {
    fieldName: 'expenses/fees/amounts',
    fieldPattern: EXPENSE_FEE_AMOUNT_PATTERN,
    min: 0,
    max: MAX_EXPENSE
  },

  // Bedroom fields
  roomCountRule('BedroomsTotal'),
  roomCountRule('BedroomsPossible'),
  roomCountRule('MainLevelBedrooms'),

  // Bathroom fields
  roomCountRule('BathroomsTotalInteger'),
  roomCountRule('BathroomsFull'),
  roomCountRule('BathroomsHalf'),
  roomCountRule('BathroomsOneQuarter'),
  roomCountRule('BathroomsPartial'),
  roomCountRule('BathroomsThreeQuarter'),
  roomCountRule('MainLevelBathrooms')
];

// --- Cross-field helpers ---

/** Extracts a numeric value from a record field, or returns null if absent/non-numeric. */
const numericField = (body: Readonly<Record<string, unknown>>, field: string): number | null => {
  const v = body[field];
  return typeof v === 'number' ? v : null;
};

/** Bathroom part fields that should sum to BathroomsTotalInteger. */
const BATHROOM_PARTS = ['BathroomsFull', 'BathroomsHalf', 'BathroomsPartial', 'BathroomsOneQuarter', 'BathroomsThreeQuarter'] as const;

/** Cross-field rules for the Property resource. */
export const PROPERTY_CROSS_RULES: ReadonlyArray<CrossFieldRule> = [
  {
    name: 'ListPrice >= ListPriceLow',
    validate: body => {
      const listPrice = numericField(body, 'ListPrice');
      const listPriceLow = numericField(body, 'ListPriceLow');
      if (listPrice === null || listPriceLow === null) return null;
      if (listPrice < listPriceLow) {
        return {
          field: 'ListPrice',
          reason: `ListPrice ($${listPrice.toLocaleString('en-US')}) must be greater than or equal to ListPriceLow ($${listPriceLow.toLocaleString('en-US')}).`
        };
      }
      return null;
    }
  },
  {
    name: 'BathroomsTotalInteger = sum of parts',
    validate: body => {
      const total = numericField(body, 'BathroomsTotalInteger');
      if (total === null) return null;

      let sum = 0;
      let hasParts = false;
      for (const part of BATHROOM_PARTS) {
        const v = numericField(body, part);
        if (v !== null) {
          sum += v;
          hasParts = true;
        }
      }
      if (!hasParts) return null;

      if (total !== sum) {
        return {
          field: 'BathroomsTotalInteger',
          reason: `BathroomsTotalInteger (${total}) must equal the sum of bathroom parts (${sum}).`
        };
      }
      return null;
    }
  }
];
