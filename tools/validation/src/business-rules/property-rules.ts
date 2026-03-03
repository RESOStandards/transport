import type { FieldRule } from './types.js';

/** Maximum allowed value for price fields. */
const MAX_PRICE = 1_000_000_000;

/** Maximum allowed value for bedroom/bathroom count fields. */
const MAX_ROOM_COUNT = 100;

const priceRule = (fieldName: string): FieldRule => ({
  fieldName,
  min: 0,
  max: MAX_PRICE
});

const roomCountRule = (fieldName: string): FieldRule => ({
  fieldName,
  min: 0,
  max: MAX_ROOM_COUNT
});

/** Business rules for the Property resource. */
export const PROPERTY_RULES: ReadonlyArray<FieldRule> = [
  // Price fields
  priceRule('ListPrice'),
  priceRule('OriginalListPrice'),
  priceRule('PreviousListPrice'),
  priceRule('ClosePrice'),
  priceRule('ListPriceLow'),

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
