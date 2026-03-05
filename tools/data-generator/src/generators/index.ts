import { generateRecords } from './field-generator.js';
import { generateMediaRecords } from './media.js';
import { generateMemberRecords } from './member.js';
import { generateOfficeRecords } from './office.js';
import { generateOpenHouseRecords } from './open-house.js';
import { generatePropertyChildRecords } from './property-child.js';
import { generatePropertyRecords } from './property.js';
import { generateShowingRecords } from './showing.js';
import type { RecordGenerator, ResoField, ResoLookup } from './types.js';

export type {
  AuthConfig,
  BackFillPhase,
  ForeignKeyBinding,
  GeneratorConfig,
  MultiResourceSeedPlan,
  RecordGenerator,
  ResourceDependency,
  ResoAnnotation,
  ResoField,
  ResoLookup,
  SeedOptions,
  SeedPhase,
  SeedPlan,
  SeedResult
} from './types.js';
export { KEY_FIELD_MAP } from './types.js';
export {
  generateFieldValue,
  generateRecord,
  generateRecords,
  isEnumType,
  randomChoice,
  randomDecimal,
  randomInt
} from './field-generator.js';

/** Registry of resource-specific generators. Falls back to generic generator. */
const GENERATORS: Readonly<Record<string, RecordGenerator>> = {
  Property: (fields, lookups, count) => generatePropertyRecords(fields, lookups, count),
  Member: (fields, lookups, count) => generateMemberRecords(fields, lookups, count),
  Office: (fields, lookups, count) => generateOfficeRecords(fields, lookups, count),
  Media: (fields, lookups, count, parentResource, parentKey) => generateMediaRecords(fields, lookups, count, parentResource, parentKey),
  OpenHouse: (fields, lookups, count, parentResource, parentKey) =>
    generateOpenHouseRecords(fields, lookups, count, parentResource, parentKey),
  Showing: (fields, lookups, count, parentResource, parentKey) => generateShowingRecords(fields, lookups, count, parentResource, parentKey),
  PropertyRooms: (fields, lookups, count, parentResource, parentKey) =>
    generatePropertyChildRecords(fields, lookups, count, parentResource, parentKey),
  PropertyGreenVerification: (fields, lookups, count, parentResource, parentKey) =>
    generatePropertyChildRecords(fields, lookups, count, parentResource, parentKey),
  PropertyPowerProduction: (fields, lookups, count, parentResource, parentKey) =>
    generatePropertyChildRecords(fields, lookups, count, parentResource, parentKey),
  PropertyUnitTypes: (fields, lookups, count, parentResource, parentKey) =>
    generatePropertyChildRecords(fields, lookups, count, parentResource, parentKey)
};

/**
 * Returns the record generator for a given resource.
 * Falls back to the generic field-based generator for unknown resources.
 */
export const getGenerator = (resource: string): RecordGenerator =>
  GENERATORS[resource] ??
  ((fields: ReadonlyArray<ResoField>, lookups: Readonly<Record<string, ReadonlyArray<ResoLookup>>>, count: number) =>
    generateRecords(fields, lookups, count));
