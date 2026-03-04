import type { GeneratorConfig, ResoField, SeedPlan } from './generators/types.js';

/** Resources that can be child (related) resources of a parent resource. */
const CHILD_RESOURCES = new Set([
  'Media',
  'OpenHouse',
  'Showing',
  'PropertyRooms',
  'PropertyGreenVerification',
  'PropertyPowerProduction',
  'PropertyUnitTypes'
]);

/** Default related record counts by resource. */
const DEFAULT_RELATED_COUNTS: Readonly<Record<string, number>> = {
  Media: 5,
  OpenHouse: 2,
  Showing: 2,
  PropertyRooms: 3,
  PropertyGreenVerification: 1,
  PropertyPowerProduction: 1,
  PropertyUnitTypes: 2
};

/**
 * Determines which resources can be related to the given parent resource.
 *
 * A resource is a valid related resource if it either:
 * - Has ResourceName + ResourceRecordKey fields (polymorphic FK convention), or
 * - Has the parent's key field (e.g. ListingKey for Property children)
 */
export const getRelatedResources = (
  parentResource: string,
  fieldsByResource: Readonly<Record<string, ReadonlyArray<ResoField>>>
): ReadonlyArray<string> => {
  const parentKeyField = PARENT_KEY_FIELDS[parentResource];
  const related: string[] = [];

  for (const [resource, fields] of Object.entries(fieldsByResource)) {
    if (resource === parentResource) continue;
    if (!CHILD_RESOURCES.has(resource)) continue;

    const hasResourceRecordKey = fields.some(f => f.fieldName === 'ResourceName') && fields.some(f => f.fieldName === 'ResourceRecordKey');
    const hasParentKey = parentKeyField ? fields.some(f => f.fieldName === parentKeyField) : false;

    if (hasResourceRecordKey || hasParentKey) {
      related.push(resource);
    }
  }

  return related;
};

/** Maps parent resources to their key field for child resource discovery. */
const PARENT_KEY_FIELDS: Readonly<Record<string, string>> = {
  Property: 'ListingKey'
};

/** Returns the default count for a related resource. */
export const getDefaultRelatedCount = (resource: string): number => DEFAULT_RELATED_COUNTS[resource] ?? 2;

/**
 * Builds a SeedPlan from user configuration.
 * Creates the parent resource first, then related records.
 */
export const buildSeedPlan = (config: GeneratorConfig): SeedPlan => {
  const resources: GeneratorConfig[] = [];

  // Parent resource (without related records — those are handled separately per parent)
  resources.push({
    resource: config.resource,
    count: config.count,
    relatedRecords: config.relatedRecords
  });

  return { resources };
};

/** Checks whether a resource is a child resource (linked to a parent via FK). */
export const isChildResource = (resource: string): boolean => CHILD_RESOURCES.has(resource);
