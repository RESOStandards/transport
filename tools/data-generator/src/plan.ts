import type { GeneratorConfig, ResoField, SeedPlan } from './generators/types.js';

/** Resources that can be child (related) resources via ResourceName/ResourceRecordKey FK convention. */
const CHILD_RESOURCES = new Set(['Media', 'OpenHouse', 'Showing']);

/** Default related record counts by resource. */
const DEFAULT_RELATED_COUNTS: Readonly<Record<string, number>> = {
  Media: 5,
  OpenHouse: 2,
  Showing: 2
};

/**
 * Determines which resources can be related to the given parent resource.
 * A resource is a valid related resource if it has both ResourceName and
 * ResourceRecordKey fields (RESO FK convention).
 */
export const getRelatedResources = (
  parentResource: string,
  fieldsByResource: Readonly<Record<string, ReadonlyArray<ResoField>>>
): ReadonlyArray<string> => {
  const related: string[] = [];

  for (const [resource, fields] of Object.entries(fieldsByResource)) {
    if (resource === parentResource) continue;
    const hasResourceName = fields.some(f => f.fieldName === 'ResourceName');
    const hasResourceRecordKey = fields.some(f => f.fieldName === 'ResourceRecordKey');
    if (hasResourceName && hasResourceRecordKey) {
      related.push(resource);
    }
  }

  return related;
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

/**
 * Checks whether a resource is a child resource that uses the
 * ResourceName/ResourceRecordKey FK convention.
 */
export const isChildResource = (resource: string): boolean => CHILD_RESOURCES.has(resource);
