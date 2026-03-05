import { createHash } from 'node:crypto';
import type { DataAccessLayer, ResourceContext } from '../db/data-access.js';
import { getFieldsForResource, getKeyFieldForResource } from './loader.js';
import type { ResoLookup, ResoMetadata } from './types.js';

/** Prefix to strip from fully-qualified lookup names (e.g., "org.reso.metadata.enums.StandardStatus" → "StandardStatus"). */
const ENUM_PREFIX = 'org.reso.metadata.enums.';

/** Strips the enum namespace prefix from a lookup name. */
const stripEnumPrefix = (lookupName: string): string =>
  lookupName.startsWith(ENUM_PREFIX) ? lookupName.slice(ENUM_PREFIX.length) : lookupName;

/** Extracts the StandardName annotation value from a lookup entry. */
const getStandardName = (lookup: ResoLookup): string | undefined =>
  lookup.annotations?.find(a => a.term === 'RESO.OData.Metadata.StandardName')?.value;

/** Generates a deterministic LookupKey using SHA-3 256 hash of LookupName:LookupValue. */
const generateLookupKey = (lookupName: string, lookupValue: string): string =>
  createHash('sha3-256').update(`${lookupName}:${lookupValue}`).digest('hex');

/**
 * Transforms the metadata lookups array into Lookup entity records
 * conforming to RESO DD 2.0 Section 2.2.
 *
 * Each record contains:
 * - LookupKey: SHA-3 256 hash of "{LookupName}:{LookupValue}"
 * - LookupName: Short name (e.g., "StandardStatus")
 * - LookupValue: Human-friendly display name from StandardName annotation
 * - StandardLookupValue: Same as LookupValue for DD-standard entries
 * - LegacyODataValue: Original CamelCase lookupValue from metadata
 * - ModificationTimestamp: Metadata generation timestamp
 */
export const buildLookupRecords = (metadata: ResoMetadata): ReadonlyArray<Record<string, unknown>> => {
  const timestamp = metadata.generatedOn ?? new Date().toISOString();

  return metadata.lookups.map(lookup => {
    const shortName = stripEnumPrefix(lookup.lookupName);
    const standardName = getStandardName(lookup);
    const humanFriendlyValue = standardName ?? lookup.lookupValue;

    return {
      LookupKey: generateLookupKey(shortName, humanFriendlyValue),
      LookupName: shortName,
      LookupValue: humanFriendlyValue,
      StandardLookupValue: humanFriendlyValue,
      LegacyODataValue: lookup.lookupValue,
      ModificationTimestamp: timestamp
    };
  });
};

/**
 * Seeds the Lookup table/collection from metadata if it is empty.
 * Called at server startup when ENUM_MODE=string.
 */
export const seedLookups = async (dal: DataAccessLayer, metadata: ResoMetadata): Promise<number> => {
  const keyField = getKeyFieldForResource('Lookup');
  const fields = getFieldsForResource(metadata, 'Lookup');
  if (!keyField || fields.length === 0) return 0;

  const ctx: ResourceContext = {
    resource: 'Lookup',
    keyField,
    fields,
    navigationBindings: []
  };

  // Check if already seeded
  const existing = await dal.queryCollection(ctx, { $count: true, $top: 0 });
  if (existing.count && existing.count > 0) {
    return existing.count;
  }

  const records = buildLookupRecords(metadata);
  let inserted = 0;

  for (const record of records) {
    try {
      await dal.insert(ctx, record as Record<string, unknown>);
      inserted++;
    } catch (err) {
      console.warn(
        `Failed to insert lookup ${record.LookupName}.${record.LegacyODataValue}: ${err instanceof Error ? err.message : 'unknown error'}`
      );
    }
  }

  return inserted;
};
