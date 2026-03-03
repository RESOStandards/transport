import type { ResoField, ResoLookup } from '../types';

const fieldsCache = new Map<string, ReadonlyArray<ResoField>>();
const lookupsCache = new Map<string, ReadonlyArray<ResoLookup>>();
const resourceLookupsCache = new Map<string, Readonly<Record<string, ReadonlyArray<ResoLookup>>>>();

/** Fetches field definitions for a resource. Cached per resource. */
export const fetchFieldsForResource = async (resource: string): Promise<ReadonlyArray<ResoField>> => {
  const cached = fieldsCache.get(resource);
  if (cached) return cached;
  const res = await fetch(`/api/metadata/fields?resource=${encodeURIComponent(resource)}`);
  if (!res.ok) throw new Error(`Failed to fetch fields for ${resource}: ${res.statusText}`);
  const fields: ReadonlyArray<ResoField> = await res.json();
  fieldsCache.set(resource, fields);
  return fields;
};

/** Fetches lookup values for a specific enum type. Cached per type. */
export const fetchLookupsForType = async (type: string): Promise<ReadonlyArray<ResoLookup>> => {
  const cached = lookupsCache.get(type);
  if (cached) return cached;
  const res = await fetch(`/api/metadata/lookups?type=${encodeURIComponent(type)}`);
  if (!res.ok) throw new Error(`Failed to fetch lookups for ${type}: ${res.statusText}`);
  const lookups: ReadonlyArray<ResoLookup> = await res.json();
  lookupsCache.set(type, lookups);
  return lookups;
};

/** Fetches all lookup values for all enum fields in a resource. Cached per resource. */
export const fetchLookupsForResource = async (resource: string): Promise<Readonly<Record<string, ReadonlyArray<ResoLookup>>>> => {
  const cached = resourceLookupsCache.get(resource);
  if (cached) return cached;
  const res = await fetch(`/api/metadata/lookups-for-resource?resource=${encodeURIComponent(resource)}`);
  if (!res.ok) throw new Error(`Failed to fetch lookups for ${resource}: ${res.statusText}`);
  const lookups: Readonly<Record<string, ReadonlyArray<ResoLookup>>> = await res.json();
  resourceLookupsCache.set(resource, lookups);
  return lookups;
};
