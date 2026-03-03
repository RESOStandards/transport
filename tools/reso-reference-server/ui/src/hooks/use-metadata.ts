import { useEffect, useState } from 'react';
import { fetchFieldsForResource, fetchLookupsForResource } from '../api/metadata';
import type { ResoField, ResoLookup } from '../types';

export interface UseMetadataResult {
  readonly fields: ReadonlyArray<ResoField>;
  readonly lookups: Readonly<Record<string, ReadonlyArray<ResoLookup>>>;
  readonly isLoading: boolean;
  readonly error: string | null;
}

/** Fetches and caches field definitions and lookups for a resource. */
export const useMetadata = (resource: string): UseMetadataResult => {
  const [fields, setFields] = useState<ReadonlyArray<ResoField>>([]);
  const [lookups, setLookups] = useState<Readonly<Record<string, ReadonlyArray<ResoLookup>>>>({});
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;
    setIsLoading(true);
    setError(null);

    const load = async () => {
      try {
        const [fieldsResult, lookupsResult] = await Promise.all([fetchFieldsForResource(resource), fetchLookupsForResource(resource)]);
        if (!cancelled) {
          setFields(fieldsResult);
          setLookups(lookupsResult);
        }
      } catch (err) {
        if (!cancelled) {
          setError(err instanceof Error ? err.message : 'Failed to load metadata');
        }
      } finally {
        if (!cancelled) setIsLoading(false);
      }
    };

    load();
    return () => {
      cancelled = true;
    };
  }, [resource]);

  return { fields, lookups, isLoading, error };
};
