import { useCallback, useEffect, useRef, useState } from 'react';
import { fetchCollectionByUrl, queryCollection } from '../api/client.js';
import type { ResourceName } from '../types.js';

const PAGE_SIZE = 25;

export interface UseCollectionResult {
  readonly rows: ReadonlyArray<Record<string, unknown>>;
  readonly count: number | undefined;
  readonly isLoading: boolean;
  readonly hasMore: boolean;
  readonly error: string | null;
  readonly loadMore: () => void;
}

/** Fetches a resource collection with infinite scroll pagination via @odata.nextLink. */
export const useCollection = (
  resource: ResourceName,
  params: { $filter?: string; $orderby?: string; $select?: string; $expand?: string }
): UseCollectionResult => {
  const [rows, setRows] = useState<Record<string, unknown>[]>([]);
  const [count, setCount] = useState<number | undefined>(undefined);
  const [isLoading, setIsLoading] = useState(false);
  const [hasMore, setHasMore] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const nextLinkRef = useRef<string | null>(null);
  const abortRef = useRef<AbortController | null>(null);

  // Reset when resource or params change
  useEffect(() => {
    setRows([]);
    setCount(undefined);
    setHasMore(true);
    setError(null);
    nextLinkRef.current = null;

    const loadFirst = async () => {
      setIsLoading(true);
      try {
        abortRef.current?.abort();
        abortRef.current = new AbortController();

        const result = await queryCollection(resource, {
          $filter: params.$filter || undefined,
          $orderby: params.$orderby || undefined,
          $select: params.$select || undefined,
          $expand: params.$expand || undefined,
          $top: PAGE_SIZE,
          $skip: 0,
          $count: true
        });

        setRows([...result.value]);
        if (result['@odata.count'] !== undefined) {
          setCount(result['@odata.count']);
        }
        // Use server-provided nextLink for pagination
        nextLinkRef.current = result['@odata.nextLink'] ?? null;
        setHasMore(nextLinkRef.current !== null);
      } catch (err) {
        const msg =
          err instanceof Error ? err.message : ((err as { error?: { message?: string } })?.error?.message ?? 'Failed to load data');
        setError(msg);
      } finally {
        setIsLoading(false);
      }
    };

    loadFirst();

    return () => {
      abortRef.current?.abort();
    };
  }, [resource, params.$filter, params.$orderby, params.$select, params.$expand]);

  const loadMore = useCallback(async () => {
    if (isLoading || !hasMore || !nextLinkRef.current) return;
    setIsLoading(true);
    try {
      // Follow the server-provided @odata.nextLink
      const result = await fetchCollectionByUrl(nextLinkRef.current);

      setRows(prev => [...prev, ...result.value]);
      nextLinkRef.current = result['@odata.nextLink'] ?? null;
      setHasMore(nextLinkRef.current !== null);
    } catch (err) {
      const msg =
        err instanceof Error ? err.message : ((err as { error?: { message?: string } })?.error?.message ?? 'Failed to load more data');
      setError(msg);
    } finally {
      setIsLoading(false);
    }
  }, [isLoading, hasMore]);

  return { rows, count, isLoading, hasMore, error, loadMore };
};
