import { useCallback, useEffect, useRef, useState } from 'react';
import { queryCollection } from '../api/client';
import type { ResourceName } from '../types';

const PAGE_SIZE = 25;

export interface UseCollectionResult {
  readonly rows: ReadonlyArray<Record<string, unknown>>;
  readonly count: number | undefined;
  readonly isLoading: boolean;
  readonly hasMore: boolean;
  readonly error: string | null;
  readonly loadMore: () => void;
}

/** Fetches a resource collection with infinite scroll pagination. */
export const useCollection = (
  resource: ResourceName,
  params: { $filter?: string; $orderby?: string; $select?: string; $expand?: string }
): UseCollectionResult => {
  const [rows, setRows] = useState<Record<string, unknown>[]>([]);
  const [count, setCount] = useState<number | undefined>(undefined);
  const [isLoading, setIsLoading] = useState(false);
  const [hasMore, setHasMore] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const skipRef = useRef(0);
  const abortRef = useRef<AbortController | null>(null);

  // Reset when resource or params change
  useEffect(() => {
    setRows([]);
    setCount(undefined);
    setHasMore(true);
    setError(null);
    skipRef.current = 0;

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
        setHasMore(result.value.length >= PAGE_SIZE);
        skipRef.current = result.value.length;
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
    if (isLoading || !hasMore) return;
    setIsLoading(true);
    try {
      const result = await queryCollection(resource, {
        $filter: params.$filter || undefined,
        $orderby: params.$orderby || undefined,
        $select: params.$select || undefined,
        $expand: params.$expand || undefined,
        $top: PAGE_SIZE,
        $skip: skipRef.current
      });

      setRows(prev => [...prev, ...result.value]);
      setHasMore(result.value.length >= PAGE_SIZE);
      skipRef.current += result.value.length;
    } catch (err) {
      const msg =
        err instanceof Error ? err.message : ((err as { error?: { message?: string } })?.error?.message ?? 'Failed to load more data');
      setError(msg);
    } finally {
      setIsLoading(false);
    }
  }, [resource, params.$filter, params.$orderby, params.$select, params.$expand, isLoading, hasMore]);

  return { rows, count, isLoading, hasMore, error, loadMore };
};
