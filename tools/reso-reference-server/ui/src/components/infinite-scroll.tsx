import { useEffect, useRef } from 'react';

interface InfiniteScrollProps {
  readonly onLoadMore: () => void;
  readonly hasMore: boolean;
  readonly isLoading: boolean;
}

/** Sentinel element that triggers loading more data when scrolled into view. */
export const InfiniteScroll = ({ onLoadMore, hasMore, isLoading }: InfiniteScrollProps) => {
  const sentinelRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    const sentinel = sentinelRef.current;
    if (!sentinel || !hasMore) return;

    const observer = new IntersectionObserver(
      entries => {
        if (entries[0].isIntersecting && !isLoading && hasMore) {
          onLoadMore();
        }
      },
      { rootMargin: '200px' }
    );

    observer.observe(sentinel);
    return () => observer.disconnect();
  }, [onLoadMore, hasMore, isLoading]);

  return (
    <div ref={sentinelRef} className="py-4 text-center">
      {isLoading && <span className="text-sm text-gray-500">Loading...</span>}
      {!hasMore && !isLoading && <span className="text-sm text-gray-400">End of results</span>}
    </div>
  );
};
