import type { ResoField, ResourceName } from '../types';
import { InfiniteScroll } from './infinite-scroll';
import { ResultsCard } from './results-card';

interface ResultsListProps {
  readonly resource: ResourceName;
  readonly rows: ReadonlyArray<Record<string, unknown>>;
  readonly summaryFields: ReadonlyArray<string>;
  readonly fields: ReadonlyArray<ResoField>;
  readonly count: number | undefined;
  readonly isLoading: boolean;
  readonly hasMore: boolean;
  readonly error: string | null;
  readonly onLoadMore: () => void;
  readonly onRowClick: (key: string) => void;
}

/** Scrollable list of result cards with infinite scroll and count display. */
export const ResultsList = ({
  resource,
  rows,
  summaryFields,
  fields,
  count,
  isLoading,
  hasMore,
  error,
  onLoadMore,
  onRowClick
}: ResultsListProps) => {
  const fieldMap = new Map(fields.map(f => [f.fieldName, f]));

  return (
    <div>
      {/* Count display */}
      {count !== undefined && (
        <p className="text-sm text-gray-500 dark:text-gray-400 mb-3">
          {count.toLocaleString()} result{count !== 1 ? 's' : ''}
          {rows.length < count ? ` (showing ${rows.length.toLocaleString()})` : ''}
        </p>
      )}

      {/* Error */}
      {error && (
        <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 text-red-700 dark:text-red-400 rounded px-3 py-2 text-sm mb-3">
          {error}
        </div>
      )}

      {/* Empty state */}
      {!isLoading && rows.length === 0 && !error && (
        <div className="text-center py-12 text-gray-400 dark:text-gray-500">
          <p className="text-lg">No records found</p>
          <p className="text-sm mt-1">Try adjusting your search or add some data.</p>
        </div>
      )}

      {/* Results */}
      <div className="flex flex-col gap-2">
        {rows.map((row, i) => (
          <ResultsCard
            key={String(row[summaryFields[0]] ?? i)}
            resource={resource}
            record={row}
            summaryFields={summaryFields}
            fieldMap={fieldMap}
            onClick={onRowClick}
          />
        ))}
      </div>

      <InfiniteScroll onLoadMore={onLoadMore} hasMore={hasMore} isLoading={isLoading} />
    </div>
  );
};
