import { LexerError, ParseError, parseFilter } from '@reso/odata-filter-parser';
import { useCallback, useEffect, useState } from 'react';
import { useNavigate, useParams, useSearchParams } from 'react-router';
import { AdvancedSearch } from '../components/advanced-search';
import { ResultsList } from '../components/results-list';
import { SearchBar } from '../components/search-bar';
import { useCollection } from '../hooks/use-collection';
import { useMetadata } from '../hooks/use-metadata';
import { useUiConfig } from '../hooks/use-ui-config';
import type { ResourceName } from '../types';
import { KEY_FIELD_MAP, READ_ONLY_RESOURCES, TARGET_RESOURCES } from '../types';
import { getDisplayNameFromMap } from '../utils/format';

/** Search page with filter bar, optional advanced search, and infinite scroll results. */
export const SearchPage = () => {
  const { resource } = useParams<{ resource: string }>();
  const [searchParams, setSearchParams] = useSearchParams();
  const navigate = useNavigate();

  const resourceName = resource as ResourceName;
  if (!TARGET_RESOURCES.includes(resourceName)) {
    return <div className="p-4 sm:p-6 text-red-600 dark:text-red-400">Unknown resource: {resource}</div>;
  }

  const filter = searchParams.get('$filter') ?? '';
  const orderby = searchParams.get('$orderby') ?? '';
  const mode = searchParams.get('mode') ?? 'simple';
  const isAdvanced = mode === 'advanced';

  // Draft filter state — tracks what the user is composing before submitting
  const [draftFilter, setDraftFilter] = useState(filter);
  const [validationError, setValidationError] = useState<string | null>(null);

  // Sync draft when URL filter changes (browser back/forward, initial load)
  useEffect(() => {
    setDraftFilter(filter);
    setValidationError(null);
  }, [filter]);

  const { config, fieldGroups } = useUiConfig();
  const { fields, lookups, isLoading: metaLoading } = useMetadata(resourceName);

  // Resolve summary fields from config
  const resourceConfig = config?.resources?.[resourceName];
  const isAllFields = !resourceConfig || resourceConfig.summaryFields === '__all__';
  const summaryFields: string[] = isAllFields ? fields.map(f => f.fieldName) : [...resourceConfig.summaryFields];

  // Build field lookup map for display names
  const fieldMap = new Map(fields.map(f => [f.fieldName, f]));

  // Determine $select and $expand for the collection query
  const hasMedia = resourceName === 'Property';
  const selectFields = isAllFields ? undefined : summaryFields.join(',');

  const { rows, count, isLoading, hasMore, error, loadMore } = useCollection(resourceName, {
    $filter: filter || undefined,
    $orderby: orderby || undefined,
    $select: selectFields,
    $expand: hasMedia ? 'Media' : undefined
  });

  const handleSearch = useCallback(
    (newFilter: string) => {
      const params = new URLSearchParams(searchParams);
      if (newFilter) {
        params.set('$filter', newFilter);
      } else {
        params.delete('$filter');
      }
      setSearchParams(params);
    },
    [searchParams, setSearchParams]
  );

  /** Validate and submit the current draft filter. */
  const handleSubmit = useCallback(() => {
    const trimmed = draftFilter.trim();
    if (!trimmed) {
      setValidationError(null);
      handleSearch('');
      return;
    }
    try {
      parseFilter(trimmed);
      setValidationError(null);
      handleSearch(trimmed);
    } catch (err) {
      if (err instanceof ParseError || err instanceof LexerError) {
        setValidationError(err.message);
      } else {
        setValidationError('Invalid filter expression');
      }
    }
  }, [draftFilter, handleSearch]);

  const handleToggleAdvanced = useCallback(() => {
    const params = new URLSearchParams(searchParams);
    if (isAdvanced) {
      params.delete('mode');
    } else {
      params.set('mode', 'advanced');
    }
    setSearchParams(params);
  }, [searchParams, setSearchParams, isAdvanced]);

  const handleSort = useCallback(
    (field: string) => {
      const params = new URLSearchParams(searchParams);
      const currentOrder = params.get('$orderby') ?? '';
      if (currentOrder === `${field} asc`) {
        params.set('$orderby', `${field} desc`);
      } else {
        params.set('$orderby', `${field} asc`);
      }
      setSearchParams(params);
    },
    [searchParams, setSearchParams]
  );

  const handleRowClick = useCallback(
    (key: string) => {
      navigate(`/${resourceName}/${encodeURIComponent(key)}`);
    },
    [navigate, resourceName]
  );

  if (metaLoading) {
    return <div className="p-4 sm:p-6 text-sm text-gray-500 dark:text-gray-400">Loading metadata...</div>;
  }

  return (
    <div className="flex flex-col h-full min-h-0">
      {/* Pinned toolbar — does not scroll */}
      <div className="shrink-0 px-4 sm:px-6 pt-4 sm:pt-6 pb-3 space-y-4 bg-gray-50 dark:bg-gray-900 border-b border-gray-200 dark:border-gray-700">
        {/* Header */}
        <div className="flex flex-col sm:flex-row items-start sm:items-center justify-between gap-2">
          <h2 className="text-xl font-semibold text-gray-900 dark:text-gray-100">{resourceName}</h2>
          {!READ_ONLY_RESOURCES.has(resourceName) && (
            <div className="flex gap-2">
              <button
                type="button"
                onClick={() => navigate(`/${resourceName}/add`)}
                className="px-3 py-1.5 text-sm bg-green-600 text-white rounded hover:bg-green-700">
                + Add
              </button>
              <button
                type="button"
                onClick={() => navigate(`/${resourceName}/edit`)}
                className="px-3 py-1.5 text-sm border border-gray-300 dark:border-gray-600 text-gray-700 dark:text-gray-300 rounded hover:bg-gray-50 dark:hover:bg-gray-700">
                Edit
              </button>
              <button
                type="button"
                onClick={() => navigate(`/${resourceName}/delete`)}
                className="px-3 py-1.5 text-sm border border-red-300 dark:border-red-700 text-red-600 dark:text-red-400 rounded hover:bg-red-50 dark:hover:bg-red-900/20">
                Delete
              </button>
            </div>
          )}
        </div>

        {/* Search */}
        <SearchBar
          value={draftFilter}
          onChange={setDraftFilter}
          onSearch={handleSubmit}
          onToggleAdvanced={handleToggleAdvanced}
          isAdvancedMode={isAdvanced}
          validationError={validationError}
        />

        {/* Sortable column headers */}
        {rows.length > 0 && (
          <div className="flex flex-wrap gap-1">
            <span className="text-xs text-gray-500 dark:text-gray-400 mr-1">Sort by:</span>
            {summaryFields.slice(0, 6).map(f => (
              <button
                type="button"
                key={f}
                onClick={() => handleSort(f)}
                className={`text-xs px-2 py-0.5 rounded border ${
                  orderby.includes(f)
                    ? 'bg-blue-50 dark:bg-blue-900/30 border-blue-300 dark:border-blue-600 text-blue-700 dark:text-blue-400'
                    : 'border-gray-200 dark:border-gray-700 text-gray-500 dark:text-gray-400 hover:bg-gray-50 dark:hover:bg-gray-700'
                }`}>
                {getDisplayNameFromMap(f, fieldMap)} {orderby === `${f} asc` ? '↑' : orderby === `${f} desc` ? '↓' : ''}
              </button>
            ))}
          </div>
        )}

        {/* Result count */}
        {count !== undefined && (
          <p className="text-sm text-gray-500 dark:text-gray-400">
            {count.toLocaleString()} result{count !== 1 ? 's' : ''}
          </p>
        )}
      </div>

      {/* Scrollable results */}
      <div className="flex-1 overflow-y-auto min-h-0 px-4 sm:px-6 py-4 space-y-4">
        {/* Advanced search panel */}
        {isAdvanced && (
          <div className="bg-white dark:bg-gray-800 border border-gray-200 dark:border-gray-700 rounded-lg p-4">
            <AdvancedSearch
              resource={resourceName}
              fields={fields}
              lookups={lookups}
              fieldGroups={fieldGroups}
              filterString={draftFilter}
              onFilterChange={setDraftFilter}
              onSearch={handleSubmit}
            />
          </div>
        )}

        <ResultsList
          resource={resourceName}
          rows={rows}
          summaryFields={summaryFields}
          fields={fields}
          count={count}
          isLoading={isLoading}
          hasMore={hasMore}
          error={error}
          onLoadMore={loadMore}
          onRowClick={handleRowClick}
        />
      </div>
    </div>
  );
};
