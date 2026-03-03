import { useCallback, useEffect, useState } from 'react';
import type { GenerateResponse, ResourceStatus } from '../../api/admin-client';
import { generateData, getGeneratorStatus } from '../../api/admin-client';

/** Default related record counts. */
const DEFAULT_RELATED_COUNTS: Record<string, number> = {
  Media: 5,
  OpenHouse: 2,
  Showing: 2
};

/** Known child resources (have ResourceName/ResourceRecordKey FK convention). */
const CHILD_RESOURCES = ['Media', 'OpenHouse', 'Showing'];

/** Admin page for generating seed data. */
export const DataGeneratorPage = () => {
  const [resources, setResources] = useState<ReadonlyArray<ResourceStatus>>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  // Form state
  const [selectedResource, setSelectedResource] = useState('Property');
  const [count, setCount] = useState(10);
  const [relatedConfig, setRelatedConfig] = useState<Record<string, { enabled: boolean; count: number }>>(
    Object.fromEntries(CHILD_RESOURCES.map(r => [r, { enabled: true, count: DEFAULT_RELATED_COUNTS[r] ?? 2 }]))
  );

  // Generation state
  const [generating, setGenerating] = useState(false);
  const [result, setResult] = useState<GenerateResponse | null>(null);

  const loadStatus = useCallback(async () => {
    try {
      setLoading(true);
      setError(null);
      const status = await getGeneratorStatus();
      setResources(status.resources);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to load status');
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    loadStatus();
  }, [loadStatus]);

  const handleGenerate = async () => {
    setGenerating(true);
    setResult(null);
    setError(null);

    const relatedRecords: Record<string, number> = {};
    for (const [resource, config] of Object.entries(relatedConfig)) {
      if (config.enabled && resource !== selectedResource) {
        relatedRecords[resource] = config.count;
      }
    }

    try {
      const response = await generateData({
        resource: selectedResource,
        count,
        relatedRecords: Object.keys(relatedRecords).length > 0 ? relatedRecords : undefined
      });
      setResult(response);
      // Refresh status after generation
      await loadStatus();
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Generation failed');
    } finally {
      setGenerating(false);
    }
  };

  const toggleRelated = (resource: string) => {
    setRelatedConfig(prev => ({
      ...prev,
      [resource]: { ...prev[resource], enabled: !prev[resource].enabled }
    }));
  };

  const setRelatedCount = (resource: string, newCount: number) => {
    setRelatedConfig(prev => ({
      ...prev,
      [resource]: { ...prev[resource], count: Math.max(1, newCount) }
    }));
  };

  // Parent resources are those not in CHILD_RESOURCES
  const parentResources = resources.filter(r => !CHILD_RESOURCES.includes(r.resource));

  return (
    <div className="max-w-2xl">
      <h2 className="text-xl font-semibold text-gray-900 dark:text-gray-100 mb-6">Data Generator</h2>

      {/* Current status */}
      {!loading && resources.length > 0 && (
        <div className="mb-6">
          <h3 className="text-sm font-medium text-gray-600 dark:text-gray-400 mb-2">Current Record Counts</h3>
          <div className="grid grid-cols-3 sm:grid-cols-6 gap-2">
            {resources.map(r => (
              <div key={r.resource} className="bg-gray-100 dark:bg-gray-800 rounded px-3 py-2 text-center">
                <div className="text-xs text-gray-500 dark:text-gray-400">{r.resource}</div>
                <div className="text-lg font-semibold text-gray-900 dark:text-gray-100">{r.count}</div>
              </div>
            ))}
          </div>
        </div>
      )}

      {error && (
        <div className="mb-4 bg-red-50 dark:bg-red-900/20 border border-red-300 dark:border-red-700 rounded-lg p-3 text-sm text-red-700 dark:text-red-300">
          {error}
        </div>
      )}

      {/* Generation form */}
      <div className="bg-white dark:bg-gray-800 border border-gray-200 dark:border-gray-700 rounded-lg p-5 space-y-5">
        {/* Resource selector */}
        <div>
          <label htmlFor="resource" className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
            Resource
          </label>
          <select
            id="resource"
            value={selectedResource}
            onChange={e => setSelectedResource(e.target.value)}
            className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100 text-sm focus:outline-none focus:ring-2 focus:ring-blue-500">
            {(parentResources.length > 0 ? parentResources : resources).map(r => (
              <option key={r.resource} value={r.resource}>
                {r.resource} ({r.fields} fields, {r.count} existing)
              </option>
            ))}
          </select>
        </div>

        {/* Count */}
        <div>
          <label htmlFor="count" className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
            Number of records
          </label>
          <input
            id="count"
            type="number"
            value={count}
            onChange={e => setCount(Math.max(1, Number(e.target.value)))}
            min={1}
            max={10000}
            className="w-32 px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100 text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
          />
        </div>

        {/* Related records */}
        <div>
          <h3 className="text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">Related Records</h3>
          <div className="space-y-2">
            {CHILD_RESOURCES.filter(r => r !== selectedResource).map(resource => (
              <div key={resource} className="flex items-center gap-3">
                <label className="flex items-center gap-2 cursor-pointer">
                  <input
                    type="checkbox"
                    checked={relatedConfig[resource]?.enabled ?? false}
                    onChange={() => toggleRelated(resource)}
                    className="rounded border-gray-300 dark:border-gray-600 text-blue-600 focus:ring-blue-500"
                  />
                  <span className="text-sm text-gray-700 dark:text-gray-300">{resource}</span>
                </label>
                {relatedConfig[resource]?.enabled && (
                  <div className="flex items-center gap-1">
                    <input
                      type="number"
                      value={relatedConfig[resource]?.count ?? 2}
                      onChange={e => setRelatedCount(resource, Number(e.target.value))}
                      min={1}
                      max={100}
                      className="w-16 px-2 py-1 border border-gray-300 dark:border-gray-600 rounded bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100 text-xs focus:outline-none focus:ring-2 focus:ring-blue-500"
                    />
                    <span className="text-xs text-gray-500 dark:text-gray-400">per {selectedResource}</span>
                  </div>
                )}
              </div>
            ))}
          </div>
        </div>

        {/* Summary */}
        <div className="bg-gray-50 dark:bg-gray-900 rounded p-3 text-sm text-gray-600 dark:text-gray-400">
          <strong>Plan:</strong> {count} {selectedResource} records
          {Object.entries(relatedConfig).some(([r, c]) => c.enabled && r !== selectedResource) && (
            <>
              {' + '}
              {Object.entries(relatedConfig)
                .filter(([r, c]) => c.enabled && r !== selectedResource)
                .map(([r, c]) => `${c.count * count} ${r}`)
                .join(', ')}
            </>
          )}
        </div>

        {/* Generate button */}
        <button
          type="button"
          onClick={handleGenerate}
          disabled={generating || loading}
          className="w-full px-4 py-2.5 bg-amber-600 hover:bg-amber-700 disabled:bg-gray-400 text-white text-sm font-medium rounded-lg transition-colors">
          {generating ? 'Generating...' : 'Generate Data'}
        </button>
      </div>

      {/* Results */}
      {result && (
        <div className="mt-6 bg-green-50 dark:bg-green-900/20 border border-green-300 dark:border-green-700 rounded-lg p-5">
          <h3 className="text-sm font-semibold text-green-800 dark:text-green-200 mb-3">Generation Complete</h3>
          <div className="space-y-1 text-sm text-green-700 dark:text-green-300">
            <p>
              {result.resource}: {result.created} created, {result.failed} failed
            </p>
            {result.relatedResults.map(r => (
              <p key={r.resource}>
                {r.resource}: {r.created} created, {r.failed} failed
              </p>
            ))}
            <p className="text-xs text-green-600 dark:text-green-400 mt-2">Duration: {(result.durationMs / 1000).toFixed(1)}s</p>
          </div>
          {result.errors.length > 0 && (
            <div className="mt-3 text-xs text-red-600 dark:text-red-400">
              <p className="font-medium">Errors:</p>
              {result.errors.slice(0, 5).map(err => (
                <p key={err} className="ml-2">
                  {err}
                </p>
              ))}
            </div>
          )}
        </div>
      )}
    </div>
  );
};
