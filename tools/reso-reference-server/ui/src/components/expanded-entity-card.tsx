import { useState } from 'react';
import { useNavigate } from 'react-router';
import { KEY_FIELD_MAP, TARGET_RESOURCES } from '../types';
import type { ResourceName } from '../types';

interface ExpandedEntityCardProps {
  /** Navigation property name (e.g., "ListAgent", "Media"). */
  readonly title: string;
  /** Target resource type (e.g., "Member", "Media"). */
  readonly targetResource: string;
  /** Expanded entity records (1 for to-one, N for to-many). */
  readonly records: ReadonlyArray<Record<string, unknown>>;
  /** Whether this is a collection (enables pagination). */
  readonly isCollection: boolean;
}

/** Formats a value for display without full metadata. */
const formatValue = (value: unknown): string => {
  if (value === null || value === undefined) return '\u2014';
  if (typeof value === 'boolean') return value ? 'Yes' : 'No';
  if (Array.isArray(value)) return value.join(', ');
  if (typeof value === 'object') return JSON.stringify(value);
  return String(value);
};

/** Returns true if a field key should be displayed. */
const isDisplayableField = (key: string, value: unknown): boolean => {
  if (key.startsWith('@')) return false;
  if (value === null || value === undefined) return false;
  // Skip nested expansion objects/arrays (they'd be another level of $expand)
  if (typeof value === 'object' && !Array.isArray(value)) return false;
  return true;
};

/** Inset card displaying the field-value data of an expanded navigation property. */
export const ExpandedEntityCard = ({ title, targetResource, records, isCollection }: ExpandedEntityCardProps) => {
  const [currentIndex, setCurrentIndex] = useState(0);
  const navigate = useNavigate();

  if (records.length === 0) return null;

  const current = records[currentIndex];
  const isTargetNavigable = TARGET_RESOURCES.includes(targetResource as ResourceName);
  const keyField = isTargetNavigable ? KEY_FIELD_MAP[targetResource as ResourceName] : undefined;
  const entityKey = keyField ? current[keyField] : undefined;

  const displayFields = Object.entries(current)
    .filter(([key, value]) => isDisplayableField(key, value))
    .sort(([a], [b]) => a.localeCompare(b));

  const handlePrev = () => setCurrentIndex(i => (i > 0 ? i - 1 : records.length - 1));
  const handleNext = () => setCurrentIndex(i => (i < records.length - 1 ? i + 1 : 0));

  const handleViewEntity = () => {
    if (isTargetNavigable && entityKey) {
      navigate(`/${targetResource}/${encodeURIComponent(String(entityKey))}`);
    }
  };

  return (
    <div className="border border-gray-200 dark:border-gray-700 rounded-lg bg-gray-50 dark:bg-gray-800/50 overflow-hidden">
      {/* Title bar */}
      <div className="flex items-center justify-between px-3 py-2 bg-gray-100 dark:bg-gray-800 border-b border-gray-200 dark:border-gray-700">
        <span className="text-sm font-medium text-gray-700 dark:text-gray-300">
          {title}
          <span className="ml-1.5 text-xs font-normal text-gray-400 dark:text-gray-500">({targetResource})</span>
        </span>

        <div className="flex items-center gap-2">
          {/* Collection pagination */}
          {isCollection && records.length > 1 && (
            <div className="flex items-center gap-1">
              <button
                type="button"
                onClick={handlePrev}
                className="px-1.5 py-0.5 text-xs text-gray-500 hover:text-gray-700 dark:text-gray-400 dark:hover:text-gray-200"
                aria-label="Previous record">
                &larr;
              </button>
              <span className="text-xs text-gray-500 dark:text-gray-400 tabular-nums">
                {currentIndex + 1}/{records.length}
              </span>
              <button
                type="button"
                onClick={handleNext}
                className="px-1.5 py-0.5 text-xs text-gray-500 hover:text-gray-700 dark:text-gray-400 dark:hover:text-gray-200"
                aria-label="Next record">
                &rarr;
              </button>
            </div>
          )}

          {/* View link for navigable to-one entities */}
          {isTargetNavigable && entityKey != null && (
            <button
              type="button"
              onClick={handleViewEntity}
              className="text-xs text-blue-600 hover:text-blue-800 dark:text-blue-400 dark:hover:text-blue-300">
              View &rarr;
            </button>
          )}
        </div>
      </div>

      {/* Two-column field list, vertically scrollable, max 4 rows visible */}
      <div className="overflow-y-auto max-h-28 px-3 py-1">
        <div className="grid grid-cols-1 sm:grid-cols-2 gap-x-6 gap-y-0.5">
          {displayFields.map(([key, value]) => (
            <div key={key} className="flex items-baseline gap-2 py-0.5 text-sm">
              <span className="text-gray-500 dark:text-gray-400 shrink-0 w-40 sm:w-44 truncate">{key}</span>
              <span className="text-gray-800 dark:text-gray-200 truncate" title={formatValue(value)}>
                {formatValue(value)}
              </span>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
};
