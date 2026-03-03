import type { ResourceName } from '../types';
import { KEY_FIELD_MAP } from '../types';
import { ADDRESS_FIELDS, formatAddress, formatFieldValue } from '../utils/format';
import { MediaCarousel } from './media-carousel';

interface ResultsCardProps {
  readonly resource: ResourceName;
  readonly record: Record<string, unknown>;
  readonly summaryFields: ReadonlyArray<string>;
  readonly fieldMap: ReadonlyMap<string, import('../types').ResoField>;
  readonly onClick: (key: string) => void;
}

/** Summary result card with media thumbnail and configurable fields. */
export const ResultsCard = ({ resource, record, summaryFields, fieldMap, onClick }: ResultsCardProps) => {
  const keyField = KEY_FIELD_MAP[resource];
  const key = String(record[keyField] ?? '');
  const media = Array.isArray(record.Media) ? (record.Media as Record<string, unknown>[]) : [];

  // Build formatted address for Property resources
  const address = resource === 'Property' ? formatAddress(record) : null;

  // Filter to fields that have data; hide individual address fields when a composed address is shown
  const hiddenFields = address ? ADDRESS_FIELDS : new Set<string>();
  const displayFields = summaryFields.filter(f => f !== keyField && !hiddenFields.has(f) && record[f] !== undefined && record[f] !== null);

  return (
    <button
      type="button"
      onClick={() => onClick(key)}
      className="w-full text-left bg-white dark:bg-gray-800 rounded-lg border border-gray-200 dark:border-gray-700 hover:border-blue-300 dark:hover:border-blue-600 hover:shadow-sm transition-all p-3 sm:p-4">
      <div className="flex flex-col sm:flex-row gap-3">
        {/* Media thumbnail */}
        {media.length > 0 && (
          <div className="w-full sm:w-36 shrink-0">
            <MediaCarousel media={media} compact />
          </div>
        )}

        {/* Fields */}
        <div className="flex-1 min-w-0">
          {/* Key + primary info */}
          <div className="flex items-baseline gap-2 mb-1">
            <span className="text-xs text-gray-500 dark:text-gray-400">{keyField}:</span>
            <span className="text-sm font-mono text-gray-700 dark:text-gray-300 truncate">{key}</span>
          </div>

          {/* Address line for Property */}
          {address && <div className="text-sm font-medium text-gray-900 dark:text-gray-100 truncate mb-1">{address}</div>}

          {/* Summary fields in a responsive grid */}
          <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-x-4 gap-y-0.5">
            {displayFields.slice(0, 9).map(fieldName => (
              <div key={fieldName} className="flex items-baseline gap-1 text-sm truncate">
                <span className="text-gray-500 dark:text-gray-400 shrink-0">{fieldName}:</span>
                <span className="text-gray-800 dark:text-gray-200 truncate">
                  {formatFieldValue(record[fieldName], fieldMap.get(fieldName))}
                </span>
              </div>
            ))}
          </div>

          {displayFields.length > 9 && (
            <span className="text-xs text-gray-400 dark:text-gray-500 mt-1 block">+{displayFields.length - 9} more fields</span>
          )}
        </div>
      </div>
    </button>
  );
};
