import { useEffect, useState } from 'react';
import { useNavigate, useParams } from 'react-router';
import { readEntity } from '../api/client';
import { DeleteDialog } from '../components/delete-dialog';
import { FieldGroupSection } from '../components/field-group-section';
import { MediaCarousel } from '../components/media-carousel';
import { useMetadata } from '../hooks/use-metadata';
import { useUiConfig } from '../hooks/use-ui-config';
import type { ResoField, ResourceName } from '../types';
import { KEY_FIELD_MAP, TARGET_RESOURCES } from '../types';
import { formatAddress, formatFieldValue } from '../utils/format';

/** Detail page showing a full record with fields grouped by RESO Data Dictionary categories. */
export const DetailPage = () => {
  const { resource, key } = useParams<{ resource: string; key: string }>();
  const navigate = useNavigate();
  const resourceName = resource as ResourceName;

  const [record, setRecord] = useState<Record<string, unknown> | null>(null);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [showDelete, setShowDelete] = useState(false);

  const { fields } = useMetadata(resourceName);
  const { fieldGroups } = useUiConfig();

  if (!TARGET_RESOURCES.includes(resourceName) || !key) {
    return <div className="text-red-600 dark:text-red-400">Invalid resource or key</div>;
  }

  const keyField = KEY_FIELD_MAP[resourceName];

  useEffect(() => {
    let cancelled = false;
    setIsLoading(true);
    setError(null);

    const load = async () => {
      try {
        const result = await readEntity(resourceName, key!, { $expand: 'Media' });
        if (!cancelled) setRecord(result);
      } catch (err) {
        if (!cancelled) {
          const msg =
            err instanceof Error ? err.message : ((err as { error?: { message?: string } })?.error?.message ?? 'Failed to load record');
          setError(msg);
        }
      } finally {
        if (!cancelled) setIsLoading(false);
      }
    };

    load();
    return () => {
      cancelled = true;
    };
  }, [resourceName, key]);

  if (isLoading) return <div className="text-sm text-gray-500 dark:text-gray-400 py-4">Loading...</div>;
  if (error)
    return (
      <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 text-red-700 dark:text-red-400 rounded px-3 py-2 text-sm">
        {error}
      </div>
    );
  if (!record) return <div className="text-gray-500 dark:text-gray-400">Record not found</div>;

  // Extract media records
  const media = Array.isArray(record.Media) ? (record.Media as Record<string, unknown>[]) : [];

  // Group fields by RESO groups
  const resourceGroups = fieldGroups?.[resourceName] ?? {};
  const grouped = new Map<string, ResoField[]>();
  const ungrouped: ResoField[] = [];
  const pinnedFields = new Set([keyField, 'ModificationTimestamp']);

  for (const field of fields) {
    if (pinnedFields.has(field.fieldName)) continue;
    if (record[field.fieldName] === undefined || record[field.fieldName] === null) continue;
    // Skip OData annotations
    if (field.fieldName.startsWith('@')) continue;

    const groups = resourceGroups[field.fieldName];
    if (groups && groups.length > 0) {
      const groupKey = groups[0];
      const existing = grouped.get(groupKey);
      if (existing) existing.push(field);
      else grouped.set(groupKey, [field]);
    } else {
      ungrouped.push(field);
    }
  }

  for (const groupFields of grouped.values()) {
    groupFields.sort((a, b) => a.fieldName.localeCompare(b.fieldName));
  }
  ungrouped.sort((a, b) => a.fieldName.localeCompare(b.fieldName));

  const sortedGroups = [...grouped.entries()].sort((a, b) => a[0].localeCompare(b[0]));

  const renderFieldTable = (fieldList: ResoField[]) => (
    <div className="grid grid-cols-1 sm:grid-cols-2 gap-x-6 gap-y-1">
      {fieldList.map(field => (
        <div key={field.fieldName} className="flex items-baseline gap-2 py-0.5 text-sm">
          <span className="text-gray-500 dark:text-gray-400 shrink-0 w-40 sm:w-52 truncate">{field.fieldName}</span>
          <span className="text-gray-800 dark:text-gray-200 truncate">{formatFieldValue(record[field.fieldName], field)}</span>
        </div>
      ))}
    </div>
  );

  return (
    <div className="space-y-4">
      {/* Header with actions */}
      <div className="flex flex-col sm:flex-row items-start sm:items-center justify-between gap-2">
        <div>
          <button type="button" onClick={() => navigate(`/${resourceName}`)} className="text-sm text-blue-600 hover:text-blue-800 mb-1">
            &larr; Back to {resourceName}
          </button>
          <h2 className="text-xl font-semibold text-gray-900 dark:text-gray-100">{resourceName} Detail</h2>
        </div>
        <div className="flex gap-2">
          <button
            type="button"
            onClick={() => navigate(`/${resourceName}/edit/${encodeURIComponent(key!)}`)}
            className="px-3 py-1.5 text-sm bg-blue-600 text-white rounded hover:bg-blue-700">
            Edit
          </button>
          <button
            type="button"
            onClick={() => setShowDelete(true)}
            className="px-3 py-1.5 text-sm bg-red-600 text-white rounded hover:bg-red-700">
            Delete
          </button>
        </div>
      </div>

      {/* Pinned: Key + Address + Timestamp */}
      <div className="bg-white dark:bg-gray-800 border border-gray-200 dark:border-gray-700 rounded-lg p-4">
        {resourceName === 'Property' && formatAddress(record) && (
          <div className="text-base font-medium text-gray-900 dark:text-gray-100 mb-2">{formatAddress(record)}</div>
        )}
        <div className="grid grid-cols-1 sm:grid-cols-2 gap-2 text-sm">
          <div>
            <span className="text-gray-500 dark:text-gray-400">{keyField}: </span>
            <span className="font-mono text-gray-800 dark:text-gray-200">{String(record[keyField] ?? '')}</span>
          </div>
          {record.ModificationTimestamp != null && (
            <div>
              <span className="text-gray-500 dark:text-gray-400">ModificationTimestamp: </span>
              <span className="text-gray-800 dark:text-gray-200">{String(record.ModificationTimestamp)}</span>
            </div>
          )}
        </div>
      </div>

      {/* Media carousel */}
      {media.length > 0 && (
        <div className="bg-white dark:bg-gray-800 border border-gray-200 dark:border-gray-700 rounded-lg p-4">
          <h3 className="text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">Media ({media.length})</h3>
          <MediaCarousel media={media} />
        </div>
      )}

      {/* Grouped fields */}
      {sortedGroups.map(([group, groupFields]) => (
        <FieldGroupSection key={group} title={group} defaultOpen>
          {renderFieldTable(groupFields)}
        </FieldGroupSection>
      ))}

      {/* Ungrouped fields — flat list when no groupings exist, "Other" section otherwise */}
      {ungrouped.length > 0 && grouped.size === 0 && (
        <div className="bg-white dark:bg-gray-800 border border-gray-200 dark:border-gray-700 rounded-lg p-4">
          {renderFieldTable(ungrouped)}
        </div>
      )}
      {ungrouped.length > 0 && grouped.size > 0 && (
        <FieldGroupSection title="Other" defaultOpen>
          {renderFieldTable(ungrouped)}
        </FieldGroupSection>
      )}

      {/* Delete dialog */}
      {showDelete && (
        <DeleteDialog
          resource={resourceName}
          record={record}
          onDeleted={() => navigate(`/${resourceName}`)}
          onCancel={() => setShowDelete(false)}
        />
      )}
    </div>
  );
};
