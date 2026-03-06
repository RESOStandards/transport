import { useCallback, useEffect, useState } from 'react';
import { useNavigate, useParams } from 'react-router';
import { readEntity } from '../api/client';
import { DeleteDialog } from '../components/delete-dialog';
import { ExpandedEntityCard } from '../components/expanded-entity-card';
import { FieldGroupSection } from '../components/field-group-section';
import { MediaCarousel } from '../components/media-carousel';
import { useMetadata } from '../hooks/use-metadata';
import { useUiConfig } from '../hooks/use-ui-config';
import type { ResoField, ResourceName } from '../types';
import { KEY_FIELD_MAP, READ_ONLY_RESOURCES, TARGET_RESOURCES } from '../types';
import { ADDRESS_FIELDS, formatAddress, formatFieldValue, getDisplayName, isUrlValue, isVideoMediaType } from '../utils/format';

/** Renders a media preview (image or video) for a URL based on MediaType. */
const MediaPreview = ({ url, mediaType }: { readonly url: string; readonly mediaType?: string }) => {
  if (isVideoMediaType(mediaType)) {
    return (
      <video controls className="w-full max-h-96 rounded-lg bg-black">
        <source src={url} />
        <track kind="captions" />
      </video>
    );
  }
  // Default to image for photos, unknown types, and picsum URLs
  return <img src={url} alt="Media preview" className="w-full max-h-96 object-contain rounded-lg bg-gray-100 dark:bg-gray-800" />;
};

/** Detail page showing a full record with fields grouped by RESO Data Dictionary categories. */
export const DetailPage = () => {
  const { resource, key } = useParams<{ resource: string; key: string }>();
  const navigate = useNavigate();
  const resourceName = resource as ResourceName;

  const [record, setRecord] = useState<Record<string, unknown> | null>(null);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [showDelete, setShowDelete] = useState(false);
  const [keyCopied, setKeyCopied] = useState(false);

  const handleCopyKey = useCallback(() => {
    navigator.clipboard.writeText(String(key ?? '')).then(() => {
      setKeyCopied(true);
      setTimeout(() => setKeyCopied(false), 1500);
    });
  }, [key]);

  const { fields } = useMetadata(resourceName);
  const { config, fieldGroups } = useUiConfig();

  if (!TARGET_RESOURCES.includes(resourceName) || !key) {
    return <div className="p-4 sm:p-6 text-red-600 dark:text-red-400">Invalid resource or key</div>;
  }

  const keyField = KEY_FIELD_MAP[resourceName];

  useEffect(() => {
    // Wait for metadata to load before fetching the entity so we know which nav props to $expand
    if (fields.length === 0) return;

    let cancelled = false;
    setIsLoading(true);
    setError(null);

    const load = async () => {
      try {
        const expandFields = fields.filter(f => f.isExpansion).map(f => f.fieldName);
        const result = await readEntity(resourceName, key!, expandFields.length ? { $expand: expandFields.join(',') } : {});
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
  }, [resourceName, key, fields]);

  if (isLoading) return <div className="p-4 sm:p-6 text-sm text-gray-500 dark:text-gray-400">Loading...</div>;
  if (error)
    return (
      <div className="p-4 sm:p-6">
        <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 text-red-700 dark:text-red-400 rounded px-3 py-2 text-sm">
          {error}
        </div>
      </div>
    );
  if (!record) return <div className="p-4 sm:p-6 text-gray-500 dark:text-gray-400">Record not found</div>;

  // Extract media records
  const media = Array.isArray(record.Media) ? (record.Media as Record<string, unknown>[]) : [];

  // Build a field lookup map
  const fieldMap = new Map(fields.map(f => [f.fieldName, f]));

  // Group fields by RESO groups
  const resourceGroups = fieldGroups?.[resourceName] ?? {};
  const hasGroupings = Object.keys(resourceGroups).length > 0;
  const grouped = new Map<string, ResoField[]>();
  const ungrouped: ResoField[] = [];
  const skipFields = new Set([keyField, 'ModificationTimestamp']);

  // Determine summary fields for the left pane
  const summaryFieldNames = config?.resources?.[resourceName]?.summaryFields;
  const summarySet = new Set(Array.isArray(summaryFieldNames) ? summaryFieldNames : []);

  // For Property: summary fields go in the left pane, rest go in groups
  // For other resources: all fields go in a single alphabetical list (rendered beside carousel if present)
  const summaryFields: ResoField[] = [];

  // Collect expanded navigation properties
  const expansions: Array<{
    readonly fieldName: string;
    readonly targetResource: string;
    readonly records: ReadonlyArray<Record<string, unknown>>;
    readonly isCollection: boolean;
  }> = [];

  for (const field of fields) {
    if (!field.isExpansion) continue;
    const value = record[field.fieldName];
    if (value === null || value === undefined) continue;

    if (field.isCollection && Array.isArray(value) && value.length > 0) {
      expansions.push({
        fieldName: field.fieldName,
        targetResource: field.typeName ?? field.fieldName,
        records: value as Record<string, unknown>[],
        isCollection: true
      });
    } else if (!field.isCollection && typeof value === 'object' && !Array.isArray(value)) {
      expansions.push({
        fieldName: field.fieldName,
        targetResource: field.typeName ?? field.fieldName,
        records: [value as Record<string, unknown>],
        isCollection: false
      });
    }
  }

  for (const field of fields) {
    if (skipFields.has(field.fieldName)) continue;
    if (record[field.fieldName] === undefined || record[field.fieldName] === null) continue;
    if (field.fieldName.startsWith('@')) continue;
    if (field.isExpansion) continue;

    if (hasGroupings) {
      // Resources with groupings: split into summary vs grouped/ungrouped
      if (summarySet.has(field.fieldName)) {
        summaryFields.push(field);
      } else {
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
    } else {
      // Resources without groupings: all fields in one flat list
      ungrouped.push(field);
    }
  }

  // Sort summary fields in config order
  if (hasGroupings && summaryFields.length > 0) {
    const orderMap = new Map(Array.from(summarySet).map((name, i) => [name, i]));
    summaryFields.sort((a, b) => (orderMap.get(a.fieldName) ?? 999) - (orderMap.get(b.fieldName) ?? 999));
  }

  for (const groupFields of grouped.values()) {
    groupFields.sort((a, b) => a.fieldName.localeCompare(b.fieldName));
  }
  ungrouped.sort((a, b) => a.fieldName.localeCompare(b.fieldName));

  const sortedGroups = [...grouped.entries()].sort((a, b) => a[0].localeCompare(b[0]));

  // For resources without groupings, hide address fields from the flat list when a composed address is shown
  const address = formatAddress(record);

  // Sort expansions alphabetically by nav prop name
  expansions.sort((a, b) => a.fieldName.localeCompare(b.fieldName));

  // Right pane shows either an expanded Media carousel or a direct Media record preview
  const hasMediaPreview = resourceName === 'Media' && isUrlValue(record.MediaURL);
  const hasRightPane = media.length > 0 || hasMediaPreview;

  const renderFieldList = (fieldList: ResoField[], columns: 1 | 2 = 2) => (
    <div className={`grid grid-cols-1 ${columns === 2 ? 'sm:grid-cols-2' : ''} gap-x-6 gap-y-1`}>
      {fieldList.map(field => {
        const value = record[field.fieldName];
        return (
          <div key={field.fieldName} className="flex items-baseline gap-2 py-0.5 text-sm">
            <span className="text-gray-500 dark:text-gray-400 shrink-0 w-48 sm:w-64 truncate" title={field.fieldName}>
              {getDisplayName(field)}
            </span>
            {isUrlValue(value) ? (
              <a
                href={value}
                target="_blank"
                rel="noopener noreferrer"
                className="text-blue-600 dark:text-blue-400 hover:underline truncate">
                {value}
              </a>
            ) : (
              <span className="text-gray-800 dark:text-gray-200 truncate">{formatFieldValue(value, field)}</span>
            )}
          </div>
        );
      })}
    </div>
  );

  return (
    <div className="flex flex-col h-full min-h-0">
      {/* Pinned header — does not scroll */}
      <div className="shrink-0 px-4 sm:px-6 pt-4 sm:pt-6 pb-3 bg-gray-50 dark:bg-gray-900 border-b border-gray-200 dark:border-gray-700">
        <div className="flex flex-col sm:flex-row items-start sm:items-center justify-between gap-2">
          <div>
            <button type="button" onClick={() => navigate(`/${resourceName}`)} className="text-sm text-blue-600 hover:text-blue-800 mb-1">
              &larr; Back to {resourceName}
            </button>
            <h2 className="text-xl font-semibold text-gray-900 dark:text-gray-100">{resourceName} Detail</h2>
          </div>
          {!READ_ONLY_RESOURCES.has(resourceName) && (
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
          )}
        </div>
      </div>

      {/* Scrollable content */}
      <div className="flex-1 overflow-y-auto min-h-0 px-4 sm:px-6 py-4 space-y-4">
        {/* Summary/fields + Media side-by-side */}
        <div className={`flex flex-col ${hasRightPane ? 'lg:flex-row' : ''} gap-4`}>
          {/* Left pane: Summary (grouped resources) or all fields (ungrouped resources) */}
          <div
            className={`bg-white dark:bg-gray-800 border border-gray-200 dark:border-gray-700 rounded-lg p-4 ${hasRightPane ? 'lg:w-1/2' : 'w-full'}`}>
            {resourceName === 'Property' && address && (
              <div className="text-base font-medium text-gray-900 dark:text-gray-100 mb-2">{address}</div>
            )}
            <div className="flex items-center gap-2 py-0.5 text-sm mb-1">
              <span className="text-gray-500 dark:text-gray-400 shrink-0">{keyField}:</span>
              <span className="font-mono text-gray-800 dark:text-gray-200">{String(record[keyField] ?? '')}</span>
              <button
                type="button"
                onClick={handleCopyKey}
                title={keyCopied ? 'Copied!' : 'Copy key to clipboard'}
                className="text-gray-400 hover:text-gray-600 dark:hover:text-gray-300">
                {keyCopied ? (
                  <svg className="w-4 h-4 text-green-500" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <title>Copied</title>
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M5 13l4 4L19 7" />
                  </svg>
                ) : (
                  <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <title>Copy to clipboard</title>
                    <path
                      strokeLinecap="round"
                      strokeLinejoin="round"
                      strokeWidth={2}
                      d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z"
                    />
                  </svg>
                )}
              </button>
            </div>
            {record.ModificationTimestamp != null && (
              <div className="flex items-baseline gap-2 py-0.5 text-sm mb-1">
                <span className="text-gray-500 dark:text-gray-400 shrink-0">ModificationTimestamp:</span>
                <span className="text-gray-800 dark:text-gray-200">{String(record.ModificationTimestamp)}</span>
              </div>
            )}
            {hasGroupings && summaryFields.length > 0 && (
              <>
                <h3 className="text-sm font-medium text-gray-700 dark:text-gray-300 mt-3 mb-2">Summary</h3>
                {renderFieldList(
                  summaryFields.filter(f => !ADDRESS_FIELDS.has(f.fieldName)),
                  1
                )}
              </>
            )}
            {!hasGroupings && ungrouped.length > 0 && renderFieldList(ungrouped, 2)}
          </div>

          {/* Right pane: Media carousel (Property etc.) or Media preview (Media resource) */}
          {media.length > 0 && (
            <div className="bg-white dark:bg-gray-800 border border-gray-200 dark:border-gray-700 rounded-lg p-4 lg:w-1/2">
              <h3 className="text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">Media ({media.length})</h3>
              <MediaCarousel media={media} />
            </div>
          )}
          {hasMediaPreview && (
            <div className="bg-white dark:bg-gray-800 border border-gray-200 dark:border-gray-700 rounded-lg p-4 lg:w-1/2">
              <h3 className="text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">Preview</h3>
              <MediaPreview url={record.MediaURL as string} mediaType={record.MediaType as string | undefined} />
            </div>
          )}
        </div>

        {/* Grouped fields (resources with groupings only) */}
        {sortedGroups.map(([group, groupFields]) => (
          <FieldGroupSection key={group} title={group} defaultOpen>
            {renderFieldList(groupFields)}
          </FieldGroupSection>
        ))}

        {/* Ungrouped remainder in "Other" (only when resource HAS groupings) */}
        {ungrouped.length > 0 && hasGroupings && (
          <FieldGroupSection title="Other" defaultOpen>
            {renderFieldList(ungrouped)}
          </FieldGroupSection>
        )}

        {/* Related Records — all expanded navigation properties */}
        {expansions.length > 0 && (
          <FieldGroupSection title="Related Records" defaultOpen>
            <div className="space-y-3">
              {expansions.map(exp => (
                <ExpandedEntityCard
                  key={exp.fieldName}
                  title={exp.fieldName}
                  targetResource={exp.targetResource}
                  records={exp.records}
                  isCollection={exp.isCollection}
                />
              ))}
            </div>
          </FieldGroupSection>
        )}
      </div>

      {/* Delete dialog (fixed overlay, outside scroll area) */}
      {!READ_ONLY_RESOURCES.has(resourceName) && showDelete && (
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
