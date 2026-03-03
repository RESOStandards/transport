import { useCallback, useEffect, useState } from 'react';
import { useNavigate, useParams } from 'react-router';
import { readEntity, updateEntity } from '../api/client';
import { KeyPrompt } from '../components/key-prompt';
import { RecordForm } from '../components/record-form';
import { useMetadata } from '../hooks/use-metadata';
import { useUiConfig } from '../hooks/use-ui-config';
import type { ResourceName } from '../types';
import { TARGET_RESOURCES } from '../types';

/** Page for editing an existing record. Shows key prompt if no key in URL. */
export const EditPage = () => {
  const { resource, key } = useParams<{ resource: string; key: string }>();
  const navigate = useNavigate();
  const resourceName = resource as ResourceName;

  const [record, setRecord] = useState<Record<string, unknown> | null>(null);
  const [isLoadingRecord, setIsLoadingRecord] = useState(false);
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const { fields, lookups, isLoading: metaLoading } = useMetadata(resourceName);
  const { fieldGroups } = useUiConfig();

  if (!TARGET_RESOURCES.includes(resourceName)) {
    return <div className="text-red-600 dark:text-red-400">Unknown resource: {resource}</div>;
  }

  // Load record when key is provided
  useEffect(() => {
    if (!key) return;
    let cancelled = false;
    setIsLoadingRecord(true);
    setError(null);

    const load = async () => {
      try {
        const result = await readEntity(resourceName, key);
        if (!cancelled) setRecord(result);
      } catch (err) {
        if (!cancelled) {
          const msg =
            err instanceof Error ? err.message : ((err as { error?: { message?: string } })?.error?.message ?? 'Failed to load record');
          setError(msg);
        }
      } finally {
        if (!cancelled) setIsLoadingRecord(false);
      }
    };

    load();
    return () => {
      cancelled = true;
    };
  }, [resourceName, key]);

  const handleKeySubmit = useCallback(
    (submittedKey: string) => {
      navigate(`/${resourceName}/edit/${encodeURIComponent(submittedKey)}`);
    },
    [navigate, resourceName]
  );

  const handleSubmit = useCallback(
    async (values: Record<string, unknown>) => {
      if (!key) return;
      setIsSubmitting(true);
      try {
        await updateEntity(resourceName, key, values);
        navigate(`/${resourceName}/${encodeURIComponent(key)}`);
      } finally {
        setIsSubmitting(false);
      }
    },
    [resourceName, key, navigate]
  );

  if (metaLoading) return <div className="text-sm text-gray-500 dark:text-gray-400 py-4">Loading metadata...</div>;

  // No key — show prompt
  if (!key) {
    return (
      <div className="space-y-4">
        <button type="button" onClick={() => navigate(`/${resourceName}`)} className="text-sm text-blue-600 hover:text-blue-800">
          &larr; Back to {resourceName}
        </button>
        <KeyPrompt resource={resourceName} onSubmit={handleKeySubmit} action="edit" />
      </div>
    );
  }

  if (isLoadingRecord) return <div className="text-sm text-gray-500 dark:text-gray-400 py-4">Loading record...</div>;
  if (error)
    return (
      <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 text-red-700 dark:text-red-400 rounded px-3 py-2 text-sm">
        {error}
      </div>
    );
  if (!record) return <div className="text-gray-500 dark:text-gray-400">Record not found</div>;

  return (
    <div className="space-y-4">
      <div>
        <button type="button" onClick={() => navigate(`/${resourceName}`)} className="text-sm text-blue-600 hover:text-blue-800 mb-1">
          &larr; Back to {resourceName}
        </button>
        <h2 className="text-xl font-semibold text-gray-900 dark:text-gray-100">Edit {resourceName}</h2>
      </div>

      <RecordForm
        resource={resourceName}
        fields={fields}
        lookups={lookups}
        fieldGroups={fieldGroups}
        initialValues={record}
        isEdit
        onSubmit={handleSubmit}
        isLoading={isSubmitting}
      />
    </div>
  );
};
