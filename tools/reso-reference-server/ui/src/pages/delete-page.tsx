import { useCallback, useEffect, useState } from 'react';
import { useNavigate, useParams } from 'react-router';
import { readEntity } from '../api/client';
import { DeleteDialog } from '../components/delete-dialog';
import { KeyPrompt } from '../components/key-prompt';
import type { ResourceName } from '../types';
import { KEY_FIELD_MAP, TARGET_RESOURCES } from '../types';

/** Page for deleting a record. Prompts for key, loads record, shows confirmation dialog. */
export const DeletePage = () => {
  const { resource } = useParams<{ resource: string }>();
  const navigate = useNavigate();
  const resourceName = resource as ResourceName;

  const [key, setKey] = useState<string | null>(null);
  const [record, setRecord] = useState<Record<string, unknown> | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  if (!TARGET_RESOURCES.includes(resourceName)) {
    return <div className="text-red-600 dark:text-red-400">Unknown resource: {resource}</div>;
  }

  // Load record when key is set
  useEffect(() => {
    if (!key) return;
    let cancelled = false;
    setIsLoading(true);
    setError(null);

    const load = async () => {
      try {
        const result = await readEntity(resourceName, key);
        if (!cancelled) setRecord(result);
      } catch (err) {
        if (!cancelled) {
          const msg =
            err instanceof Error ? err.message : ((err as { error?: { message?: string } })?.error?.message ?? 'Record not found');
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

  const handleKeySubmit = useCallback((submittedKey: string) => {
    setKey(submittedKey);
    setRecord(null);
    setError(null);
  }, []);

  // No key yet — show prompt
  if (!key) {
    return (
      <div className="space-y-4">
        <button type="button" onClick={() => navigate(`/${resourceName}`)} className="text-sm text-blue-600 hover:text-blue-800">
          &larr; Back to {resourceName}
        </button>
        <KeyPrompt resource={resourceName} onSubmit={handleKeySubmit} action="delete" />
      </div>
    );
  }

  if (isLoading) return <div className="text-sm text-gray-500 dark:text-gray-400 py-4">Looking up record...</div>;

  if (error) {
    return (
      <div className="space-y-4">
        <button type="button" onClick={() => navigate(`/${resourceName}`)} className="text-sm text-blue-600 hover:text-blue-800">
          &larr; Back to {resourceName}
        </button>
        <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 text-red-700 dark:text-red-400 rounded px-3 py-2 text-sm">
          {error}
        </div>
        <button type="button" onClick={() => setKey(null)} className="text-sm text-blue-600 hover:text-blue-800">
          Try a different key
        </button>
      </div>
    );
  }

  if (record) {
    return (
      <DeleteDialog
        resource={resourceName}
        record={record}
        onDeleted={() => navigate(`/${resourceName}`)}
        onCancel={() => navigate(`/${resourceName}`)}
      />
    );
  }

  return null;
};
