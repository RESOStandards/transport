import { useState } from 'react';
import { deleteEntity } from '../api/client';
import type { ResourceName } from '../types';
import { KEY_FIELD_MAP } from '../types';

interface DeleteDialogProps {
  readonly resource: ResourceName;
  readonly record: Record<string, unknown>;
  readonly onDeleted: () => void;
  readonly onCancel: () => void;
}

/** Confirmation dialog for deleting a record. Shows key info and requires confirmation. */
export const DeleteDialog = ({ resource, record, onDeleted, onCancel }: DeleteDialogProps) => {
  const [isDeleting, setIsDeleting] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const keyField = KEY_FIELD_MAP[resource];
  const key = String(record[keyField] ?? '');

  const handleDelete = async () => {
    setIsDeleting(true);
    setError(null);
    try {
      await deleteEntity(resource, key);
      onDeleted();
    } catch (err) {
      const odataErr = err as { error?: { message?: string } };
      const msg = odataErr?.error?.message ?? (err instanceof Error ? err.message : 'Failed to delete record. Please try again.');
      setError(msg);
    } finally {
      setIsDeleting(false);
    }
  };

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/40">
      <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 max-w-sm w-full mx-4">
        <h3 className="text-lg font-medium text-gray-900 dark:text-gray-100 mb-2">Delete {resource}?</h3>
        <p className="text-sm text-gray-600 dark:text-gray-400 mb-1">This will permanently delete the record:</p>
        <p className="text-sm font-mono bg-gray-50 dark:bg-gray-900 text-gray-900 dark:text-gray-100 rounded px-2 py-1 mb-4">
          {keyField}: {key}
        </p>

        {error && (
          <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 text-red-700 dark:text-red-400 rounded px-3 py-2 text-sm mb-3">
            {error}
          </div>
        )}

        <div className="flex gap-2 justify-end">
          <button
            type="button"
            onClick={onCancel}
            disabled={isDeleting}
            className="px-4 py-2 text-sm border border-gray-300 dark:border-gray-600 text-gray-700 dark:text-gray-300 rounded hover:bg-gray-50 dark:hover:bg-gray-700">
            Cancel
          </button>
          <button
            type="button"
            onClick={handleDelete}
            disabled={isDeleting}
            className="px-4 py-2 text-sm bg-red-600 text-white rounded hover:bg-red-700 disabled:opacity-50">
            {isDeleting ? 'Deleting...' : 'Delete'}
          </button>
        </div>
      </div>
    </div>
  );
};
