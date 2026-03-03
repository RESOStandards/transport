import { useCallback, useState } from 'react';
import { useNavigate, useParams } from 'react-router';
import { createEntity } from '../api/client';
import { RecordForm } from '../components/record-form';
import { useMetadata } from '../hooks/use-metadata';
import { useUiConfig } from '../hooks/use-ui-config';
import type { ResourceName } from '../types';
import { TARGET_RESOURCES } from '../types';

/** Page for creating a new record. */
export const AddPage = () => {
  const { resource } = useParams<{ resource: string }>();
  const navigate = useNavigate();
  const resourceName = resource as ResourceName;

  const [isSubmitting, setIsSubmitting] = useState(false);
  const { fields, lookups, isLoading: metaLoading } = useMetadata(resourceName);
  const { fieldGroups } = useUiConfig();

  if (!TARGET_RESOURCES.includes(resourceName)) {
    return <div className="text-red-600 dark:text-red-400">Unknown resource: {resource}</div>;
  }

  const handleSubmit = useCallback(
    async (values: Record<string, unknown>) => {
      setIsSubmitting(true);
      try {
        await createEntity(resourceName, values);
        navigate(`/${resourceName}`);
      } finally {
        setIsSubmitting(false);
      }
    },
    [resourceName, navigate]
  );

  if (metaLoading) return <div className="text-sm text-gray-500 dark:text-gray-400 py-4">Loading metadata...</div>;

  return (
    <div className="space-y-4">
      <div>
        <button type="button" onClick={() => navigate(`/${resourceName}`)} className="text-sm text-blue-600 hover:text-blue-800 mb-1">
          &larr; Back to {resourceName}
        </button>
        <h2 className="text-xl font-semibold text-gray-900 dark:text-gray-100">Add {resourceName}</h2>
        <p className="text-sm text-gray-500 dark:text-gray-400 mt-1">Create a new {resourceName} record. The key will be auto-generated.</p>
      </div>

      <RecordForm
        resource={resourceName}
        fields={fields}
        lookups={lookups}
        fieldGroups={fieldGroups}
        onSubmit={handleSubmit}
        isLoading={isSubmitting}
      />
    </div>
  );
};
