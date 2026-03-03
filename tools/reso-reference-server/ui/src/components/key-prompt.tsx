import { type FormEvent, useEffect, useRef, useState } from 'react';
import type { ResourceName } from '../types';
import { ID_FIELD_MAP, KEY_FIELD_MAP } from '../types';

interface KeyPromptProps {
  readonly resource: ResourceName;
  readonly onSubmit: (key: string) => void;
  readonly action: 'edit' | 'delete';
}

/** Prompt for entering a record key (for edit/delete operations). */
export const KeyPrompt = ({ resource, onSubmit, action }: KeyPromptProps) => {
  const [value, setValue] = useState('');
  const inputRef = useRef<HTMLInputElement>(null);
  const keyField = KEY_FIELD_MAP[resource];
  const idField = ID_FIELD_MAP[resource];

  useEffect(() => {
    inputRef.current?.focus();
  }, []);

  const handleSubmit = (e: FormEvent) => {
    e.preventDefault();
    const trimmed = value.trim();
    if (trimmed) onSubmit(trimmed);
  };

  return (
    <div className="max-w-md">
      <h2 className="text-lg font-medium text-gray-900 dark:text-gray-100 mb-4">
        {action === 'edit' ? 'Edit' : 'Delete'} {resource} Record
      </h2>
      <form onSubmit={handleSubmit} className="space-y-3">
        <div>
          <label htmlFor="key-input" className="block text-sm text-gray-600 dark:text-gray-400 mb-1">
            Enter the {keyField}
            {idField ? ` or ${idField}` : ''} of the record to {action}:
          </label>
          <input
            ref={inputRef}
            id="key-input"
            type="text"
            value={value}
            onChange={e => setValue(e.target.value)}
            placeholder={keyField}
            className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded text-sm bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100 focus:outline-none focus:ring-1 focus:ring-blue-500"
          />
        </div>
        <button
          type="submit"
          disabled={!value.trim()}
          className={`px-4 py-2 text-sm rounded text-white ${
            action === 'delete' ? 'bg-red-600 hover:bg-red-700 disabled:opacity-50' : 'bg-blue-600 hover:bg-blue-700 disabled:opacity-50'
          }`}>
          {action === 'edit' ? 'Load Record' : 'Find Record'}
        </button>
      </form>
    </div>
  );
};
