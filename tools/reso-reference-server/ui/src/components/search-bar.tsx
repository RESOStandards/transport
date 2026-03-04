import { type FormEvent, useCallback, useState } from 'react';

interface SearchBarProps {
  readonly value: string;
  readonly onChange: (value: string) => void;
  readonly onSearch: () => void;
  readonly onToggleAdvanced: () => void;
  readonly isAdvancedMode: boolean;
  readonly validationError: string | null;
}

/** OData $filter search bar with validation display and clipboard copy. */
export const SearchBar = ({ value, onChange, onSearch, onToggleAdvanced, isAdvancedMode, validationError }: SearchBarProps) => {
  const [copied, setCopied] = useState(false);

  const handleSubmit = (e: FormEvent) => {
    e.preventDefault();
    onSearch();
  };

  const handleCopy = useCallback(() => {
    navigator.clipboard.writeText(value).then(() => {
      setCopied(true);
      setTimeout(() => setCopied(false), 1500);
    });
  }, [value]);

  const isLong = value.length > 80;

  return (
    <div>
      <form onSubmit={handleSubmit} className="flex flex-col sm:flex-row gap-2">
        <div className="flex-1 relative">
          <input
            type="text"
            value={value}
            onChange={e => onChange(e.target.value)}
            placeholder="OData $filter expression (e.g. ListPrice gt 500000)"
            className={`w-full px-3 py-2 border rounded text-sm bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100 focus:outline-none focus:ring-1 focus:ring-blue-500 focus:border-blue-500 ${
              validationError ? 'border-red-400 dark:border-red-600' : 'border-gray-300 dark:border-gray-600'
            } ${isLong ? 'pr-9' : ''}`}
          />
          {isLong && (
            <button
              type="button"
              onClick={handleCopy}
              title={copied ? 'Copied!' : 'Copy filter to clipboard'}
              className="absolute right-2 top-1/2 -translate-y-1/2 text-gray-400 hover:text-gray-600 dark:hover:text-gray-300">
              {copied ? (
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
          )}
        </div>
        <div className="flex gap-2">
          <button type="submit" className="px-4 py-2 bg-blue-600 text-white text-sm rounded hover:bg-blue-700 whitespace-nowrap">
            Search
          </button>
          <button
            type="button"
            onClick={onToggleAdvanced}
            className={`px-3 py-2 text-sm rounded border whitespace-nowrap ${
              isAdvancedMode
                ? 'bg-blue-50 dark:bg-blue-900/30 border-blue-300 dark:border-blue-600 text-blue-700 dark:text-blue-400'
                : 'border-gray-300 dark:border-gray-600 text-gray-600 dark:text-gray-400 hover:bg-gray-50 dark:hover:bg-gray-700'
            }`}>
            Advanced
          </button>
        </div>
      </form>
      {validationError && (
        <div className="text-xs text-red-600 dark:text-red-400 mt-1.5 font-mono whitespace-pre-wrap">{validationError}</div>
      )}
    </div>
  );
};
