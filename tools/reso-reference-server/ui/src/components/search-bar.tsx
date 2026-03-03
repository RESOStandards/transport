import { type FormEvent, useState } from 'react';

interface SearchBarProps {
  readonly initialFilter: string;
  readonly onSearch: (filter: string) => void;
  readonly onToggleAdvanced: () => void;
  readonly isAdvancedMode: boolean;
}

/** Simple OData $filter search bar with toggle for advanced mode. */
export const SearchBar = ({ initialFilter, onSearch, onToggleAdvanced, isAdvancedMode }: SearchBarProps) => {
  const [value, setValue] = useState(initialFilter);

  const handleSubmit = (e: FormEvent) => {
    e.preventDefault();
    onSearch(value.trim());
  };

  return (
    <form onSubmit={handleSubmit} className="flex flex-col sm:flex-row gap-2">
      <div className="flex-1 relative">
        <input
          type="text"
          value={value}
          onChange={e => setValue(e.target.value)}
          placeholder="OData $filter expression (e.g. ListPrice gt 500000)"
          className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded text-sm bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100 focus:outline-none focus:ring-1 focus:ring-blue-500 focus:border-blue-500"
        />
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
  );
};
