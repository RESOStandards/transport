import { type ReactNode, useEffect, useState } from 'react';

interface FieldGroupSectionProps {
  readonly title: string;
  readonly defaultOpen?: boolean;
  readonly errorCount?: number;
  readonly children: ReactNode;
}

/** Collapsible section for grouping fields by RESO Data Dictionary category. */
export const FieldGroupSection = ({ title, defaultOpen = false, errorCount = 0, children }: FieldGroupSectionProps) => {
  const [isOpen, setIsOpen] = useState(defaultOpen);

  // Auto-expand when errors appear in this group
  useEffect(() => {
    if (errorCount > 0) setIsOpen(true);
  }, [errorCount]);

  return (
    <div
      className={`border rounded-lg overflow-hidden ${errorCount > 0 ? 'border-red-300 dark:border-red-700' : 'border-gray-200 dark:border-gray-700'}`}>
      <button
        type="button"
        onClick={() => setIsOpen(!isOpen)}
        className="w-full flex items-center justify-between px-4 py-2.5 bg-gray-50 dark:bg-gray-900 hover:bg-gray-100 dark:hover:bg-gray-700 text-left">
        <span className="text-sm font-medium text-gray-700 dark:text-gray-300">
          {title}
          {errorCount > 0 && (
            <span className="ml-2 text-xs text-red-600 dark:text-red-400 font-normal">
              ({errorCount} {errorCount === 1 ? 'error' : 'errors'})
            </span>
          )}
        </span>
        <span className="text-gray-400 dark:text-gray-500 text-xs">{isOpen ? '▲' : '▼'}</span>
      </button>
      {isOpen && <div className="p-4">{children}</div>}
    </div>
  );
};
