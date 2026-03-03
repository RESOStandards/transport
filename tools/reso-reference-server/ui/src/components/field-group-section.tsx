import { type ReactNode, useState } from 'react';

interface FieldGroupSectionProps {
  readonly title: string;
  readonly defaultOpen?: boolean;
  readonly children: ReactNode;
}

/** Collapsible section for grouping fields by RESO Data Dictionary category. */
export const FieldGroupSection = ({ title, defaultOpen = false, children }: FieldGroupSectionProps) => {
  const [isOpen, setIsOpen] = useState(defaultOpen);

  return (
    <div className="border border-gray-200 dark:border-gray-700 rounded-lg overflow-hidden">
      <button
        type="button"
        onClick={() => setIsOpen(!isOpen)}
        className="w-full flex items-center justify-between px-4 py-2.5 bg-gray-50 dark:bg-gray-900 hover:bg-gray-100 dark:hover:bg-gray-700 text-left">
        <span className="text-sm font-medium text-gray-700 dark:text-gray-300">{title}</span>
        <span className="text-gray-400 dark:text-gray-500 text-xs">{isOpen ? '▲' : '▼'}</span>
      </button>
      {isOpen && <div className="p-4">{children}</div>}
    </div>
  );
};
