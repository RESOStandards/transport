import { type FormEvent, useCallback, useState } from 'react';
import type { FieldGroups, ResoField, ResoLookup, ResourceName } from '../types';
import { isEnumType, isNumericEdmType } from '../types';
import { FieldGroupSection } from './field-group-section';

interface AdvancedSearchProps {
  readonly resource: ResourceName;
  readonly fields: ReadonlyArray<ResoField>;
  readonly lookups: Readonly<Record<string, ReadonlyArray<ResoLookup>>>;
  readonly fieldGroups: FieldGroups | null;
  readonly onSearch: (filter: string) => void;
}

/** Comparison operators available for filter expressions. */
const OPERATORS = [
  { value: 'eq', label: '=' },
  { value: 'ne', label: '!=' },
  { value: 'gt', label: '>' },
  { value: 'ge', label: '>=' },
  { value: 'lt', label: '<' },
  { value: 'le', label: '<=' },
  { value: 'contains', label: 'contains' }
] as const;

interface FilterEntry {
  readonly field: string;
  readonly operator: string;
  readonly value: string;
}

/** Groups fields into sections matching the RESO Data Dictionary groupings. */
const groupFields = (
  fields: ReadonlyArray<ResoField>,
  resource: ResourceName,
  fieldGroups: FieldGroups | null
): { grouped: Map<string, ResoField[]>; ungrouped: ResoField[] } => {
  const grouped = new Map<string, ResoField[]>();
  const ungrouped: ResoField[] = [];
  const resourceGroups = fieldGroups?.[resource] ?? {};

  for (const field of fields) {
    const groups = resourceGroups[field.fieldName];
    if (groups && groups.length > 0) {
      const groupKey = groups[0];
      const existing = grouped.get(groupKey);
      if (existing) {
        existing.push(field);
      } else {
        grouped.set(groupKey, [field]);
      }
    } else {
      ungrouped.push(field);
    }
  }

  for (const fields of grouped.values()) {
    fields.sort((a, b) => a.fieldName.localeCompare(b.fieldName));
  }
  ungrouped.sort((a, b) => a.fieldName.localeCompare(b.fieldName));

  return { grouped, ungrouped };
};

/** Builds an OData $filter string from a list of filter entries. */
const buildFilterString = (entries: ReadonlyArray<FilterEntry>, fields: ReadonlyArray<ResoField>): string => {
  const fieldMap = new Map(fields.map(f => [f.fieldName, f]));
  const parts: string[] = [];

  for (const entry of entries) {
    if (!entry.value.trim()) continue;
    const field = fieldMap.get(entry.field);
    if (!field) continue;

    const isString = field.type === 'Edm.String' || isEnumType(field.type);
    const isNumeric = isNumericEdmType(field.type);
    const val = isString ? `'${entry.value.replace(/'/g, "''")}'` : isNumeric ? entry.value : `'${entry.value}'`;

    if (entry.operator === 'contains') {
      parts.push(`contains(${entry.field},${val})`);
    } else {
      parts.push(`${entry.field} ${entry.operator} ${val}`);
    }
  }

  return parts.join(' and ');
};

/** Advanced search form with fields organized by RESO Data Dictionary groups. */
export const AdvancedSearch = ({ resource, fields, lookups, fieldGroups, onSearch }: AdvancedSearchProps) => {
  const [filters, setFilters] = useState<Map<string, FilterEntry>>(new Map());
  const { grouped, ungrouped } = groupFields(fields, resource, fieldGroups);

  const handleChange = useCallback((fieldName: string, operator: string, value: string) => {
    setFilters(prev => {
      const next = new Map(prev);
      if (!value.trim() && operator === 'eq') {
        next.delete(fieldName);
      } else {
        next.set(fieldName, { field: fieldName, operator, value });
      }
      return next;
    });
  }, []);

  const handleSubmit = (e: FormEvent) => {
    e.preventDefault();
    const filterStr = buildFilterString([...filters.values()], fields);
    onSearch(filterStr);
  };

  const handleClear = () => {
    setFilters(new Map());
    onSearch('');
  };

  const renderFieldRow = (field: ResoField) => {
    const entry = filters.get(field.fieldName);
    const fieldLookups = isEnumType(field.type) ? lookups[field.type] : undefined;

    return (
      <div key={field.fieldName} className="flex flex-col sm:flex-row gap-1 sm:gap-2 items-start sm:items-center py-1">
        <span className="text-xs text-gray-600 dark:text-gray-400 w-full sm:w-48 shrink-0 truncate">{field.fieldName}</span>
        <select
          value={entry?.operator ?? 'eq'}
          onChange={e => handleChange(field.fieldName, e.target.value, entry?.value ?? '')}
          className="px-1 py-1 border border-gray-300 dark:border-gray-600 rounded text-xs w-full sm:w-20 bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100">
          {OPERATORS.map(op => (
            <option key={op.value} value={op.value}>
              {op.label}
            </option>
          ))}
        </select>
        {fieldLookups && fieldLookups.length > 0 ? (
          <select
            value={entry?.value ?? ''}
            onChange={e => handleChange(field.fieldName, entry?.operator ?? 'eq', e.target.value)}
            className="flex-1 px-2 py-1 border border-gray-300 dark:border-gray-600 rounded text-xs w-full bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100">
            <option value="">— Any —</option>
            {fieldLookups.map(l => (
              <option key={l.lookupValue} value={l.lookupValue}>
                {l.lookupValue}
              </option>
            ))}
          </select>
        ) : (
          <input
            type="text"
            value={entry?.value ?? ''}
            onChange={e => handleChange(field.fieldName, entry?.operator ?? 'eq', e.target.value)}
            placeholder="value"
            className="flex-1 px-2 py-1 border border-gray-300 dark:border-gray-600 rounded text-xs w-full bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100"
          />
        )}
      </div>
    );
  };

  const sortedGroups = [...grouped.entries()].sort((a, b) => a[0].localeCompare(b[0]));
  const activeFilters = [...filters.values()].filter(f => f.value.trim());

  return (
    <form onSubmit={handleSubmit} className="space-y-3">
      {activeFilters.length > 0 && (
        <div className="text-xs text-gray-500 dark:text-gray-400">
          {activeFilters.length} filter{activeFilters.length !== 1 ? 's' : ''} active
        </div>
      )}

      {sortedGroups.map(([group, groupFields]) => (
        <FieldGroupSection key={group} title={group}>
          {groupFields.map(renderFieldRow)}
        </FieldGroupSection>
      ))}

      {ungrouped.length > 0 && <FieldGroupSection title="Other">{ungrouped.map(renderFieldRow)}</FieldGroupSection>}

      <div className="flex gap-2 pt-2">
        <button type="submit" className="px-4 py-2 bg-blue-600 text-white text-sm rounded hover:bg-blue-700">
          Apply Filters
        </button>
        <button
          type="button"
          onClick={handleClear}
          className="px-4 py-2 text-sm border border-gray-300 dark:border-gray-600 text-gray-700 dark:text-gray-300 rounded hover:bg-gray-50 dark:hover:bg-gray-700">
          Clear
        </button>
      </div>
    </form>
  );
};
