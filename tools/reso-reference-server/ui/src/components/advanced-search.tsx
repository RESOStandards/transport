import { type FormEvent, useCallback, useEffect, useRef, useState } from 'react';
import type { FieldGroups, ResoField, ResoLookup, ResourceName } from '../types';
import { isEnumType, isNumericEdmType } from '../types';
import { type FilterEntry, buildFilterString, parseFilterToEntries } from '../utils/filter-sync.js';
import { getDisplayName } from '../utils/format';
import { FieldGroupSection } from './field-group-section';

interface AdvancedSearchProps {
  readonly resource: ResourceName;
  readonly fields: ReadonlyArray<ResoField>;
  readonly lookups: Readonly<Record<string, ReadonlyArray<ResoLookup>>>;
  readonly fieldGroups: FieldGroups | null;
  readonly filterString: string;
  readonly onFilterChange: (filter: string) => void;
  readonly onSearch: () => void;
}

/** All comparison operators. */
const OP_EQ = { value: 'eq', label: '=' } as const;
const OP_NE = { value: 'ne', label: '!=' } as const;
const OP_GT = { value: 'gt', label: '>' } as const;
const OP_GE = { value: 'ge', label: '>=' } as const;
const OP_LT = { value: 'lt', label: '<' } as const;
const OP_LE = { value: 'le', label: '<=' } as const;
const OP_CONTAINS = { value: 'contains', label: 'contains' } as const;

const OP_ANY = { value: 'any', label: 'any' } as const;
const OP_ALL = { value: 'all', label: 'all' } as const;

/** Operators valid for orderable types (numeric, date/time). */
const ORDERABLE_OPS = [OP_EQ, OP_NE, OP_GT, OP_GE, OP_LT, OP_LE] as const;
/** Operators valid for string fields. */
const STRING_OPS = [OP_EQ, OP_NE, OP_CONTAINS] as const;
/** Operators valid for equality-only types (enum, boolean, guid). */
const EQUALITY_OPS = [OP_EQ, OP_NE] as const;
/** Operators valid for collection fields (lambda). */
const COLLECTION_OPS = [OP_ANY, OP_ALL] as const;

const DATE_TYPES = new Set(['Edm.Date', 'Edm.DateTimeOffset', 'Edm.TimeOfDay']);

/** Returns the valid operators for a given field based on its Edm type. */
const getOperatorsForField = (field: ResoField): ReadonlyArray<{ readonly value: string; readonly label: string }> => {
  if (field.isCollection) return COLLECTION_OPS;
  if (isEnumType(field.type)) return EQUALITY_OPS;
  if (isNumericEdmType(field.type)) return ORDERABLE_OPS;
  if (DATE_TYPES.has(field.type)) return ORDERABLE_OPS;
  if (field.type === 'Edm.String') return STRING_OPS;
  if (field.type === 'Edm.Boolean') return EQUALITY_OPS;
  return EQUALITY_OPS;
};

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

/** Advanced search form with fields organized by RESO Data Dictionary groups. */
export const AdvancedSearch = ({ resource, fields, lookups, fieldGroups, filterString, onFilterChange, onSearch }: AdvancedSearchProps) => {
  const [filters, setFilters] = useState<Map<string, FilterEntry>>(new Map());
  const [hasUnrepresentable, setHasUnrepresentable] = useState(false);
  const lastEmittedRef = useRef('');
  const { grouped, ungrouped } = groupFields(fields, resource, fieldGroups);

  // Derive form state from incoming filter string (e.g. typed in search bar)
  useEffect(() => {
    if (filterString === lastEmittedRef.current) return;
    const result = parseFilterToEntries(filterString, fields);
    if (!result.parseError) {
      setFilters(new Map(result.entries));
      setHasUnrepresentable(result.hasUnrepresentable);
    }
  }, [filterString, fields]);

  const handleChange = useCallback(
    (fieldName: string, operator: string, value: string) => {
      setFilters(prev => {
        const next = new Map(prev);
        if (!value.trim()) {
          next.delete(fieldName);
        } else {
          next.set(fieldName, { field: fieldName, operator, value });
        }
        const newFilterStr = buildFilterString([...next.values()], fields);
        lastEmittedRef.current = newFilterStr;
        onFilterChange(newFilterStr);
        return next;
      });
    },
    [fields, onFilterChange]
  );

  const handleSubmit = (e: FormEvent) => {
    e.preventDefault();
    onSearch();
  };

  const handleClear = () => {
    setFilters(new Map());
    setHasUnrepresentable(false);
    lastEmittedRef.current = '';
    onFilterChange('');
    onSearch();
  };

  const renderFieldRow = (field: ResoField, index: number) => {
    const entry = filters.get(field.fieldName);
    const fieldLookups = isEnumType(field.type) ? lookups[field.type] : undefined;
    const operators = getOperatorsForField(field);
    const defaultOp = operators[0].value;
    const stripe = index % 2 === 1 ? 'bg-gray-100 dark:bg-gray-700/40' : '';

    if (field.isExpansion) {
      return (
        <div
          key={field.fieldName}
          className={`flex flex-col sm:flex-row gap-1 sm:gap-2 items-start sm:items-center py-2.5 px-2 rounded opacity-50 ${stripe}`}>
          <span className="text-xs text-gray-600 dark:text-gray-400 w-full sm:w-56 shrink-0 truncate" title={field.fieldName}>
            {getDisplayName(field)}
          </span>
          <span className="text-xs text-gray-400 dark:text-gray-500 italic">expansion — filtering not yet supported</span>
        </div>
      );
    }

    return (
      <div
        key={field.fieldName}
        className={`flex flex-col sm:flex-row gap-1 sm:gap-2 items-start sm:items-center py-2.5 px-2 rounded ${stripe}`}>
        <span className="text-xs text-gray-600 dark:text-gray-400 w-full sm:w-56 shrink-0 truncate" title={field.fieldName}>
          {getDisplayName(field)}
        </span>
        <select
          value={entry?.operator ?? defaultOp}
          onChange={e => handleChange(field.fieldName, e.target.value, entry?.value ?? '')}
          className="px-1 py-1 border border-gray-300 dark:border-gray-600 rounded text-xs w-full sm:w-20 bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100">
          {operators.map(op => (
            <option key={op.value} value={op.value}>
              {op.label}
            </option>
          ))}
        </select>
        {field.isCollection && fieldLookups && fieldLookups.length > 0 ? (
          <div className="flex-1 flex flex-wrap gap-1 w-full">
            {fieldLookups.map(l => {
              const selected = (entry?.value ?? '').split('|').filter(Boolean);
              const isSelected = selected.includes(l.lookupValue);
              return (
                <button
                  key={l.lookupValue}
                  type="button"
                  onClick={() => {
                    const next = isSelected ? selected.filter(v => v !== l.lookupValue) : [...selected, l.lookupValue];
                    handleChange(field.fieldName, entry?.operator ?? defaultOp, next.join('|'));
                  }}
                  className={`px-2 py-0.5 rounded text-xs border transition-colors ${
                    isSelected
                      ? 'bg-blue-600 text-white border-blue-600'
                      : 'bg-white dark:bg-gray-800 text-gray-700 dark:text-gray-300 border-gray-300 dark:border-gray-600 hover:border-blue-400 dark:hover:border-blue-500'
                  }`}>
                  {l.lookupValue}
                </button>
              );
            })}
          </div>
        ) : fieldLookups && fieldLookups.length > 0 ? (
          <select
            value={entry?.value ?? ''}
            onChange={e => handleChange(field.fieldName, entry?.operator ?? defaultOp, e.target.value)}
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
            onChange={e => handleChange(field.fieldName, entry?.operator ?? defaultOp, e.target.value)}
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
      {hasUnrepresentable && (
        <div className="text-xs text-amber-600 dark:text-amber-400 bg-amber-50 dark:bg-amber-900/20 border border-amber-200 dark:border-amber-800 rounded px-3 py-2">
          This filter contains expressions that can only be edited in the search bar.
        </div>
      )}

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

      {/* Ungrouped fields — flat list when no groupings exist, "Other" section otherwise */}
      {ungrouped.length > 0 && sortedGroups.length === 0 && <div>{ungrouped.map(renderFieldRow)}</div>}
      {ungrouped.length > 0 && sortedGroups.length > 0 && (
        <FieldGroupSection title="Other">{ungrouped.map(renderFieldRow)}</FieldGroupSection>
      )}

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
