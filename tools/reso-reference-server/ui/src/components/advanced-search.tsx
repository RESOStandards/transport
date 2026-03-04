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
  // Guid and any other types — equality only
  return EQUALITY_OPS;
};

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

    if (entry.operator === 'any' || entry.operator === 'all') {
      const values = entry.value.split('|').filter(v => v.trim());
      if (values.length === 0) continue;
      const quoted = values.map(v => `'${v.replace(/'/g, "''")}'`);
      if (entry.operator === 'any') {
        // any: at least one of the selected values exists in the collection
        const inner = quoted.map(q => `x eq ${q}`).join(' or ');
        parts.push(`${entry.field}/any(x:${inner})`);
      } else {
        // all: every selected value exists in the collection
        for (const q of quoted) {
          parts.push(`${entry.field}/any(x:x eq ${q})`);
        }
      }
    } else if (entry.operator === 'contains') {
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
      if (!value.trim()) {
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

  const renderFieldRow = (field: ResoField, index: number) => {
    const entry = filters.get(field.fieldName);
    const fieldLookups = isEnumType(field.type) ? lookups[field.type] : undefined;
    const operators = getOperatorsForField(field);
    const defaultOp = operators[0].value;
    const stripe = index % 2 === 1 ? 'bg-gray-100 dark:bg-gray-700/40' : '';

    return (
      <div
        key={field.fieldName}
        className={`flex flex-col sm:flex-row gap-1 sm:gap-2 items-start sm:items-center py-2.5 px-2 rounded ${stripe}`}>
        <span className="text-xs text-gray-600 dark:text-gray-400 w-full sm:w-48 shrink-0 truncate">{field.fieldName}</span>
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
