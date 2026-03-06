import type { ResoField, ResoLookup } from '../types';
import { isEnumType } from '../types';
import { getDisplayName } from '../utils/format';

interface FieldInputProps {
  readonly field: ResoField;
  readonly value: unknown;
  readonly onChange: (fieldName: string, value: unknown) => void;
  readonly lookups?: ReadonlyArray<ResoLookup>;
  readonly disabled?: boolean;
  readonly error?: string;
}

/** Type-aware input for a single RESO field. Renders the appropriate input element based on Edm type. */
export const FieldInput = ({ field, value, onChange, lookups, disabled = false, error }: FieldInputProps) => {
  const id = `field-${field.fieldName}`;
  const baseInputClass = `w-full px-2 py-1.5 border rounded text-sm bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100 focus:outline-none focus:ring-1 focus:ring-blue-500 ${
    error ? 'border-red-300 bg-red-50 dark:bg-red-900/20' : 'border-gray-300 dark:border-gray-600'
  }`;

  // Enum field with lookup values → select dropdown
  if (isEnumType(field.type) && lookups && lookups.length > 0) {
    return (
      <div>
        <label htmlFor={id} className="block text-xs text-gray-500 dark:text-gray-400 mb-0.5" title={getDisplayName(field)}>
          {getDisplayName(field)}
        </label>
        <select
          id={id}
          value={String(value ?? '')}
          onChange={e => onChange(field.fieldName, e.target.value || null)}
          disabled={disabled}
          className={baseInputClass}>
          <option value="">— Select —</option>
          {lookups.map(l => (
            <option key={l.lookupValue} value={l.lookupValue}>
              {l.lookupValue}
            </option>
          ))}
        </select>
        {error && <p className="text-xs text-red-600 dark:text-red-400 mt-0.5">{error}</p>}
      </div>
    );
  }

  // Boolean → checkbox
  if (field.type === 'Edm.Boolean') {
    return (
      <div>
        <label className="flex items-center gap-2 text-sm">
          <input
            type="checkbox"
            checked={Boolean(value)}
            onChange={e => onChange(field.fieldName, e.target.checked)}
            disabled={disabled}
            className="rounded border-gray-300 dark:border-gray-600"
          />
          <span className="text-gray-700 dark:text-gray-300" title={getDisplayName(field)}>
            {getDisplayName(field)}
          </span>
        </label>
        {error && <p className="text-xs text-red-600 dark:text-red-400 mt-0.5">{error}</p>}
      </div>
    );
  }

  // Date
  if (field.type === 'Edm.Date') {
    return (
      <div>
        <label htmlFor={id} className="block text-xs text-gray-500 dark:text-gray-400 mb-0.5" title={getDisplayName(field)}>
          {getDisplayName(field)}
        </label>
        <input
          id={id}
          type="date"
          value={String(value ?? '')}
          onChange={e => onChange(field.fieldName, e.target.value || null)}
          disabled={disabled}
          className={baseInputClass}
        />
        {error && <p className="text-xs text-red-600 dark:text-red-400 mt-0.5">{error}</p>}
      </div>
    );
  }

  // DateTimeOffset
  if (field.type === 'Edm.DateTimeOffset') {
    return (
      <div>
        <label htmlFor={id} className="block text-xs text-gray-500 dark:text-gray-400 mb-0.5" title={getDisplayName(field)}>
          {getDisplayName(field)}
        </label>
        <input
          id={id}
          type="datetime-local"
          value={String(value ?? '')}
          onChange={e => onChange(field.fieldName, e.target.value || null)}
          disabled={disabled}
          className={baseInputClass}
        />
        {error && <p className="text-xs text-red-600 dark:text-red-400 mt-0.5">{error}</p>}
      </div>
    );
  }

  // Numeric types
  if (['Edm.Int64', 'Edm.Int32', 'Edm.Int16', 'Edm.Byte', 'Edm.Decimal', 'Edm.Double', 'Edm.Single'].includes(field.type)) {
    const step = field.scale
      ? `0.${'0'.repeat(field.scale - 1)}1`
      : field.type.includes('Decimal') || field.type.includes('Double')
        ? '0.01'
        : '1';
    return (
      <div>
        <label htmlFor={id} className="block text-xs text-gray-500 dark:text-gray-400 mb-0.5" title={getDisplayName(field)}>
          {getDisplayName(field)}
        </label>
        <input
          id={id}
          type="number"
          step={step}
          value={value !== null && value !== undefined ? String(value) : ''}
          onChange={e => onChange(field.fieldName, e.target.value === '' ? null : Number(e.target.value))}
          disabled={disabled}
          className={baseInputClass}
        />
        {error && <p className="text-xs text-red-600 dark:text-red-400 mt-0.5">{error}</p>}
      </div>
    );
  }

  // Default: string input
  return (
    <div>
      <label htmlFor={id} className="block text-xs text-gray-500 mb-0.5" title={getDisplayName(field)}>
        {getDisplayName(field)}
      </label>
      <input
        id={id}
        type="text"
        value={String(value ?? '')}
        onChange={e => onChange(field.fieldName, e.target.value || null)}
        disabled={disabled}
        maxLength={field.maxLength}
        className={baseInputClass}
      />
      {error && <p className="text-xs text-red-600 dark:text-red-400 mt-0.5">{error}</p>}
    </div>
  );
};
