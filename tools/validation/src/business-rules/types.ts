import type { ValidationFailure } from '../metadata/types.js';

/** A per-field constraint for a specific field within a resource. */
export interface FieldRule {
  readonly fieldName: string;
  /** Pattern to match multiple field names. When set, fieldName is a descriptive label. */
  readonly fieldPattern?: RegExp;
  readonly required?: boolean;
  readonly min?: number;
  readonly max?: number;
  /** Custom error message. If omitted, a default is generated from min/max or required. */
  readonly message?: string;
}

/** A cross-field constraint that validates relationships between multiple fields. */
export interface CrossFieldRule {
  readonly name: string;
  readonly validate: (body: Readonly<Record<string, unknown>>) => ValidationFailure | null;
}
