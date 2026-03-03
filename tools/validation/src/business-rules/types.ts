import type { ValidationFailure } from '../metadata/types.js';

/** A range constraint for a specific field within a resource. */
export interface FieldRule {
  readonly fieldName: string;
  readonly min?: number;
  readonly max?: number;
  /** Custom error message. If omitted, a default is generated from min/max. */
  readonly message?: string;
}

/** A cross-field constraint that validates relationships between multiple fields. */
export interface CrossFieldRule {
  readonly name: string;
  readonly validate: (body: Readonly<Record<string, unknown>>) => ValidationFailure | null;
}
