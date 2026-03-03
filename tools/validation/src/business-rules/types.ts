/** A range constraint for a specific field within a resource. */
export interface FieldRule {
  readonly fieldName: string;
  readonly min?: number;
  readonly max?: number;
  /** Custom error message. If omitted, a default is generated from min/max. */
  readonly message?: string;
}
