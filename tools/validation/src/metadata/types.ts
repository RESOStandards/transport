/** Annotation entry from RESO metadata (e.g., StandardName, Description, DDWikiUrl). */
export interface ResoAnnotation {
  readonly term: string;
  readonly value: string;
}

/** A field definition from the RESO Data Dictionary metadata. */
export interface ResoField {
  readonly resourceName: string;
  readonly fieldName: string;
  readonly type: string;
  readonly typeName?: string;
  readonly nullable?: boolean;
  readonly isCollection?: boolean;
  readonly maxLength?: number;
  readonly scale?: number;
  readonly precision?: number;
  readonly annotations: ReadonlyArray<ResoAnnotation>;
}

/** A validation failure for a single field. */
export interface ValidationFailure {
  readonly field: string;
  readonly reason: string;
}
