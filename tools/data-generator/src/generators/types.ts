/** Annotation entry from RESO metadata. */
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

/** A lookup value entry (one member of an enumeration). */
export interface ResoLookup {
  readonly lookupName: string;
  readonly lookupValue: string;
  readonly type: string;
  readonly annotations: ReadonlyArray<ResoAnnotation>;
}

/** Configuration for generating records of a single resource. */
export interface GeneratorConfig {
  readonly resource: string;
  readonly count: number;
  readonly relatedRecords?: Readonly<Record<string, number>>;
}

/** A plan describing all records to generate, in creation order. */
export interface SeedPlan {
  readonly resources: ReadonlyArray<GeneratorConfig>;
}

/** Result of a seed data generation run. */
export interface SeedResult {
  readonly created: number;
  readonly failed: number;
  readonly errors: ReadonlyArray<string>;
  readonly durationMs: number;
}

/** Authentication options for the OData server. */
export interface AuthConfig {
  readonly mode: 'token';
  readonly authToken: string;
}

/** Options for the generateSeedData function. */
export interface SeedOptions {
  readonly serverUrl: string;
  readonly resource: string;
  readonly count: number;
  readonly relatedRecords?: Readonly<Record<string, number>>;
  readonly auth: AuthConfig;
  /** Available fields for each resource (keyed by resource name). */
  readonly fieldsByResource: Readonly<Record<string, ReadonlyArray<ResoField>>>;
  /** Available lookup values (keyed by fully-qualified lookup name). */
  readonly lookupsByType: Readonly<Record<string, ReadonlyArray<ResoLookup>>>;
}

/** A function that generates records for a resource. */
export type RecordGenerator = (
  fields: ReadonlyArray<ResoField>,
  lookups: Readonly<Record<string, ReadonlyArray<ResoLookup>>>,
  count: number,
  parentResource?: string,
  parentKey?: string
) => ReadonlyArray<Record<string, unknown>>;
