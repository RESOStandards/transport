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
  readonly isExpansion?: boolean;
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
  /** When true, auto-generate dependency resources (Office, Member, etc.) in correct order. */
  readonly resolveDependencies?: boolean;
}

/** A function that generates records for a resource. */
export type RecordGenerator = (
  fields: ReadonlyArray<ResoField>,
  lookups: Readonly<Record<string, ReadonlyArray<ResoLookup>>>,
  count: number,
  parentResource?: string,
  parentKey?: string
) => ReadonlyArray<Record<string, unknown>>;

// ---------------------------------------------------------------------------
// FK resolution and multi-resource seed plan types
// ---------------------------------------------------------------------------

/** Map of resource names to their primary key field names (from RESO DD 2.0). */
export const KEY_FIELD_MAP: Readonly<Record<string, string>> = {
  Property: 'ListingKey',
  Member: 'MemberKey',
  Office: 'OfficeKey',
  Media: 'MediaKey',
  OpenHouse: 'OpenHouseKey',
  Showing: 'ShowingKey',
  Teams: 'TeamKey',
  TeamMembers: 'TeamMemberKey',
  OUID: 'OrganizationUniqueIdKey',
  PropertyGreenVerification: 'GreenBuildingVerificationKey',
  PropertyPowerProduction: 'PowerProductionKey',
  PropertyRooms: 'RoomKey',
  PropertyUnitTypes: 'UnitTypeKey',
  HistoryTransactional: 'HistoryTransactionalKey',
  SocialMedia: 'SocialMediaKey',
  OtherPhone: 'OtherPhoneKey'
};

/** A to-one FK binding discovered from metadata. */
export interface ForeignKeyBinding {
  /** The FK column on the source resource (e.g. "ListAgentKey"). */
  readonly fkColumn: string;
  /** The target resource (e.g. "Member"). */
  readonly targetResource: string;
  /** The target resource's primary key field (e.g. "MemberKey"). */
  readonly targetKeyField: string;
  /** The navigation property name (e.g. "ListAgent"). */
  readonly navPropName: string;
}

/** Dependency edge: source resource depends on target resource. */
export interface ResourceDependency {
  readonly sourceResource: string;
  readonly targetResource: string;
  /** FK bindings from source to target. */
  readonly bindings: ReadonlyArray<ForeignKeyBinding>;
}

/** A step in the multi-resource seed plan. */
export interface SeedPhase {
  readonly resource: string;
  readonly count: number;
  /** FK bindings from this resource to already-created resources. */
  readonly fkBindings: ReadonlyArray<ForeignKeyBinding>;
}

/** A back-fill step to update records after all creation phases complete. */
export interface BackFillPhase {
  readonly resource: string;
  /** FK bindings to back-fill (e.g. Office.OfficeBrokerKey → Member). */
  readonly fkBindings: ReadonlyArray<ForeignKeyBinding>;
}

/** A complete multi-resource seed plan with dependency ordering. */
export interface MultiResourceSeedPlan {
  /** Resources to create, in dependency order. */
  readonly phases: ReadonlyArray<SeedPhase>;
  /** Resources needing FK back-fills after all phases complete. */
  readonly backFillPhases: ReadonlyArray<BackFillPhase>;
  /** The originally requested resource. */
  readonly requestedResource: string;
  readonly requestedCount: number;
  /** Child collection records to generate per parent (e.g. Media:5). */
  readonly relatedRecords?: Readonly<Record<string, number>>;
}
