/** Annotation entry from RESO metadata (e.g., StandardName, Description). */
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

/** UI configuration served by the server at GET /ui-config. */
export interface UiConfig {
  readonly resources: Readonly<
    Record<
      string,
      {
        readonly summaryFields: ReadonlyArray<string> | '__all__';
      }
    >
  >;
}

/** Field group mapping served by the server at GET /field-groups. */
export type FieldGroups = Readonly<Record<string, Readonly<Record<string, ReadonlyArray<string>>>>>;

/** OData collection response shape. */
export interface ODataCollectionResponse {
  readonly '@odata.context'?: string;
  readonly '@odata.count'?: number;
  readonly '@odata.nextLink'?: string;
  readonly value: ReadonlyArray<Record<string, unknown>>;
}

/** OData error response shape. */
export interface ODataError {
  readonly error: {
    readonly code: string;
    readonly message: string;
    readonly details?: ReadonlyArray<{
      readonly code: string;
      readonly target?: string;
      readonly message: string;
    }>;
  };
}

/** The 6 target resources supported by the reference server. */
export const TARGET_RESOURCES = ['Property', 'Member', 'Office', 'Media', 'OpenHouse', 'Showing'] as const;
export type ResourceName = (typeof TARGET_RESOURCES)[number];

/** Map of resource names to their primary key field names. */
export const KEY_FIELD_MAP: Readonly<Record<ResourceName, string>> = {
  Property: 'ListingKey',
  Member: 'MemberKey',
  Office: 'OfficeKey',
  Media: 'MediaKey',
  OpenHouse: 'OpenHouseKey',
  Showing: 'ShowingKey'
};

/** Secondary ID fields (user-facing, not UUIDs). */
export const ID_FIELD_MAP: Readonly<Partial<Record<ResourceName, string>>> = {
  Property: 'ListingId',
  Member: 'MemberId',
  Office: 'OfficeId'
};

export { isEnumType, isNumericEdmType } from '@reso/validation';
