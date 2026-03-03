/** Annotation entry from RESO metadata (e.g., StandardName, Description, DDWikiUrl). */
export interface ResoAnnotation {
  readonly term: string;
  readonly value: string;
}

/** A RESO resource definition (e.g., Property, Member, Office). */
export interface ResoResource {
  readonly resourceName: string;
  readonly wikiPageURL: string;
  readonly payloads: ReadonlyArray<string>;
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

/** Top-level RESO metadata document structure. */
export interface ResoMetadata {
  readonly description: string;
  readonly version: string;
  readonly generatedOn: string;
  readonly resources: ReadonlyArray<ResoResource>;
  readonly fields: ReadonlyArray<ResoField>;
  readonly lookups: ReadonlyArray<ResoLookup>;
}

/** Map of resource names to their primary key field names. */
export const KEY_FIELD_MAP: Readonly<Record<string, string>> = {
  Property: "ListingKey",
  Member: "MemberKey",
  Office: "OfficeKey",
  Media: "MediaKey",
  OpenHouse: "OpenHouseKey",
  Showing: "ShowingKey",
  Contacts: "ContactKey",
  ContactListings: "ContactListingsKey",
  HistoryTransactional: "HistoryTransactionalKey",
  InternetTracking: "InternetTrackingKey",
  SavedSearch: "SavedSearchKey",
  Prospecting: "ProspectingKey",
  Queue: "QueueTransactionKey",
  Rules: "RuleKey",
  Teams: "TeamKey",
  TeamMembers: "TeamMemberKey",
  OUID: "OrganizationUniqueId",
};

/** Resources targeted for the reference server. */
export const TARGET_RESOURCES: ReadonlyArray<string> = [
  "Property",
  "Member",
  "Office",
  "Media",
  "OpenHouse",
  "Showing",
];
