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

/** Top-level RESO metadata document structure. */
export interface ResoMetadata {
  readonly description: string;
  readonly version: string;
  readonly generatedOn: string;
  readonly resources: ReadonlyArray<ResoResource>;
  readonly fields: ReadonlyArray<ResoField>;
  readonly lookups: ReadonlyArray<ResoLookup>;
}

/** Map of resource names to their primary key field names (from DD 2.0). */
export const KEY_FIELD_MAP: Readonly<Record<string, string>> = {
  Property: 'ListingKey',
  Association: 'AssociationKey',
  ContactListingNotes: 'ContactKey',
  ContactListings: 'ContactListingsKey',
  Contacts: 'ContactKey',
  EntityEvent: 'EntityEventSequence',
  Field: 'FieldKey',
  HistoryTransactional: 'HistoryTransactionalKey',
  InternetTracking: 'EventKey',
  LockOrBox: 'LockOrBoxKey',
  Media: 'MediaKey',
  Member: 'MemberKey',
  MemberAssociation: 'AssociationKey',
  MemberStateLicense: 'MemberStateLicenseKey',
  Office: 'OfficeKey',
  OfficeAssociation: 'AssociationKey',
  OfficeCorporateLicense: 'OfficeCorporateLicenseKey',
  OpenHouse: 'OpenHouseKey',
  OtherPhone: 'OtherPhoneKey',
  OUID: 'OrganizationUniqueIdKey',
  PropertyGreenVerification: 'GreenBuildingVerificationKey',
  PropertyPowerProduction: 'PowerProductionKey',
  PropertyPowerStorage: 'PowerStorageKey',
  PropertyRooms: 'RoomKey',
  PropertyUnitTypes: 'UnitTypeKey',
  Prospecting: 'ProspectingKey',
  Queue: 'QueueTransactionKey',
  Rules: 'RuleKey',
  SavedSearch: 'SavedSearchKey',
  Showing: 'ShowingKey',
  ShowingAppointment: 'ShowingAppointmentKey',
  ShowingAvailability: 'ShowingAvailabilityKey',
  ShowingRequest: 'ShowingRequestKey',
  SocialMedia: 'SocialMediaKey',
  TeamMembers: 'TeamMemberKey',
  Teams: 'TeamKey',
  TransactionManagement: 'TransactionKey'
};

/** Resources targeted for the reference server. */
export const TARGET_RESOURCES: ReadonlyArray<string> = [
  'Property',
  'Member',
  'Office',
  'Media',
  'OpenHouse',
  'Showing',
  'PropertyGreenVerification',
  'PropertyPowerProduction',
  'PropertyRooms',
  'PropertyUnitTypes'
];
