# RESO Organization and System Identifiers Endorsement

| **RCP** | 55 |
| :--- | :--- |
| **Version** | **1.0.0** |
| **Authors** | [Josh Darnell (RESO)](mailto:josh@reso.org) |
| **Status** | IN PROGRESS |
| **Date Ratified** | TBD |
| **Dependencies** | [Data Dictionary 1.7+](https://ddwiki.reso.org/display/DDW17/Data+Dictionary+1.7+Wiki) |


<br />

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

<br />

# Table of Contents
- [Summary of Changes](#summary-of-changes)
- [Introduction](#introduction)
- [Section 1: Purpose](#section-1-purpose)
- [Section 2: Specification](#section-2-specification)
- [Section 3: Certification](#section-3-certification)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: Appendices](#section-6-appendices)
- [Section 7: License](#section-7-license)

<br />

# Summary of Changes
* Introduces the **Organization Resource** and the RESO **Unique Organization Identifier (UOI)**, deprecating and replacing the OUID Resource. Most OUID properties port directly.
* Introduces the **System Resource** and the RESO **Unique System Identifier (USI)**, formalizing the system identifiers RESO already maintains. The identifier is distinct from the resource's primary key.
* Defines **authoritative** (RESO-maintained) identifiers and a mechanism for providers to host **local** Organization and System resources when they use identifiers not yet issued by RESO.
* Standardizes **`OriginatingUoi`/`OriginatingUsi`** and **`SourceUoi`/`SourceUsi`** as top-level identifiers. The existing `OriginatingSystem*` and `SourceSystem*` fields are deprecated in Data Dictionary v3.0; providers MAY continue to use them only if the UOI/USI analogues are also present.
* Models organization and system **lifecycle status** as an enumeration (`Active`, `Inactive`, `Superseded`) with a `SupersededBy` reference for merges and reclassifications, and adds a typed **`RelatedOrganization`** expansion for relationships between organizations.
* Standardizes UOIs and USIs as **URNs** – `urn:reso:uoi:1.0:<issuer>:<unique-identifier>` and the USI analogue – issued by RESO or by providers certified on this endorsement, so an organization's existing local identifiers are preserved and can be promoted to RESO-issued identifiers over time.

<br />

# Introduction

Real estate records routinely pass through more than one organization and system before reaching an end user. To use a record, a consumer needs to know which organization and which system it came from. Today that information is carried in fields such as `OriginatingSystemName` and `SourceSystemName`, but the values are not standardized – and `OriginatingSystemName` is, in practice, a local vendor short-code for an *organization*, not a system. Because every provider spells these codes differently, each consumer maintains its own set of mappings for every provider it works with, even when two providers are naming the same organization.

This endorsement standardizes the identifiers themselves. It defines a RESO **Unique Organization Identifier (UOI)** and a RESO **Unique System Identifier (USI)**, each backed by a resource, so an organization or system is named the same way everywhere. It extends RESO's family of identifiers – the **Unique Licensee Identifier** (ULI, RCP-54) for licensed professionals and the **Universal Parcel Identifier** ([UPI](https://upi.reso.org/)) for parcels – to the organizations and systems behind the data.

RESO already issues and relies on UOIs and USIs at scale in Certification and RESO Analytics. This endorsement formalizes that established practice so the same identifiers can be used consistently across the Data Dictionary and the Data Provenance endorsement (RCP-50).

<br />

# Section 1: Purpose

The RESO Data Dictionary carries `OriginatingSystem` and `SourceSystem` `Name`, `ID`, and `Key` fields in each resource to describe where a record originated and where it was obtained. Two problems recur with this approach:

* The names, IDs, and keys used to describe organizations and systems are not standardized, which forces custom mapping and programming work for every provider.
* The identity being carried is ambiguous – `OriginatingSystemName` typically names an *organization*, not a system – so a consumer cannot reliably separate an organization from the system that served it.

This endorsement addresses both by defining standard, resolvable identifiers for organizations (UOI) and systems (USI), a resource for each, the rules for issuing them authoritatively or hosting them locally, and standard top-level fields that replace the `OriginatingSystem`/`SourceSystem` values.

<br />

# Section 2: Specification

## Section 2.1: Organization Resource (UOI)

Organizations are currently modeled by the [OUID Resource](https://ddwiki.reso.org/display/DDW20/OUID+Resource). This endorsement deprecates the OUID Resource in favor of a new **Organization Resource** and standardizes the **Unique Organization Identifier (UOI)** it carries. Most OUID properties port directly.

Once ratified, current implementations MAY continue to expose the OUID Resource, but MUST implement the Organization Resource and populate the UOI when present.

The Organization Resource defines at least the following:

* **OrganizationKey** – String, non-nullable. The unique local key of the organization.
* **OrganizationId** – String, non-nullable. The Unique Organization Identifier (UOI).
* **OrganizationName** – String, nullable. The organization name.
* **OrganizationStatus** – String, non-nullable. The organization's lifecycle status: `Active`, `Inactive`, or `Superseded`. See [Section 2.3](#section-23-identifier-format-and-lifecycle).
* **SupersededByUoi** – String, nullable. When `OrganizationStatus` is `Superseded`, the UOI that supersedes this one; otherwise null. See [Section 2.3](#section-23-identifier-format-and-lifecycle).
* **OrganizationStatusChangeTimestamp** – Timestamp, nullable. When the organization's status last changed.
* **OrganizationComments** – String, nullable. Free-text narrative about the organization, for example a merger or reclassification history. Informational only; see [Section 2.3](#section-23-identifier-format-and-lifecycle).
* **ModificationTimestamp** – Timestamp, non-nullable. When the organization record was last updated.
* Any relevant OUID Resource attributes.

`OrganizationKey` is the local key of the record; `OrganizationId` is the well-known RESO identifier, when applicable.

**Related organizations.** An Organization MAY reference other organizations through a **`RelatedOrganization`** self-expansion, retrieved with `$expand=RelatedOrganization`. These relationships are directed graph edges among peer organizations: they describe how organizations relate, not ownership or control. An edge is read from the hosting organization outward, so an edge whose `RelationshipType` is `ServedBy` and whose `RelatedUoi` is another organization states that this organization is served by that one. This replaces the single-purpose association-to-MLS reference carried in the current OUID data, and follows the same pattern as the RelatedLookup Resource (RCP-47).

The `RelatedOrganization` expansion and its entries define at least the following:

* **RelatedOrganization** – Expansion, nullable. A self-expansion on the Organization Resource, retrieved with `$expand=RelatedOrganization`. Each entry is a directed, typed edge from this organization to another. Null or absent when the organization references no other organizations.
* **RelatedUoi** – String, non-nullable. On a `RelatedOrganization` entry, the UOI of the referenced organization.
* **RelationshipType** – String, non-nullable. On a `RelatedOrganization` entry, the type of the relationship, for example `ParticipatesIn`, `ServedBy`, or `AffiliatedWith`. The edge is read from the hosting organization outward.

## Section 2.2: System Resource (USI)

This endorsement defines a new **System Resource** to model the systems that produce and carry records, and standardizes the **Unique System Identifier (USI)** it carries. RESO already maintains a list of system identifiers (with `UniqueSystemId`, `SystemName`, and `IsActive`); the fields below align that list to current Data Dictionary conventions.

A System Resource MAY model any relevant system properties, but MUST define at least an identifier. The identifier is **not** the primary key: the primary key (`SystemKey`) is internal to a single system, whereas the identifier (`SystemId`, the USI) is well-known and spans systems. Each system is provided by an organization – its provider – identified by that organization's UOI.

The System Resource defines at least the following:

* **SystemKey** – String, non-nullable. The unique local key of the system.
* **SystemId** – String, non-nullable. The Unique System Identifier (USI).
* **ProviderUoi** – String, non-nullable. The UOI of the organization that provides the system.
* **SystemName** – String, nullable. The system name.
* **SystemStatus** – String, non-nullable. The system's lifecycle status: `Active`, `Inactive`, or `Superseded`. See [Section 2.3](#section-23-identifier-format-and-lifecycle).
* **SupersededByUsi** – String, nullable. When `SystemStatus` is `Superseded`, the USI that supersedes this one; otherwise null. See [Section 2.3](#section-23-identifier-format-and-lifecycle).
* **ModificationTimestamp** – Timestamp, non-nullable. When the system record was last updated.

Other attributes MAY be added through separate proposals.

## Section 2.3: Identifier Format and Lifecycle

**Format.** Each Unique Organization Identifier and Unique System Identifier is a URN. A UOI has the form

```
urn:reso:uoi:1.0:<issuer>:<unique-identifier>
```

and a USI the parallel form `urn:reso:usi:1.0:<issuer>:<unique-identifier>`, where:

* **`1.0`** is the version of the identifier scheme.
* **`<issuer>`** is a RESO-assigned identifier for the organization that issued this identifier. The issuer MUST be an organization in RESO's UOI service; a consumer confirms an identifier by confirming its issuer is a known RESO organization. RESO is the root issuer, so an organization's own UOI is one that RESO issues.
* **`<unique-identifier>`** is the value the issuing organization assigns, in any format it chooses, percent-encoded per the URN rules so that reserved characters – notably the `:` segment delimiter – are carried safely.

Consumers SHOULD treat the identifier as opaque past its scheme: the `urn:reso:uoi` or `urn:reso:usi` prefix types it, and the `<unique-identifier>` carries no consumer-parseable meaning. The scheme aligns with the `urn:reso:` identifiers RESO already uses in the [RESO Common Format](https://transport.reso.org/proposals/reso-common-format/) (RCP-25).

**Local identifiers and migration.** Because `<unique-identifier>` is issuer-defined, an organization keeps its own identifiers until they enter RESO's registry. An existing local identifier is preserved unchanged as the tail of a URN issued under a RESO organization, so adopting the scheme changes how identifiers are formed at the transport layer without requiring the systems underneath to renumber. When an entity identified this way later receives its own RESO-issued UOI, the URN carrying the local identifier is superseded by that new UOI through the lifecycle below.

> **Open item (Workgroup decision).** The internal format of the RESO-assigned token in the `<issuer>` and RESO-issued `<unique-identifier>` segments – whether to keep the current organization-type letter prefix (for example `T00000012`) or move to an opaque fixed-width token with a legacy mapping – is pending a Workgroup decision. The URN scheme accommodates either.

**Lifecycle.** A UOI or USI is created, updated, deactivated, or superseded, and is never removed. `OrganizationStatus` and `SystemStatus` each take one of three values:

* **`Active`** – current and in use.
* **`Inactive`** – retired with no successor, for example dissolved or closed.
* **`Superseded`** – replaced by another identifier, for example through a merger, acquisition, or reclassification.

When the status is `Superseded`, `SupersededByUoi` or `SupersededByUsi` MUST be populated; when it is `Active` or `Inactive`, that field MUST be null.

**Supersession and resolution.** A superseded record is retained, and its `SupersededBy` identifier points to the replacement, forming a redirect chain. A consumer holding a superseded identifier resolves to the current one by following the `SupersededBy` reference to the end of the chain. This mirrors the tombstone-and-redirect model of the Unique Licensee Identifier (ULI, RCP-54). Human narrative about a change, such as a merger history, MAY be carried in a free-text comment, but the machine-resolvable state is the status and `SupersededBy` fields.

## Section 2.4: Authoritative and Local Identifiers

RESO maintains authoritative Unique Organization and System Identifiers – primarily real estate associations, MLSs, and their technology providers – in both spreadsheet and JSON formats, which are kept current and used in Certification and RESO Analytics. Records can be created, updated, deactivated, or superseded, but not removed. New authoritative identifiers can be created by [contacting RESO](mailto:support@reso.org?subject=UOI%20%2F%20USI%20Request&body=Organization%20or%20system%20name%3A%0D%0AWebsite%3A%0D%0AContact%20name%20and%20email%3A%0D%0AIdentifier%20requested%20%28UOI%2C%20USI%2C%20or%20both%29%3A%0D%0AExisting%20local%20identifier%2C%20if%20any%3A).

**Trusted issuers.** A provider becomes a trusted issuer by passing the Organization and System endorsement. On certification it is recorded as holding the UOI and USI endorsements in RESO's [Organizations and Endorsements feed](https://www.reso.org/certification/), which is the authoritative allow-list of issuers. A consumer validates an identifier's `<issuer>` by confirming the issuer holds the corresponding endorsement in that feed. RESO is the root issuer and issues without the endorsement, since it is the body that grants it.

A trusted issuer MAY issue its own identifiers – the `<unique-identifier>` tail under its issuer segment – for organizations and systems not carrying a RESO-issued identifier. In that case the issuer MUST host instances of the Organization and System resources exposing `OrganizationId` / `SystemId` (the identifier MAY equal the key) along with `ModificationTimestamp`; `OrganizationName` and `SystemName` MAY be null.

**Organization Resource**

**REQUEST**
```
GET https://api.example.com/Organization?$select=OrganizationId,OrganizationName,ModificationTimestamp&$filter=OrganizationId eq 'urn:reso:uoi:1.0:T00000045:local-org-1'
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "https://api.example.com/Organization?$select=OrganizationId,OrganizationName,ModificationTimestamp&$filter=OrganizationId eq 'urn:reso:uoi:1.0:T00000045:local-org-1'",
  "value": [
    {
      "OrganizationId": "urn:reso:uoi:1.0:T00000045:local-org-1",
      "OrganizationName": "Name of local organization",
      "ModificationTimestamp": "2024-12-16T20:34:47Z"
    }
  ]
}
```

**System Resource**

**REQUEST**
```
GET https://api.example.com/System?$select=SystemId,SystemName,ModificationTimestamp&$filter=SystemId eq 'urn:reso:usi:1.0:T00000045:local-system-1'
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "https://api.example.com/System?$select=SystemId,SystemName,ModificationTimestamp&$filter=SystemId eq 'urn:reso:usi:1.0:T00000045:local-system-1'",
  "value": [
    {
      "SystemId": "urn:reso:usi:1.0:T00000045:local-system-1",
      "SystemName": "Name of local system",
      "ModificationTimestamp": "2024-12-16T20:34:47Z"
    }
  ]
}
```

End users may not have access to a provider's local Organization and System resources. However, the organization that issued those local identifiers will itself have a well-known RESO UOI, which appears wherever the record is described; a consumer can contact that organization if more information is needed.

## Section 2.5: Originating and Source Identifiers

The Data Dictionary's `OriginatingSystem` and `SourceSystem` fields carry the organization and system a record came from and was obtained from. This endorsement standardizes those as UOI/USI pairs at the top level of each resource that supports them:

* **OriginatingUoi** – String, nullable. The UOI of the organization where the record originated. Required when the originating organization is known.
* **OriginatingUsi** – String, nullable. The USI of the system where the record originated. Null when the originating system is not known.
* **SourceUoi** – String, nullable. The UOI of the organization the current provider obtained the record from. Required when the source organization is known.
* **SourceUsi** – String, nullable. The USI of the system the record was obtained from. Null when that system is not known.

An organization identifier MAY be present without its system identifier. Because `OriginatingSystemName` and `SourceSystemName` name an *organization* today, a provider migrating those values can populate the `*Uoi` fields even when the `*Usi` is unknown.

These fields are usable on their own and do **not** require the Data Provenance endorsement (RCP-50); they carry the same originating- and source-identification that `OriginatingSystemName`/`ID` and `SourceSystemName`/`ID` carry today, using standard identifiers.

**Deprecation.** `OriginatingSystemName`, `OriginatingSystemID`, `OriginatingSystemKey`, `SourceSystemName`, `SourceSystemID`, and `SourceSystemKey` are deprecated in Data Dictionary v3.0. A provider MAY continue to populate them on a record, but only if that record also carries the organization analogue: any populated `OriginatingSystem*` field requires `OriginatingUoi`, and any populated `SourceSystem*` field requires `SourceUoi`, with the matching `*Usi` when the system is known.

**Consistency with Provenance.** When Provenance is present, the top-level identifiers MUST agree with the ends of the provenance chain: `OriginatingUoi`/`OriginatingUsi` MUST equal the `ProviderUoi`/`ProviderUsi` of the earliest (origin) Provenance record, and `SourceUoi`/`SourceUsi` MUST equal the `ProviderUoi`/`ProviderUsi` of the latest record – the hop the current provider obtained the record from. For a single-record chain the origin and source coincide. Provenance describes the full chain in between; its ends MUST NOT contradict the top-level fields.

<br />

# Section 3: Certification

RESO will validate the following during certification:

* When an Organization or System resource is hosted, it MUST be defined with the required identifier fields – `OrganizationKey`/`OrganizationId` or `SystemKey`/`SystemId` – along with `ModificationTimestamp`.
* The System Resource MUST define `SystemKey` (the primary key) and `SystemId` (the USI) as distinct fields.
* Identifier resolution keys off the `<issuer>` segment. A RESO-issued identifier resolves against the RESO authoritative registry described in Section 2.4. A provider-issued identifier resolves against that issuer's hosted Organization or System resource. When the issuer is a third party – neither RESO nor the endpoint under test – the identifier is validated against the RESO authoritative registry only, since the endpoint is not obligated to host another party's resources.
* The `<issuer>` segment of any UOI or USI MUST identify RESO or an organization holding the UOI or USI endorsement in RESO's Organizations and Endorsements feed.
* A provider that issues its own UOIs or USIs MUST host correctly implemented Organization and System resources for the identifiers it issues; RESO samples those resources during certification to confirm the issued identifiers resolve and the resources conform.
* `OriginatingUoi`/`OriginatingUsi`, `SourceUoi`/`SourceUsi`, any `SupersededByUoi`/`SupersededByUsi`, and any `RelatedUoi`, when present, MUST resolve as above.
* A `RelatedOrganization` entry, when present, MUST carry both `RelatedUoi` and `RelationshipType`.
* When `OrganizationStatus` or `SystemStatus` is `Superseded`, the corresponding `SupersededByUoi` or `SupersededByUsi` MUST be present; when the status is `Active` or `Inactive`, it MUST be null.
* On any record where a deprecated `OriginatingSystem*` field is populated, `OriginatingUoi` MUST be populated; likewise a populated `SourceSystem*` field requires `SourceUoi`.
* When Provenance is present, the top-level `OriginatingUoi`/`OriginatingUsi` and `SourceUoi`/`SourceUsi` MUST match the ends of the provenance chain (the ends-match rule of Section 2.5).

<br />

# Section 4: Contributors
This document was written by [Joshua Darnell](mailto:josh@reso.org).

<br />

# Section 5: References

Please see the following references for more information regarding topics covered in this document:
* [RESO Unique Organization Identifier (UOI)](https://www.reso.org/reso-unique-identifiers/)
* [RESO Certification and the Organizations and Endorsements feed](https://www.reso.org/certification/)
* [RESO Common Format (RCP-25)](https://transport.reso.org/proposals/reso-common-format/)
* [RESO Data Provenance Endorsement (RCP-50)](./data-provenance.md)
* [RESO Unique Licensee Identifier (ULI, RCP-54)](./uli-resolution-protocol.md)

<br />

# Section 6: Appendices

## Design rationale

**Sub-organization granularity is handled by Originating at grain, not a dedicated field.** Some data sets bundle many sub-organizations under a single originating organization, and a provider may need to scope to one of them. This endorsement handles that case with `OriginatingUoi` at the appropriate grain – the identifier points at the actual creating organization, however specific – rather than adding a per-record sub-organization field. Organization parentage is separately expressible through the `RelatedOrganization` self-expansion (Section 2.1) as an `AffiliatedWith` edge, which keeps the hierarchy in one place and resolvable. A denormalized `OriginatingSubUoi`/`SourceSubUoi` field pair alongside the top-level identifiers was also considered – it matches how some providers filter today but adds a column to every record – and is held for a future revision; `OriginatingSubUoi` and `SourceSubUoi` are reserved as the names should the field pair be needed.

**Top-level identifiers, not Tenant/Subtenant.** An earlier approach modeled multi-tenant filtering with `TenantUoi`/`SubtenantUoi`. Both uses that motivated it – de-multiplexing a combined feed by origin, and scoping to one organization within a grant – are served today by `OriginatingSystemName`, so they are served here by `OriginatingUoi`/`OriginatingUsi`. Originating is the more intuitive, 1:1 migration from the existing fields, is usable before Provenance is adopted, and avoids introducing tenancy vocabulary into the Data Dictionary. Tenancy as an access-and-partitioning concern is left to the layer that governs access.

**Status as an enumeration, and relationships as neutral edges.** The authoritative registry historically carried organization status as a Boolean and recorded mergers and reclassifications only in free-text comments, which are not machine-resolvable. This endorsement models status as an enumeration – `Active`, `Inactive`, `Superseded` – paired with a `SupersededBy` reference, so a consumer can resolve a retired identifier to its current one without parsing prose. Relationships between organizations are modeled as directed, typed graph edges (`RelatedOrganization`) rather than ownership statements: an edge records that two organizations are related and how, not that one organization owns or controls another. The `RelationshipType` vocabulary is intentionally functional – `ParticipatesIn`, `ServedBy`, `AffiliatedWith` – to avoid language that could imply control in a real estate context.

## Worked example

The following shows an Organization Resource response, a System Resource response, a record whose top-level identifiers reconcile against a Provenance chain, and an organization mid-promotion from a locally issued identifier to a RESO-issued one. Identifiers are shown in their URN form; values are illustrative.

**Organization Resource** – the technology provider that served the record, expanded (`$expand=RelatedOrganization`) to show an `AffiliatedWith` edge to its parent organization.

**RESPONSE**
```json
{
  "OrganizationKey": "T00000045",
  "OrganizationId": "urn:reso:uoi:1.0:T00000012:T00000045",
  "OrganizationName": "Example Technology Provider",
  "OrganizationStatus": "Active",
  "OrganizationStatusChangeTimestamp": "2019-03-11T00:00:00Z",
  "ModificationTimestamp": "2024-11-02T18:22:10Z",
  "RelatedOrganization": [
    {
      "RelatedUoi": "urn:reso:uoi:1.0:T00000012:T00000009",
      "RelationshipType": "AffiliatedWith"
    }
  ]
}
```

**System Resource** – one of that provider's systems. The identifier (`SystemId`, the USI) is distinct from the primary key (`SystemKey`).

**RESPONSE**
```json
{
  "SystemKey": "example-web-api",
  "SystemId": "urn:reso:usi:1.0:T00000012:50011",
  "ProviderUoi": "urn:reso:uoi:1.0:T00000012:T00000045",
  "SystemName": "Example Web API",
  "SystemStatus": "Active",
  "ModificationTimestamp": "2024-11-02T18:22:10Z"
}
```

**Record with Provenance** – the record originated at an MLS and was obtained from the technology provider. The top-level identifiers equal the ends of the chain: `OriginatingUoi`/`OriginatingUsi` match the earliest Provenance record, and `SourceUoi`/`SourceUsi` match the latest.

**RESPONSE**
```json
{
  "ListingKey": "EXA123456",
  "OriginatingUoi": "urn:reso:uoi:1.0:T00000012:M00000123",
  "OriginatingUsi": "urn:reso:usi:1.0:T00000012:50010",
  "SourceUoi": "urn:reso:uoi:1.0:T00000012:T00000045",
  "SourceUsi": "urn:reso:usi:1.0:T00000012:50011",
  "Provenance": [
    { "SequenceNumber": 0, "ProviderUoi": "urn:reso:uoi:1.0:T00000012:M00000123", "ProviderUsi": "urn:reso:usi:1.0:T00000012:50010" },
    { "SequenceNumber": 1, "ProviderUoi": "urn:reso:uoi:1.0:T00000012:T00000045", "ProviderUsi": "urn:reso:usi:1.0:T00000012:50011" }
  ]
}
```

**Organization mid-promotion** – a brokerage the technology provider first identified with a locally issued UOI, now superseded by a RESO-issued UOI after the brokerage entered the registry. A system promotes the same way, with a `SupersededByUsi` on the issuer's local System Resource.

**RESPONSE**
```json
{
  "OrganizationKey": "brokerage-7",
  "OrganizationId": "urn:reso:uoi:1.0:T00000045:brokerage-7",
  "OrganizationName": "Example Brokerage",
  "OrganizationStatus": "Superseded",
  "SupersededByUoi": "urn:reso:uoi:1.0:T00000012:B00000078",
  "OrganizationStatusChangeTimestamp": "2024-06-01T00:00:00Z",
  "ModificationTimestamp": "2024-06-01T00:00:00Z"
}
```

<br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO Transport](mailto:transport@reso.org) with questions about this proposal, or [RESO developer support](mailto:dev@reso.org) with specific development questions.
