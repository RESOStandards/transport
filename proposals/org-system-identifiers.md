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

**Format.** UOIs and USIs are opaque identifiers. Consumers SHOULD treat them as opaque and SHOULD NOT infer meaning from their structure. For reference, the current forms are a nine-character UOI carrying a legacy organization-type letter prefix, for example `T00000012`, and a numeric USI, for example `50001`. RESO maintains the authoritative current format in the identifier registry.

> **Open item (Workgroup decision).** The final identifier format – retiring the organization-type prefix in favor of an opaque fixed-width identifier, with prior identifiers linked through a legacy mapping – is pending a Workgroup decision.

**Lifecycle.** A UOI or USI is created, updated, deactivated, or superseded, and is never removed. `OrganizationStatus` and `SystemStatus` each take one of three values:

* **`Active`** – current and in use.
* **`Inactive`** – retired with no successor, for example dissolved or closed.
* **`Superseded`** – replaced by another identifier, for example through a merger, acquisition, or reclassification.

When the status is `Superseded`, `SupersededByUoi` or `SupersededByUsi` MUST be populated; when it is `Active` or `Inactive`, that field MUST be null.

**Supersession and resolution.** A superseded record is retained, and its `SupersededBy` identifier points to the replacement, forming a redirect chain. A consumer holding a superseded identifier resolves to the current one by following the `SupersededBy` reference to the end of the chain. This mirrors the tombstone-and-redirect model of the Unique Licensee Identifier (ULI, RCP-54). Human narrative about a change, such as a merger history, MAY be carried in a free-text comment, but the machine-resolvable state is the status and `SupersededBy` fields.

## Section 2.4: Authoritative and Local Identifiers

RESO maintains authoritative Unique Organization and System Identifiers – primarily real estate associations, MLSs, and their technology providers – in both spreadsheet and JSON formats, which are kept current and used in Certification and RESO Analytics. Records can be created, updated, deactivated, or superseded, but not removed. New authoritative identifiers can be created by [contacting RESO](mailto:support@reso.org).

Where a RESO organization or system identifier cannot be used, a provider MAY define its own local identifier. In that case the provider MUST host instances of the Organization and System resources exposing `OrganizationId` / `SystemId` (the identifier MAY equal the key) along with `ModificationTimestamp`; `OrganizationName` and `SystemName` MAY be null.

**Organization Resource**

**REQUEST**
```
GET https://example.api.com/Organization?$select=OrganizationId,OrganizationName,ModificationTimestamp&$filter=OrganizationId eq '{LocalUoi}'
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "https://example.api.com/Organization?$select=OrganizationId,OrganizationName,ModificationTimestamp&$filter=OrganizationId eq '{LocalUoi}'",
  "value": [
    {
      "OrganizationId": "{LocalUoi}",
      "OrganizationName": "Name of local organization",
      "ModificationTimestamp": "2024-12-16T20:34:47Z"
    }
  ]
}
```

**System Resource**

**REQUEST**
```
GET https://example.api.com/System?$select=SystemId,SystemName,ModificationTimestamp&$filter=SystemId eq '{LocalUsi}'
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "https://example.api.com/System?$select=SystemId,SystemName,ModificationTimestamp&$filter=SystemId eq '{LocalUsi}'",
  "value": [
    {
      "SystemId": "{LocalUsi}",
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
* Identifier resolution follows one of two paths. An authoritative identifier MUST appear in the RESO authoritative registry artifact described in Section 2.4. A provider's own local identifier MUST resolve against that provider's hosted Organization or System resource. An identifier that names a third party is validated against the RESO authoritative registry only, since the endpoint under test is not obligated to host another party's resources.
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
* [RESO Data Provenance Endorsement (RCP-50)](./data-provenance.md)
* [RESO Unique Licensee Identifier (ULI, RCP-54)](./uli-resolution-protocol.md)

<br />

# Section 6: Appendices

## Design rationale

**Sub-organization granularity is handled by Originating at grain, not a dedicated field.** Some data sets bundle many sub-organizations under a single originating organization, and a provider may need to scope to one of them. This endorsement handles that case with `OriginatingUoi` at the appropriate grain – the identifier points at the actual creating organization, however specific – rather than adding a per-record sub-organization field. Organization parentage is separately expressible through the `RelatedOrganization` self-expansion (Section 2.1) as an `AffiliatedWith` edge, which keeps the hierarchy in one place and resolvable. A denormalized `OriginatingSubUoi`/`SourceSubUoi` field pair alongside the top-level identifiers was also considered – it matches how some providers filter today but adds a column to every record – and is held for a future revision; `OriginatingSubUoi` and `SourceSubUoi` are reserved as the names should the field pair be needed.

**Top-level identifiers, not Tenant/Subtenant.** An earlier approach modeled multi-tenant filtering with `TenantUoi`/`SubtenantUoi`. Both uses that motivated it – de-multiplexing a combined feed by origin, and scoping to one organization within a grant – are served today by `OriginatingSystemName`, so they are served here by `OriginatingUoi`/`OriginatingUsi`. Originating is the more intuitive, 1:1 migration from the existing fields, is usable before Provenance is adopted, and avoids introducing tenancy vocabulary into the Data Dictionary. Tenancy as an access-and-partitioning concern is left to the layer that governs access.

**Status as an enumeration, and relationships as neutral edges.** The authoritative registry historically carried organization status as a Boolean and recorded mergers and reclassifications only in free-text comments, which are not machine-resolvable. This endorsement models status as an enumeration – `Active`, `Inactive`, `Superseded` – paired with a `SupersededBy` reference, so a consumer can resolve a retired identifier to its current one without parsing prose. Relationships between organizations are modeled as directed, typed graph edges (`RelatedOrganization`) rather than ownership statements: an edge records that two organizations are related and how, not that one organization owns or controls another. The `RelationshipType` vocabulary is intentionally functional – `ParticipatesIn`, `ServedBy`, `AffiliatedWith` – to avoid language that could imply control in a real estate context.

## Worked example

The following shows an Organization Resource response, a System Resource response, and a record whose top-level identifiers reconcile against a Provenance chain. Identifier values are illustrative.

**Organization Resource** – the technology provider that served the record, expanded (`$expand=RelatedOrganization`) to show an `AffiliatedWith` edge to its parent organization.

**RESPONSE**
```json
{
  "OrganizationKey": "T00000045",
  "OrganizationId": "T00000045",
  "OrganizationName": "Example Technology Provider",
  "OrganizationStatus": "Active",
  "OrganizationStatusChangeTimestamp": "2019-03-11T00:00:00Z",
  "ModificationTimestamp": "2024-11-02T18:22:10Z",
  "RelatedOrganization": [
    {
      "RelatedUoi": "T00000009",
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
  "SystemId": "50011",
  "ProviderUoi": "T00000045",
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
  "OriginatingUoi": "M00000123",
  "OriginatingUsi": "50010",
  "SourceUoi": "T00000045",
  "SourceUsi": "50011",
  "Provenance": [
    { "SequenceNumber": 0, "ProviderUoi": "M00000123", "ProviderUsi": "50010" },
    { "SequenceNumber": 1, "ProviderUoi": "T00000045", "ProviderUsi": "50011" }
  ]
}
```

<br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO Transport](mailto:transport@reso.org) with questions about this proposal, or [RESO developer support](mailto:dev@reso.org) with specific development questions.
