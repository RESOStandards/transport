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

<br />

# Introduction

Real estate records routinely pass through more than one organization and system before reaching an end user. To use a record, a consumer needs to know which organization and which system it came from. Today that information is carried in fields such as `OriginatingSystemName` and `SourceSystemName`, but the values are not standardized — and `OriginatingSystemName` is, in practice, a local vendor short-code for an *organization*, not a system. Because every provider spells these codes differently, each consumer maintains its own set of mappings for every provider it works with, even when two providers are naming the same organization.

This endorsement standardizes the identifiers themselves. It defines a RESO **Unique Organization Identifier (UOI)** and a RESO **Unique System Identifier (USI)**, each backed by a resource, so an organization or system is named the same way everywhere. It is one of a family of RESO universal identifiers — alongside the Universal Listing Identifier (ULI, RCP-54), which identifies listings — and it supplies the organization and system identifiers that the Data Provenance endorsement (RCP-50) and the Data Dictionary rely on.

<br />

# Section 1: Purpose

The RESO Data Dictionary carries `OriginatingSystem` and `SourceSystem` `Name`, `ID`, and `Key` fields in each resource to describe where a record originated and where it was obtained. Two problems recur with this approach:

* The names, IDs, and keys used to describe organizations and systems are not standardized, which forces custom mapping and programming work for every provider.
* The identity being carried is ambiguous — `OriginatingSystemName` typically names an *organization*, not a system — so a consumer cannot reliably separate an organization from the system that served it.

This endorsement addresses both by defining standard, resolvable identifiers for organizations (UOI) and systems (USI), a resource for each, the rules for issuing them authoritatively or hosting them locally, and standard top-level fields that replace the `OriginatingSystem`/`SourceSystem` values.

<br />

# Section 2: Specification

## Section 2.1: Organization Resource (UOI)

Organizations are currently modeled by the [OUID Resource](https://ddwiki.reso.org/display/DDW20/OUID+Resource). This endorsement deprecates the OUID Resource in favor of a new **Organization Resource** and standardizes the **Unique Organization Identifier (UOI)** it carries. Most OUID properties port directly.

Once ratified, current implementations MAY continue to expose the OUID Resource, but MUST implement the Organization Resource and populate the UOI when present.

The Organization Resource defines at least the following:

* **OrganizationKey** — String, non-nullable. The unique local key of the organization.
* **OrganizationId** — String, non-nullable. The Unique Organization Identifier (UOI).
* **OrganizationName** — String, nullable. The organization name.
* **ActiveYN** — Boolean, nullable. `true` if the organization is active, `false` or `null` otherwise.
* **ModificationTimestamp** — Timestamp, non-nullable. When the organization record was last updated.
* Any relevant OUID Resource attributes.

`OrganizationKey` is the local key of the record; `OrganizationId` is the well-known RESO identifier, when applicable.

## Section 2.2: System Resource (USI)

This endorsement defines a new **System Resource** to model the systems that produce and carry records, and standardizes the **Unique System Identifier (USI)** it carries. RESO already maintains a list of system identifiers (with `UniqueSystemId`, `SystemName`, and `IsActive`); the fields below align that list to current Data Dictionary conventions.

A System Resource MAY model any relevant system properties, but MUST define at least an identifier. The identifier is **not** the primary key: the primary key (`SystemKey`) is internal to a single system, whereas the identifier (`SystemId`, the USI) is well-known and spans systems.

The System Resource defines at least the following:

* **SystemKey** — String, non-nullable. The unique local key of the system.
* **SystemId** — String, non-nullable. The Unique System Identifier (USI).
* **SystemName** — String, nullable. The system name.
* **ActiveYN** — Boolean, nullable. `true` if the system is active, `false` or `null` otherwise.
* **ModificationTimestamp** — Timestamp, non-nullable. When the system record was last updated.

Other attributes MAY be added through separate proposals.

## Section 2.3: Authoritative and Local Identifiers

RESO maintains authoritative Unique Organization and System Identifiers — primarily MLSs and their technology providers — in both spreadsheet and JSON formats, which are kept current and used in Certification and RESO Analytics. Records can be created, updated, or deactivated, but not removed. New authoritative identifiers can be created by [contacting RESO](mailto:support@reso.org).

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

## Section 2.4: Originating and Source Identifiers

The Data Dictionary's `OriginatingSystem` and `SourceSystem` fields carry the organization and system a record came from and was obtained from. This endorsement standardizes those as UOI/USI pairs at the top level of each resource that supports them:

* **OriginatingUoi** / **OriginatingUsi** — the organization and system where the record originated.
* **SourceUoi** / **SourceUsi** — the organization and system the current provider obtained the record from.

These fields are usable on their own and do **not** require the Data Provenance endorsement (RCP-50); they carry the same originating- and source-identification that `OriginatingSystemName`/`ID` and `SourceSystemName`/`ID` carry today, using standard identifiers.

**Deprecation.** `OriginatingSystemName`, `OriginatingSystemID`, `OriginatingSystemKey`, `SourceSystemName`, `SourceSystemID`, and `SourceSystemKey` are deprecated in Data Dictionary v3.0. A provider MAY continue to populate them, but only if the corresponding UOI/USI identifiers are also present.

**Consistency with Provenance.** When Provenance is present, the top-level identifiers MUST agree with the ends of the provenance chain: `OriginatingUoi`/`OriginatingUsi` MUST equal the earliest (origin) Provenance record, and `SourceUoi`/`SourceUsi` MUST equal the hop the current provider obtained the record from. Provenance describes the full chain in between; its endpoints MUST NOT contradict the top-level fields.

<br />

# Section 3: Certification

RESO will validate the following during certification:

* When an Organization or System resource is hosted, it MUST be defined with the required identifier fields — `OrganizationKey`/`OrganizationId` or `SystemKey`/`SystemId` — along with `ModificationTimestamp`.
* `OriginatingUoi`/`OriginatingUsi` and `SourceUoi`/`SourceUsi`, when present, MUST resolve to authoritative RESO identifiers, or to the provider's locally-hosted Organization and System resources when local identifiers are used.
* A `SystemId` (USI) MUST NOT be assumed to be the resource's primary key; the primary key is `SystemKey`.
* A deprecated `OriginatingSystem*` or `SourceSystem*` field MUST NOT be present unless its UOI/USI analogue is also present.
* When Provenance is present, the top-level `OriginatingUoi`/`OriginatingUsi` and `SourceUoi`/`SourceUsi` MUST match the endpoints of the provenance chain (the ends-match rule of §2.4).

<br />

# Section 4. Contributors
This document was written by [Joshua Darnell](mailto:josh@reso.org).

<br />

# Section 5: References

Please see the following references for more information regarding topics covered in this document:
* [RESO Unique Organization Identifier (UOI)](https://www.reso.org/reso-unique-identifiers/)
* [RESO Data Provenance Endorsement (RCP-50)](./data-provenance.md)
* [RESO Universal Listing Identifier (ULI, RCP-54)](./uli-resolution-protocol.md)

<br />

# Section 6: Appendices

## Design rationale

**Sub-organization granularity is handled by Originating at grain, not a dedicated field.** Some data sets bundle many sub-organizations under a single originating organization, and a provider may need to scope to one of them. This endorsement handles that case with `OriginatingUoi` at the appropriate grain — the identifier points at the actual creating organization, however specific — rather than adding a per-record sub-organization field. Two finer-grained mechanisms were considered and are held for a future revision if a confirmed need emerges: organization parentage carried in the Organization Resource as a self-referential parent link, which keeps the hierarchy in one place and resolvable; and a denormalized `OriginatingSubUoi`/`SourceSubUoi` field pair alongside the top-level identifiers, which matches how some providers filter today but adds a column to every record. Neither is adopted now; `OriginatingSubUoi` is reserved as the name should the field pair be needed.

**Top-level identifiers, not Tenant/Subtenant.** An earlier approach modeled multi-tenant filtering with `TenantUoi`/`SubtenantUoi`. Both uses that motivated it — de-multiplexing a combined feed by origin, and scoping to one organization within a grant — are served today by `OriginatingSystemName`, so they are served here by `OriginatingUoi`/`OriginatingUsi`. Originating is the more intuitive, 1:1 migration from the existing fields, is usable before Provenance is adopted, and avoids introducing tenancy vocabulary into the Data Dictionary. Tenancy as an access-and-partitioning concern is left to the layer that governs access.

## Worked example

*(To be added: an Organization Resource response, a System Resource response, and a record carrying `OriginatingUoi`/`Usi` + `SourceUoi`/`Usi` reconciled against a Provenance chain to show the ends-match rule.)*

<br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO Transport](mailto:transport@reso.org) if you have any questions.
