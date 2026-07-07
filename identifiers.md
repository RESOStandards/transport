# RESO Identifiers

RESO issues a family of identifiers for the core entities in real estate data: the licensed professionals, organizations, systems and parcels that records refer to. Standard identifiers let data from different sources line up without custom, per-provider mapping. This page is the map – what each identifier is, how it is formed and where to find its authoritative reference materials. It is written to be useful both to a person getting oriented and to an AI assistant navigating RESO's standards.

## The Family at a Glance

| Identifier | Full Name | Identifies | Form | Home |
|---|---|---|---|---|
| **ULI** | Unique Licensee Identifier | Licensed real estate professionals | Resolved identifier, carried as a URN | [RCP-54](./proposals/uli-resolution-protocol.md) |
| **UOI** | Unique Organization Identifier | Organizations: MLSs, brokerages, associations, technology providers | Nine-digit ID, organization-type letter prefix (for example, `T00000012`) | [RCP-55](./proposals/org-system-identifiers.md) |
| **USI** | Unique System Identifier | Systems that produce and carry records | Assigned identifier, distinct from a system's primary key | [RCP-55](./proposals/org-system-identifiers.md) |
| **UPI** | Universal Parcel Identifier | Property parcels and their subcomponents | URN coordinate: country, jurisdiction and parcel number | [upi.reso.org](https://upi.reso.org/) |

## Universal vs. Unique

The names are deliberate and mark a real difference.

- **Universal** means a derived, self-describing coordinate. The UPI is computed from a parcel number and the authoritative jurisdiction that assigns it, within global standards (ISO 3166 country codes, URNs per RFC 8141), so anyone can derive and verify it from public authoritative data with no registry lookup. It locates a parcel the way latitude and longitude locate a point.
- **Unique** means an assigned and resolved identifier. The ULI, UOI and USI are issued and resolved rather than computed. They can merge or be superseded as records reconcile, and RESO's authoritative set can be extended locally, so each is unambiguous within its issuance but is not a self-describing coordinate.

## Unique Licensee Identifier (ULI)

The ULI gives a licensed real estate professional a single identifier that links their records across MLSs and markets, whether or not they are REALTORS®. It is a linking identifier rather than a primary key: it connects established records instead of replacing their keys, and it is eventually consistent. As the network resolves a licensee, a ULI may be superseded by a merge or redirect, and the linked records re-point to the replacement rather than disappearing.

**Reference materials**

- Specification: [RCP-54, ULI Resolution Protocol](./proposals/uli-resolution-protocol.md)
- Overview: [reso.org/reso-unique-identifiers](https://www.reso.org/reso-unique-identifiers/)

## Unique Organization Identifier (UOI)

The UOI gives each organization in the real estate industry a single identifier. RESO maintains an authoritative set, primarily MLSs and their technology providers, and tracks organization types including Broker, MLS, Local Association, State or Provincial Association, National Association, Technology Company, Commercial and Pooled Platform. Identifiers are currently nine digits and usually begin with a letter for the organization type (for example, `T00000012`); a proposal to move to a six-digit numeric form is under review. RCP-55 introduces the Organization Resource that carries the UOI, deprecating the older OUID Resource, and lets providers host local organization identifiers where RESO has not issued one.

**Reference materials**

- Specification: [RCP-55, Organization and System Identifiers](./proposals/org-system-identifiers.md)
- Authoritative data: the RESO Certification UOI registry, maintained in spreadsheet and JSON – see [reso.org/certification](https://www.reso.org/certification)
- Naming conventions: the [RESO Style Guide](https://github.com/RESOStandards/reso-ai-registry/blob/main/policies/style-guide.md) covers organization names, base vs. enhanced names and addresses
- Overview: [reso.org/reso-unique-identifiers](https://www.reso.org/reso-unique-identifiers/)
- Requests: new identifiers via support@reso.org

## Unique System Identifier (USI)

The USI identifies the systems that produce and carry records. A system belongs to an organization – its provider, referenced by that organization's UOI. The USI is deliberately distinct from a system's primary key: the primary key is internal to a single system, while the USI is well known and spans systems. RCP-55 introduces the System Resource that carries the USI, formalizing the system identifiers RESO already maintains and uses in Certification and Analytics.

**Reference materials**

- Specification: [RCP-55, Organization and System Identifiers](./proposals/org-system-identifiers.md)
- Requests: new identifiers via support@reso.org

## Universal Parcel Identifier (UPI)

The UPI identifies a property parcel and its subcomponents across jurisdictions, so a parcel number from one recording authority does not collide with the same number from another. It is a URN built from the parcel number and the authoritative jurisdiction that assigns it:

```
urn:reso:upi:<Version>:<Country>:<CountrySubdivision>:<ParcelNumber>[:sub:<ParcelSubcomponent>]
```

- **Country** – the ISO 3166 country code (for example, `US`).
- **CountrySubdivision** – the parcel-assigning authority (a county GEOID in the United States, a NUTS code in the European Union).
- **ParcelNumber** – the parcel identifier assigned by that authority.
- **ParcelSubcomponent** (optional) – an element attached to a parcel, such as air rights or a boat slip.

For example, `urn:reso:upi:2.0:US:48201:R000022230` identifies a parcel in Harrison County, Texas. The current version is 2.0.

**Reference materials**

- Home and specification: [upi.reso.org](https://upi.reso.org/)
- Tooling: the [UPI Builder](https://upi.reso.org/builder)
- Standards: [RFC 8141 (URN)](https://datatracker.ietf.org/doc/html/rfc8141) and [RFC 3986 (URI)](https://datatracker.ietf.org/doc/html/rfc3986); RESO's UPI namespace is reserved with IANA
- Overview: [reso.org/reso-unique-identifiers](https://www.reso.org/reso-unique-identifiers/)

## How the Identifiers Relate

A single record often carries several of these at once. A listing is served by a system (USI) that belongs to an organization (UOI); it references the licensees involved (ULI) and the parcel it sits on (UPI). Data Provenance ([RCP-50](./proposals/data-provenance.md)) uses the UOI and USI to record where a record originated and where it was obtained, and RCP-55 standardizes those as the top-level `OriginatingUoi` / `OriginatingUsi` and `SourceUoi` / `SourceUsi` fields.

## Existing Names and the Migration

These standard identifiers replace earlier names that are still widely present, so implementers will see both in practice.

- **OUID Resource** – RESO's earlier way of modeling organizations. RCP-55 introduces the Organization Resource and the UOI in its place.
- **`OriginatingSystemName` / `OriginatingSystemID` and `SourceSystemName` / `SourceSystemID`** – the Data Dictionary fields that carry where a record came from and where it was obtained. In practice, `OriginatingSystemName` holds a local vendor short-code for an *organization* rather than a system. RCP-55 standardizes these as `OriginatingUoi` / `OriginatingUsi` and `SourceUoi` / `SourceUsi` and deprecates the older fields in Data Dictionary v3.0, where a provider may keep them only if the UOI and USI analogues are also present.

The migration is under way rather than complete. RESO already issues UOIs and USIs and uses them in Certification, across live organizations and in the endorsements service, so both the existing names and the new identifiers appear in real data today. RCP-55 is the proposal that formalizes the move from OUID and the OriginatingSystem and SourceSystem fields to the UOI and USI.

## Authoritative and Local Identifiers

RESO issues authoritative identifiers and keeps them current for Certification and Analytics. Where RESO has not issued one, a provider may host its own local Organization or System resource so that consumers can still resolve the identifier (RCP-55). New authoritative UOIs and USIs are created by contacting support@reso.org.

## All Reference Materials

- **Specifications:** [RCP-54 ULI Resolution Protocol](./proposals/uli-resolution-protocol.md), [RCP-55 Organization and System Identifiers](./proposals/org-system-identifiers.md), [RCP-50 Data Provenance](./proposals/data-provenance.md)
- **Public overviews:** [reso.org/reso-unique-identifiers](https://www.reso.org/reso-unique-identifiers/), [upi.reso.org](https://upi.reso.org/)
- **Tooling:** the [UPI Builder](https://upi.reso.org/builder); RESO Certification tooling that validates and works with these identifiers lives in [reso-tools](https://github.com/RESOStandards/reso-tools)
- **Authoritative data:** the RESO Certification UOI and USI registry, maintained in spreadsheet and JSON
- **Conventions:** the [RESO Style Guide](https://github.com/RESOStandards/reso-ai-registry/blob/main/policies/style-guide.md), including its Unique Organization Identifier section
