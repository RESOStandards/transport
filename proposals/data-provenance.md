# RESO Data Provenance Endorsement

| **RCP** | 50 |
| :--- | :--- |
| **Version** | **1.0.0** |
| **Authors** | [Josh Darnell (RESO)](mailto:josh@reso.org)<br />[Cody Gustafson (FBS)](mailto:cody@fbsdata.com) |
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
* Adds a new resource called Provenance, which can be expanded into related records and is not required at the top level, though providers MAY choose to support it.
* Adds a new standard field to each resource called _Provenance_, which contains an ordered collection of entries.
* Creates a new way of identifying who has had ownership of a record which incorporates the provider, system, and tenant using standard organization and system identifiers, when defined, and lets providers define their own local identifiers if needed. 

<br />

# Introduction
Understanding the provenance of a given record, such as a listing, is important in cases where it's flowed through many different systems. This is becoming more common, and will increase with the addition of Add/Edit capabilities. 

Each system should also use standardized identifiers to describe where a record came from, which isn't the case currently and results in end users needing to maintain different sets of local organization mappings for each provider, even if they're both referencing the same originating or source system. This proposal uses RESO's Unique Organization Identifiers (UOIs) to address this issue. 


<br />

# Section 1: Purpose
The RESO Data Dictionary currently includes OriginatingSystem and SourceSystem Key, ID, and Name fields in each resource in order to identify where a given record originated or where it was obtained from, respectively. 

However, there are a couple of challenges with the current approach:
* If a record moves through more than two systems, the intermediate provenance is lost. 
* The IDs being used to describe organizations and systems are not standardized, and therefore result in additional custom mapping and programming to manage them.

This proposal adds support for complete provenance records as well as a way to standardize IDs without affecting current OriginatingSystem or SourceSystem information, which are highly utilized at the moment but should be phased out over time in favor of the provenance record.

<br />

# Section 2: Specification

## Section 2.1: Metadata Definition of Provenance

This specification defines a new resource called Provenance and a new navigation property, also called Provenance, which can be expanded into each resource that supports it as needed. 

```xml
<EntityType Name="Provenance">
  <Key>
    <PropertyRef Name="ProvenanceKey"/>
  </Key>
  <Property Name="ProvenanceKey" Nullable="false" Type="Edm.String">
  <Property Name="ResourceName" Nullable="false" Type="Edm.String">
  <Property Name="ResourceRecordKey" Nullable="false" Type="Edm.String">
  <Property Name="OwnerUrn" Nullable="false" Type="Edm.String" />
  <Property Name="SystemKeyValue" Nullable="false" Type="Edm.String" />
  <Property Name="SystemModificationTimestamp" Nullable="false" Type="Edm.DateTimeOffset" />
  <Property Name="ModificationTimestamp" Nullable="false" Type="Edm.DataTimeOffset">
</EntityType>

<EntityType Name="Property">
  <Key>
    <PropertyRef Name="ListingKey"/>
  </Key>
  <Property Name="ListingKey" Nullable="false" Type="Edm.String" />
  <Property Name="ModificationTimestamp" Nullable="false" Type="Edm.DateTimeOffset" />
  ...
  <NavigationProperty Name="Provenance" Nullable="false" Type="Collection(Provenance)">
</EntityType>
```

Similar to Media, the Provenance Resource is not required to be present at the top level and implements a "belongs-to" relationship with its parent records based on ResourceName and ResourceRecordKey.

## Section 2.2: Definition of OwnerUrn

In order to accurately identify the source of a given record, two pieces of information are needed:
* The Unique System Identifier (USI) of the specific product the record came from. Each provider has at least one system. 
* The UOI of the owner whose records are being served by the given system. 

This specification uses the [**Uniform Resource Name (URN) standard**](https://datatracker.ietf.org/doc/html/rfc8141) along with [**RESO's URN namespace**](https://www.iana.org/assignments/urn-formal/reso) to represent OwnerUrn as single string. 
 
<br />

> **Definition**: _OwnerUrn_
> 
> `urn:reso:uoi:2.0:{ProviderUsi}:{TenantUoi}`

Where:
* `ProviderUsi` is the USI of the vendor system the record came from. This could be a provider's internal system or public/private API, including certified RESO Web APIs.
* `TenantUoi` is the UOI of the tenant whose data is being hosted (e.g., an MLS).

Note that in many cases, knowing the ProviderUsi is sufficient. However, in some cases systems change from one provider to another. For example, if a system is sold to another company. 


### Section 2.2.1: Authoritative and Local Identifiers
RESO maintains authoritative Unique Organization and System Identifiers for certification. This addresses a large percentage of those who would implement this proposal, which are mainly MLSs and their vendors. For RESO-based identifiers, please [**contact RESO**](mailto:dev@reso.org). 

There may also be scenarios that require allowing well-known organizations to define their own UOIs. This is covered in the next section.

### Section 2.2.2: Assigning Local Identifiers
If an organization cannot have an ID assigned by RESO for some reason, a provider with an existing UOI can issue their own tenant UOI. 

In these cases, the provider MUST also host an instance of the [**RESO OUID Resource**](https://ddwiki.reso.org/display/DDW20/OUID+Resource) with an OrganizationUniqueId that matches the one assigned by the provider, and the following request should be successful:

**REQUEST**
```
GET https://example.api.com/OUID?$select=OrganizationUniqueId&$filter=OrganizationUniqueId eq '{TenantUoi}'
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "https://example.api.com/OUID?$select=OrganizationUniqueId$filter=OrganizationUniqueId eq '{TenantUoi}'",
  "value": [
    {
      "OrganizationUniqueId": "{TenantUoi}"
    }
  ]
}
```

In practice, `{TenantUoi}` would be replaced with the local OrganizationUniqueId.

**Notes**
* A new System Resource will be needed if providers also need to define their own system. 
* It may also make sense to use an alternate OwnerUrn in these cases so others know that the identifiers were issued by provider rather than RESO. It might be nice to indicate which values were created locally, but this may also be a bit cumbersome. Example: `urn:reso:uoi:2.0:{ProviderUoi}:local:{ProviderUsi}:local:{TenantUoi}` indicates that both the ProviderUsi and TenantUoi were issued by the given ProviderUoi. If just the TenantUoi were local, the OwnerUrn would be `urn:reso:uoi:2.0:{ProviderUoi}:{ProviderUsi}:local:{TenantUoi}`.
* If one provider uses local identifiers in their provenance records, another system consuming that information may no longer have access to the local UOI and USI records. If an end user needs access to this information, they should contact the well-known Provider UOI contained in the OwnerUrn for more information.

## Section 2.3: Examples
This section contains a number of examples related to provenance records. 

### Example: Adding an Initial Record to a System
Assume the ProviderUoi is T00000012, ProviderUsi is 50022, TenantUoi is T00000012, and that a record was added to the Property Resource with ListingKey ABC123. 

**REQUEST**
```
GET https://example.api.com/Property('ABC123')?$expand=Provenance&$select=ListingKey,ModificationTimestamp,Provenance/OwnerUrn,Provenance/SystemKeyValue,Provenance/SystemModificationTimestamp
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "https://example.api.com/Property('ABC123')?$expand=Provenance&$select=ListingKey,ModificationTimestamp,Provenance/OwnerUrn,Provenance/SystemKeyValue,Provenance/SystemModificationTimestamp",
  "ListingKey": "ABC123",
  "Provenance": [
    {
      "OwnerUrn": "urn:reso:uoi:2.0:50022:T00000012",
      "SystemKeyValue": "ABC123",
      "SystemModificationTimestamp": "2024-09-05T02:27:51Z"
    }
  ],
  "ModificationTimestamp": "2024-09-05T02:27:51Z"
}
```

The first entry in the provenance record conveys the originating system information, as it's currently known in the Data Dictionary.

### Example: Obtaining a Record from Another Provider
When a record is ingested into another system, its provenance record should be updated. 

Assume that the record from the previous example was replicated by a consumer with a ProviderUoi of T00000010, ProviderUsi of 50000, and TenantUoi of T00000012 (since the record corresponds to the same well-known tenant UOI) and, in the process, the ListingKey also changed to DEF456.

**REQUEST**
```
GET https://example.api.com/Property('DEF456')?$expand=Provenance&$select=ListingKey,ModificationTimestamp,Provenance/OwnerUrn,Provenance/SystemKeyValue,Provenance/SystemModificationTimestamp
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "https://example.api.com/Property('DEF456')?$expand=Provenance&$select=ListingKey,ModificationTimestamp,Provenance/OwnerUrn,Provenance/SystemKeyValue,Provenance/SystemModificationTimestamp",
  "ListingKey": "DEF456",
  "Provenance": [
    {
      "OwnerUrn": "urn:reso:uoi:2.0:50022:T00000012",
      "SystemKeyValue": "ABC123",
      "SystemModificationTimestamp": "2024-09-05T02:27:51Z"
    },
    {
      "OwnerUrn": "urn:reso:uoi:2.0:50000:T00000012",
      "SystemKeyValue": "DEF456",
      "SystemModificationTimestamp": "2024-09-05T02:31:21Z"
    }
  ],
  "ModificationTimestamp": "2024-09-05T02:31:21Z"
}
```
**Notes**
* Provenance entries are ordered from earliest to latest.
* The first provenance entry corresponds to the "originating system."
* The last entry corresponds to the "source system."
* In some cases, records are inputted in one system and served from another, both from the same provider. The OwnerUrn would have the same ProviderUoi in these cases, with different ProviderUsi values.

<br />

# Section 3: Certification
RESO will validate the following during certification:
* If Provenance is defined in a given system, there MUST be a complex type defined with OwnerUrn, SystemKeyValue, and SystemModificationTimestamp. 
* OwnerUrn MUST follow the definition outlined in Section 2.2. 
  * ProviderUoi, ProviderUsi, and TenantUoi will be verified against authoritative RESO UOI and USI services if standard identifiers are used.
  * If local identifiers are used for ProviderUsi or TenantUoi, these records must also exist in the System and OUID resources with the appropriate `:local:` components in the OwnerUrn.
  * If local identifiers are used, then the System and OUID resource schema will be validated against what's defined in the Data Dictionary for all standard data elements. Providers MAY add local fields to these resources.
* SystemKeyValue MUST be present and equal to the current key value for that owner. 
* SystemModificationTimestamp MUST be a valid ISO 8601 timestamp and always be less than or equal to the ModificationTimestamp of the record.
* If more than one Provenance entry exists, the SystemModificationTimestamp of each subsequent record must be greater than the previous one.

<br />

# Section 4. Contributors
This document was written by [Joshua Darnell](mailto:josh@reso.org), and [Cody Gustafson](mailto:cody@fbsdata.com).

Thanks to Ivaan Nazaroff, Paul Stusiak, Sam DeBord, and others who attended the Winter 2024 RESO Dev Workshop for their feedback during the creation of this specification.
<br />

# Section 5: References

Please see the following references for more information regarding topics covered in this document:
* [RESO Unique Organization Identifier (UOI)](https://www.reso.org/reso-unique-identifiers/)
* [Uniform Resource Name (URN)](https://datatracker.ietf.org/doc/html/rfc8141)
* [RESO URN Assignment](https://www.iana.org/assignments/urn-formal/reso)


<br />

# Section 6: Appendices

## Appendix A: Retrieving Provenance for Expanded Records
When expanded records are fetched from a system, both top-level and expanded records will have provenance information. 

For example, the provenance for a Property record with expanded Media might be as follows:

**REQUEST**
```
GET https://example.api.com/Property('DEF456')?$expand=Provenance,Media/Provenance&$select=ListingKey,ModificationTimestamp,Provenance/OwnerUrn,Provenance/SystemKeyValue,Provenance/SystemModificationTimestamp,Media/Provenance/OwnerUrn,Media/Provenance/SystemKeyValue,Media/Provenance/SystemModificationTimestamp
```

**RESPONSE**
```json
{
  "@odata.context": "https://example.api.com/Property('DEF456')?$expand=Provenance,Media/Provenance&$select=ListingKey,ModificationTimestamp,Provenance/OwnerUrn,Provenance/SystemKeyValue,Provenance/SystemModificationTimestamp,Media/Provenance/OwnerUrn,Media/Provenance/SystemKeyValue,Media/Provenance/SystemModificationTimestamp",
  "ListingKey": "DEF456",
  "Provenance": [
    {
      "OwnerUrn": "urn:reso:uoi:2.0:50022:T00000012",
      "SystemKeyValue": "ABC123",
      "SystemModificationTimestamp": "2024-09-05T02:27:51Z"
    },
    {
      "OwnerUrn": "urn:reso:uoi:2.0:50000:T00000012",
      "SystemKeyValue": "DEF456",
      "SystemModificationTimestamp": "2024-09-05T02:31:21Z"
    }
  ],
  "ModificationTimestamp": "2024-09-05T02:31:21Z",
  "Media": [
    {
      "MediaKey": "XYZ222",
      "Provenance": [
        {
          "OwnerUrn": "urn:reso:uoi:2.0:50022:T00000012",
          "SystemKeyValue": "UVW111",
          "SystemModificationTimestamp": "2024-09-05T02:27:51Z"
        },
        {
          "OwnerUrn": "urn:reso:uoi:2.0:50000:T00000012",
          "SystemKeyValue": "XYZ222",
          "SystemModificationTimestamp": "2024-09-05T02:31:21Z"
        }
      ],
      "ModificationTimestamp": "2024-09-05T03:32:31Z"
    }
  ]
}
```

<br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO](mailto:info@reso.org) if you have any questions.
