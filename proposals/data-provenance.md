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
* Adds a new standard expansion to each resource called _Provenance_, which contains a collection of Provenance records.
* Creates a new way of identifying who has had ownership of a record which incorporates the provider, system, and tenant using standard organization and system identifiers, when defined, and lets providers define their own local identifiers if needed. 

<br />

# Introduction
Understanding the provenance of a record, such as a listing, is important in cases where it's flowed through many different systems. This is becoming more common and will increase with the addition of Add/Edit capabilities. 

Each system should use _standard identifiers_, when possible, to describe where a record came from, which isn't currently the case and results in end users needing to maintain different sets of local organization mappings for each provider even if they're referencing the same originating or source system. 

This proposal uses the RESO Unique Organization and System Identifiers (UOIs and USIs) to address this issue. 

<br />

# Section 1: Purpose
The RESO Data Dictionary includes OriginatingSystem and SourceSystem Key, ID, and Name fields in each resource to identify where a given record originated or where it was obtained from. 

However, there are a couple of challenges with the this approach:
* If a record moves through more than two systems, the intermediate provenance is lost. 
* The names, IDs, and keys being used to describe organizations and systems are not standardized, which results in additional custom mapping and programming work.

This proposal adds support for complete provenance records as a way to standardize IDs without affecting current OriginatingSystem or SourceSystem ID/Name/Key information, which are highly utilized fields and can continue to exist locally as long as Provenance is also present.

<br />

# Section 2: Specification

## Section 2.1: Metadata Definition of the Provenance Resource

This specification defines a new resource called Provenance and a new navigation property, also called Provenance, which can be expanded into each resource that supports it, as needed. 

```xml
<EntityType Name="Provenance">
  <Key>
    <PropertyRef Name="ProvenanceKey"/>
  </Key>
  <Property Name="ProvenanceKey" Nullable="false" Type="Edm.String">
  <Property Name="SequenceNumber" Nullable="false" Type="Edm.Int16">
  <Property Name="ResourceName" Nullable="false" Type="Edm.String">
  <Property Name="ResourceRecordKey" Nullable="false" Type="Edm.String">
  <Property Name="ProviderUoi" Nullable="false" Type="Edm.String" />
  <Property Name="ProviderUsi" Nullable="false" Type="Edm.String" />
  <Property Name="TenantUoi" Nullable="false" Type="Edm.String" />
  <Property Name="EditedYN" Nullable="true" Type="Edm.Boolean" />
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

Similar to Media, the Provenance Resource is not required to be present at the top level and can exist only as an expansion or both. It implements a "belongs-to" relationship with its parent records based on `ResourceName` and `ResourceRecordKey`.

## Section 2.2: Organization and System Identifiers
RESO maintains identifiers for organizations and systems in both spreadsheet and JSON formats, which are used in Certification and RESO Analytics and are kept up to date. See the [**public RESO Certification site**](https://www.reso.org/certification) for more information. Records can be created, updated, or deactivated (but not removed) by [**contacting RESO**](mailto:info@reso.org).

### Section 2.2.1: Organization Resource 
Organization identifiers currently use the [**OUID Resource**](https://ddwiki.reso.org/display/DDW20/OUID+Resource) schema. 

The OUID Resource will be deprecated in favor the new _Organization Resource_ and language updated to reflect the changes to use Unique Organization Identifiers (UOIs) based on that resource. 

* **OrganizationKey** - String, non-nullable. The unique local key of the organization.
* **OrganizationId** - String, non-nullable. The organization identifier
* **OrganizationName** - String, nullable. The organization name. 
* **ActiveYN** - Boolean, nullable. `true` if the system is active, `false` or `null` otherwise.
* **ModificationTimestamp** - Timestamp, non-nullable. The timestamp when the organization record was last updated.
* Any relevant OUID Resource attributes.


### Section 2.2.2: System Resource 
A System Resource will be created to model system data. 

The existing USI sheet that RESO maintains contains UniqueSystemId, SystemName, and IsActive columns. These will become the following to adhere to current Data Dictionary conventions:
* **SystemKey** - String, non-nullable. The unique local key of the system.
* **SystemId** - String, non-nullable. The system identifier.
* **SystemName** - String, nullable. The system name. 
* **ActiveYN** - Boolean, nullable. `true` if the system is active, `false` or `null` otherwise.
* **ModificationTimestamp** - Timestamp, non-nullable. The timestamp when the system record was last updated.

OrganizationKey and SystemKey, which RESO doesn't currently provide, would be the local key of the Organization or System records and SystemId would be the well-known RESO identifier, when applicable. Other attributes may be added as separate proposals.

### Section 2.2.3: TenantUoi and SubtenantUoi
While the Provenance Resource provides a mechanism to query the most recent record in order to filter tenants in multi-tenant systems, there may be cases where the tenant is separate from the record owner and it's much easier to query the top-level record rather than filtering by provenance records.

There are also cases where there may be subtenants within a given TenantUoi. 

This proposal adds two new fields to handle these cases:
* **TenantUoi** - String, nullable. The OrganizationId of the tenant. 
* **SubtenantUoi** - String, nullable. The OrganizationId of the subtenant. This is optional, but if SubtenantUoi is provided then TenantUoi MUST be present.

**Deprecation Notice**: TenantUoi and SubtenantUoi will replace the existing OriginatingSystemKey, OriginatingSystemName, OriginatingSystemId, SourceSystemKey, SourceSystemName, SourceSystemId fields as the mechanism for querying a given organizations data set at the top level and the existing fields will be deprecated. They can still be present in a system as long as the TenantUoi, SubtenantUoi, and Provenance records are provided, when applicable.

There are also OriginatingSystem and SourceSystem expansions defined. They will be deprecated in favor for TenantOrganization and SubtenantOrganization expansions and additional system information can be found in the Provenance record.


### Section 2.3: Authoritative and Local Identifiers

This proposal allows local extension for organizations and systems. 

RESO maintains authoritative Unique Organization and System Identifiers for certification, which mainly consist of MLSs and their technology providers. 

New authoritative identifiers can be added by [**contacting RESO**](mailto:info@reso.org). 

### Section 2.3.1: Assigning Local Identifiers
If RESO organization or system identifiers cannot be used for some reason, providers MAY create identifiers of their own. 

In these cases, providers MUST host an instance of the Organization and System resources with OrganizationKey, OrganizationId and SystemKey, SystemId attributes (where the ID can be the key), along with ModificationTimestamp. OrganizationName and SystemName could potentially be null.

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

In practice, `{LocalUoi}` would be replaced with the local OrganizationId.

**System Resource**

**REQUEST**
```
GET https://example.api.com/System?$select=SystemId,SystemName,ModificationTimestamp&$filter=SystemId eq '{LocalUsi}'
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "https://example.api.com/System?$select=SystemId,SystemName,ModificationTimestamp&$filter=SystemId eq '{LocalUsi}",
  "value": [
    { 
      "OrganizationId": "{LocalUsi}",
      "OrganizationName": "Name of local system",
      "ModificationTimestamp": "2024-12-16T20:34:47Z"
    }
  ]
}
```

When local organizations or systems are created, end users of the data may not have access to the local Organization and System resources. However, the organization that provided those identifiers will have a well-known UOI issued by RESO that will appear in the Provenance record. Clients can contact the provider if more information is needed. 

## Section 2.3: Examples
This section contains a number of examples related to provenance records. 

### Example: Adding an Initial Record to a System
Assume the ProviderUoi is T00000012, ProviderUsi is 50022, TenantUoi is T00000012, and that a record was added to the Property Resource with ListingKey ABC123. 

**REQUEST**
```
GET https://example.api.com/Property('ABC123')?$expand=Provenance&$select=ListingKey,ModificationTimestamp
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "Property('ABC123')?$expand=Provenance&$select=ListingKey,ModificationTimestamp",
  "ListingKey": "ABC123",
  "ModificationTimestamp": "2024-09-05T02:27:51Z",
  "Provenance": [
    {
      "SequenceNumber": 0,
      "ProviderUsi": "T00000012",
      "ProviderUoi": "50022",
      "TenantUoi": "T00000012",
      "SystemKeyValue": "ABC123",
      "SystemModificationTimestamp": "2024-09-05T02:27:51Z",
      "EditedYN": true,
      "ModificationTimestamp": "2024-09-05T02:28:51Z"
    }
  ]
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
  "ModificationTimestamp": "2024-09-05T02:31:21Z",
  "Provenance": [
    {
      "SequenceNumber": 0,
      "ProviderUsi": "T00000012",
      "ProviderUoi": "50022",
      "TenantUoi": "T00000012",
      "SystemKeyValue": "ABC123",
      "SystemModificationTimestamp": "2024-09-05T02:27:51Z",
      "EditedYN": true,
      "ModificationTimestamp": "2024-09-05T02:28:51Z"
    },
    {
      "SequenceNumber": 1,
      "ProviderUsi": "T00000012",
      "ProviderUoi": "50000",
      "TenantUoi": "T00000012",
      "SystemKeyValue": "DEF456",
      "SystemModificationTimestamp": "2024-09-05T02:31:21Z",
      "EditedYN": false,
      "ModificationTimestamp": "2024-09-05T02:31:21Z"
    }
  ]
}
```

The first provenance record is the originating system. The latest one is the source system.

<br />

# Section 3: Certification
RESO will validate the following during certification:
* If Provenance is defined in a given system, there MUST be an OData EntityType defined with ProviderUoi, ProviderUsi, TenantUoi, SystemKeyValue, and SystemModificationTimestamp. 
* ProviderUoi, ProviderUsi, and TenantUoi will be verified against authoritative RESO UOI and USI values.
* If local identifiers are used for ProviderUoi, ProviderUsi, or TenantUoi, the provider MUST host Organization and System resources with local definitions of these items. 
* SystemKeyValue MUST be present and equal to the current key value for that owner. 
* SystemModificationTimestamp MUST be a valid ISO 8601 timestamp and always be less than or equal to the ModificationTimestamp of the record.
* If more than one Provenance entry exists, the SystemModificationTimestamp of each subsequent record must be greater than the previous one.
* SequenceNumber MUST be present and be incremented each time an entry is added. Prior entries should never be removed or changed.

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
