# RelatedLookup Resource

| **RCP** | 47 |
| :--- | :--- |
| **Version** | **1.0.0** |
| **Authors** | [Joshua Darnell (RESO)](mailto:josh@reso.org) <br /> [Ryan Yates (Rapattoni Corporation)](mailto:ryates@rapattoni.com) |
| **Specification** | [**LINK TO RCP**](./related-lookups.md) |
| **Status** | IN PROGRESS |
| **Date Ratified** | TBD |
| **Dependencies** | [Data Dictionary 2.1](#) |
| **Related Links** | [Lookup Resource](https://ddwiki.reso.org/display/DDW20/Lookup+Resource) |

<br />

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

<br />

# Table of Contents
- [Summary](#summary)
- [Section 1: Introduction](#section-1-introduction)
- [Section 2: Specification](#section-2-specification)
- [Section 3: Certification](#section-3-certification)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: Appendices](#section-6-appendices)
- [Section 7: License](#section-7-license)

<br />

# Summary
Adds support for defining relationships between enumerations. This allows users to know which enumerated values from a given field go with others. For example, which counties are in a given state or province and which schools are in a given county.

<br />

# Section 1: Introduction
Conveying information about related enumerations is important in cases such as displaying accurate values for related option lists on search and input forms. 

This could be accomplished using computable business rules, such as those offered by [**RESO Validation Expressions (RCP-19)**](./validation-expressions.md). However, this presents implementation challenges due to the overhead in writing parsers and interpreters.

Since metadata for enumerations is already conveyed by the Lookup Resource, it's easier to build upon that and use resource-based metadata to address this case rather than grammars. 

<br />

# Section 2: Specification
This specification defines a _RelatedLookup Resource_ to model relationships between Lookup Resource items.

A _RelatedLookup_ can be thought of a directed edge from LookupKeyA to LookupKeyB, with a ModificationTimestamp field for replication purposes, and has the following structure:

| Field | Data Type | Sample Value | Nullable | Description |
| - | - | - | - | - |
| **LookupKey** | Edm.String | "ABC123" | False | The unique key of the Lookup resource item the relationship originates from. |
| **RelatedLookupKey** | Edm.String | "X22" | False | The unique key of the Lookup resource item the relationship "points to" or terminates at. |
| **ModificationTimestamp** | Edm.DateTimeOffset | "2024-07-01T16:36:40.01Z" | False | The ISO 8601 timestamp for when the relationship was last modified. |

A common use case would be that a data consumer would already know the LookupKey they are interested in and would then retrieve RelatedLookupKey items from the RelatedLookup Resource.

<br />

## Example: GET RelatedLookup Entries for a Given Lookup
Assuming "ABC123" is the LookupKey for the CountyOrParish value "Los Angeles County," the RelatedLookup Resource might contain records similar to the following:

**REQUEST**
```
GET https://your.resoapi.com/RelatedLookup?$filter=LookupKey eq 'ABC123'
```

**RESPONSE**
```json
{
  "@odata.context": "https://your.resoapi.com/RelatedLookup?$filter=LookupKey eq 'ABC123'",
  "value": [
    {
      "LookupKey": "ABC123",
      "RelatedLookupKey": "X22",
      "ModificationTimestamp": "2020-07-07T17:36:14Z"
    },
    {
      "LookupKey": "ABC123",
      "RelatedLookupKey": "Y33",
      "ModificationTimestamp": "2020-07-07T17:36:15Z"
    },
    {
      "LookupKey": "ABC123",
      "RelatedLookupKey": "Z44",
      "ModificationTimestamp": "2020-07-07T17:36:16Z"
    }
  ]
}
```

In the above example:
* "X22" is a StateOrProvince LookupKey for the LookupValue of "Los Angeles County" (LookupKey "ABC123").
* "Y33" is a City LookupKey for the LookupValue of "City of Los Angeles".
* "Z44" is a Subdivision LookupKey for the LookupValue of "Sawtelle".

**Notes** 
* `Subdivision` isn't currently an enumerated field in the Data Dictionary. There's a field called [SubdivisionName](https://ddwiki.reso.org/display/DDW20/SubdivisionName+Field) which is a string instead.
* There have been some requests to add an enumerated subdivision field to the Data Dictionary, but there's nothing stopping providers from adding a local enumeration for Subdivision, similar to the example.

<br />

## Example: GET Lookup Entries for Given RelatedLookup Items
Once related lookups have been retrieved, the data consumer would re-query the Lookup Resource for those items, unless they had already replicated the metadata and were processing it locally. 

**REQUEST**
```
GET https://your.resoapi.com/Lookup?$filter=LookupKey eq 'ABC123' or LookupKey eq 'X22' or LookupKey eq 'Y33' or LookupKey eq 'Z44'
```

**RESPONSE**
```json
{
  "@odata.context": "https://your.resoapi.com/Lookup?$filter=LookupKey eq 'ABC123' or LookupKey eq 'X22' or LookupKey eq 'Y33' or LookupKey eq 'Z44'",
  "value": [
    {
      "LookupKey": "ABC123",
      "LookupName": "CountyOrParish",
      "LookupValue": "Los Angeles County",
      "DisplayName": null,
      "ModificationTimestamp": "2020-07-07T17:36:14Z"
    },
    {
      "LookupKey": "X22",
      "LookupName": "StateOrProvince",
      "LookupValue": "CA",
      "StandardLookupValue": "CA",
      "ModificationTimestamp": "2020-07-07T17:36:15Z"
    },
    {
      "LookupKey": "Y33",
      "LookupName": "City",
      "LookupValue": "City of Los Angeles",
      "StandardLookupValue": null,
      "ModificationTimestamp": "2020-07-07T17:36:15Z"
    },
    {
      "LookupKey": "Z44",
      "LookupName": "Subdivision",
      "LookupValue": "Sawtelle",
      "StandardLookupValue": null,
      "ModificationTimestamp": "2020-07-07T17:36:16Z"
    }
  ]
}
```

**Notes**
* For those that support the OData `in` operator and Web API Core 2.1.0, the above query could be written using that rather than `or` queries. For example: `/Lookup?$filter=LookupKey in ('ABC123', 'X22','Y33', 'Z44')`. 
* However, providers MUST support and will be tested for both for compatibility.
* Examples are shown using `or` queries instead since that's currently the common case. 

<br />

## Example: GET All 'City' Values for a Given 'CountyOrParish'
Suppose we want to retrieve all the City lookups for the CountyOrParish of "Los Angeles County" with LookupKey "ABC123" that we retrieved in a previous example.

In this case, we can add `LookupName eq 'City'` to the query in order to filter only City lookups for the LookupKey items retrieved in the RelatedLookup query:

**REQUEST**
```
GET https://your.resoapi.com/Lookup?$filter=LookupName eq 'City' and (LookupKey eq 'X22' or LookupKey eq 'Y33' or LookupKey eq 'Z44')
```

**RESPONSE**
```json
{
  "@odata.context": "https://your.resoapi.com/Lookup?$filter=LookupName eq 'City' and (LookupKey eq 'X22' or LookupKey eq 'Y33' or LookupKey eq 'Z44')",
  "value": [
    {
      "LookupKey": "Y33",
      "LookupName": "City",
      "LookupValue": "City of Los Angeles",
      "StandardLookupValue": null,
      "ModificationTimestamp": "2020-07-07T17:36:15Z"
    }
  ]
}
```

**Notes**
* In the above example, only one value was returned since only `Y33` is a 'City' LookupKey.
* LookupName is something that can be used across one or more fields and is linked to the given underlying field(s) in the Field Resource. 

<br />

## Optional Section: Navigation Property Path Queries

In the previous examples, multiple queries were required to fetch related lookups.

For those using OData navigation property paths, Lookup and RelatedLookup items can be fetched in a single query.

### Example: GET Lookup Items and Related Lookup Items for a Given Lookup Item

**REQUEST**

```
GET /Lookup?$filter=LookupKey eq 'ABC123' or (RelatedLookup/any(a:a/RelatedLookupKey eq 'ABC123') and LookupName eq 'City')
```

**RESPONSE**
```json
{
  "@odata.context": "https://your.resoapi.com/Lookup?$filter=LookupKey eq 'ABC123' or (RelatedLookup/any(a:a/RelatedLookupKey eq 'ABC123') and LookupName eq 'City')",
  "value": [
    {
      "LookupKey": "ABC123",
      "LookupName": "CountyOrParish",
      "LookupValue": "City of Los Angeles",
      "StandardLookupValue": null,
      "ModificationTimestamp": "2020-07-07T17:36:15Z"
    },
    {
      "LookupKey": "ab8298",
      "LookupName": "City",
      "LookupValue": "City of Santa Monica",
      "StandardLookupValue": null,
      "ModificationTimestamp": "2020-07-07T17:36:15Z"
    },
    {
      "LookupKey": "23oerr2",
      "LookupName": "City",
      "LookupValue": "City of Westwood",
      "StandardLookupValue": null,
      "ModificationTimestamp": "2020-07-07T17:36:16Z"
    }
  ]
}
```

**Notes**
* The a standard relationship similar to the following will need to be added to the Lookup resource to support the above navigation property path queries: _Lookup has many lookups as RelatedLookup through RelatedLookupKey_.
* Not all providers might support navigation property path queries, so they're an optional part of the specification. Providers will return a non-2XX status code in that case.

<br />

## Impact
This is a MAY proposal for Web API Core 2.0.0+ and Data Dictionary 2.1+. 

<br />

# Section 3: Certification

The following tests should be performed when the RelatedLookup Resource is present:
* Metadata validation for the RelatedLookup Resource against the proposed schema.
* Replication from the RelatedLookup Resource and validation what's advertised in the metadata.
* Verification that the three mandatory fields are always present: `LookupKey`, `RelatedLookupKey`, and `ModificationTimestamp`. Providers may add additional local fields, but should be mindful of the fact that this resource could potentially be very large and therefore additional fields could add a lot of overhead.
* Once data is sampled from the RelatedLookup Resource, all LookupKey and RelatedLookupKey values should be present in the Lookup Resource.
* `OR` query options MUST be checked for Web API Core 2.0.0+. `IN` query options MUST be checked for Web API Core 2.1.0+. Providers MAY support `IN` for earlier versions of Web API Core but they won't be validated since this functionality wasn't required.
* Combined LookupName and LookupKey queries MUST be supported. 
* Navigation property path queries MUST be tested if the provider has an OData `NavigationProperty` defined between the Lookup and RelatedLookup resources.

<br />

# Section 4. Contributors
This document was written by [Joshua Darnell (RESO)](mailto:josh@reso.org) and [Ryan Yates (Rapattoni Corporation)](mailto:ryates@rapattoni.com).

Thanks to the RESO Transport Workgroup for the feedback they gave during to the creation of this proposal.

<br />

# Section 5: References

Please see the following references for more information regarding topics covered in this document:
* [RESO Data Dictionary 2.0 Lookup Resource](https://ddwiki.reso.org/display/DDW20/Lookup+Resource)
* [RESO Web API Core specification](https://github.com/RESOStandards/transport/blob/main/proposals/web-api-core.md)

<br />

# Section 6: Appendices

* [ISO 8601 Wikipedia entry](https://en.wikipedia.org/wiki/ISO_8601)
* [Original Proposal in RESO Confluence (login required)](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2275152879/RCP+-+WEBAPI-032+Lookup+and+RelatedLookup+Resources+for+Lookup+Metadata)
* [Original Proposal in PDF format](https://github.com/RESOStandards/transport/files/14968861/RESOWebAPIRCP-RCP.-.WEBAPI-032.Lookup.and.RelatedLookup.Resources.for.Lookup.Metadata-130424-224129.pdf)

<br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO](mailto:info@reso.org) if you have any questions.
