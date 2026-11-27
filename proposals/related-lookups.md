# RESO RelatedLookup Resource Endorsement

| **RCP** | 47 |
| :--- | :--- |
| **Version** | **1.0.0** |
| **Authors** | [Joshua Darnell (RESO)](mailto:josh@reso.org)<br />[Ryan Yates (Rapattoni Corporation)](mailto:ryates@rapattoni.com) |
| **Status** | IN PROGRESS |
| **Date Ratified** | TBD |
| **Dependencies** | [Data Dictionary 2.1+](https://dd.reso.org/DD2.1/)<br />[Web API Core 2.0.0+](https://transport.reso.org/proposals/web-api-core/) (2.1.0 for the `in` operator) |
| **Related Links** | [Lookup Resource, Data Dictionary 2.1](https://dd.reso.org/DD2.1/Lookup/)<br />[RelatedLookup Resource, Data Dictionary 2.1](https://dd.reso.org/DD2.1/RelatedLookup/)<br />[Data Dictionary endorsement, Section 2.2: Lookup Resource](https://transport.reso.org/proposals/data-dictionary/#section-22-lookup-resource-for-enumeration-metadata)<br />[RCP-42 Model and Field Resources](https://github.com/RESOStandards/transport/blob/b21579a147a476a95fb0a9457ab6b23d5b7afc0a/proposals/model-and-field-resources.md)<br />[RCP-45 Legacy and Deprecated Data Elements](https://github.com/RESOStandards/transport/blob/ddb8bc9792173ef1a1d57eb5cb560af21d0fc558/proposals/rcp-45-legacy-deprecated-fields-lookups.md)<br />[RCP-55 Organization and System Identifiers](https://github.com/RESOStandards/transport/blob/46619fdd2d383938ef86c6efff0f01e82c19d6c0/proposals/org-system-identifiers.md) |

<br />

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be interpreted as described in [RFC 2119](https://www.ietf.org/rfc/rfc2119.txt).

<br />

# Table of Contents
- [Summary of Changes](#summary-of-changes)
- [Introduction](#introduction)
- [Section 1: Purpose](#section-1-purpose)
- [Section 2: Specification](#section-2-specification)
  - [Section 2.1: RelatedLookup Resource](#section-21-relatedlookup-resource)
  - [Section 2.2: Query Support](#section-22-query-support)
  - [Section 2.3: Examples](#section-23-examples)
  - [Section 2.4: Navigation Property Path Queries](#section-24-navigation-property-path-queries)
  - [Section 2.5: Impact](#section-25-impact)
- [Section 3: Certification](#section-3-certification)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: Appendices](#section-6-appendices)
- [Section 7: License](#section-7-license)

<br />

# Summary of Changes
* Specifies the **RelatedLookup Resource**, which models a directed relationship from one Lookup Resource record to another so that a data consumer can tell which enumerated values go together, for example which counties are in a given state or province and which schools are in a given county. The resource and its three fields (`LookupKey`, `RelatedLookupKey`, `ModificationTimestamp`) are defined in Data Dictionary 2.1; this endorsement specifies their transport behavior and certification.
* Defines the **query support** a provider MUST offer on the RelatedLookup Resource and, for providers of this endorsement, on the Lookup Resource: filtering by `LookupKey` with `eq` and `or`, filtering by `LookupName` combined with `LookupKey`, and the OData 4.01 `in` operator for Web API Core 2.1.0 providers that advertise OData 4.01.
* Defines an OPTIONAL **navigation property** from the Lookup Resource to the RelatedLookup Resource so that a lookup and its related lookups can be fetched in a single query.
* Additive. This is a new endorsement at version 1.0.0 and a MAY endorsement for Web API Core 2.0.0+ and Data Dictionary 2.1+. No existing field, lookup or resource changes; the only requirement it adds to an existing resource is the Lookup Resource filtering in [Section 2.2](#section-22-query-support), which applies only to providers of this endorsement.

<br />

# Introduction

Conveying information about related enumerations is important in cases such as displaying accurate values for related option lists on search and input forms: the list of counties offered once a state or province has been chosen, or the list of cities offered once a county has been chosen.

This could be accomplished using computable business rules, such as those offered by the [RESO Validation Expressions endorsement (RCP-19)](https://transport.reso.org/proposals/validation-expressions/). However, that approach presents implementation challenges because of the overhead of writing parsers and interpreters.

Since metadata for enumerations is already conveyed by the [Lookup Resource](https://dd.reso.org/DD2.1/Lookup/), it is easier to build on that resource and address the case with resource-based metadata rather than with a grammar. The RelatedLookup Resource originates in the same proposal as the Lookup Resource (WEBAPI-032, linked in [Section 5](#section-5-references)); the Lookup Resource has been part of the Data Dictionary since version 1.7 and is defined in Section 2.2 of the Data Dictionary endorsement; this endorsement carries the RelatedLookup portion.

<br />

# Section 1: Purpose

A data consumer that knows one enumerated value often needs the values that go with it: the counties in a given state or province, the cities in a given county, the schools in a given county. The Lookup Resource conveys each enumeration but not the relationships between them, so a consumer has to obtain those relationships from the provider by other means, or not at all.

This endorsement adds support for defining relationships between enumerations by specifying the RelatedLookup Resource, a Data Dictionary 2.1 resource that carries a directed relationship from one Lookup Resource record to another, together with the queries a consumer uses to retrieve the related lookups and the rules RESO validates during certification.

<br />

# Section 2: Specification

## Section 2.1: RelatedLookup Resource

The [RelatedLookup Resource](https://dd.reso.org/DD2.1/RelatedLookup/) models relationships between Lookup Resource records. It is defined in Data Dictionary 2.1 with the fields below.

A RelatedLookup record is a directed edge from the Lookup Resource record identified by `LookupKey` to the Lookup Resource record identified by `RelatedLookupKey`, with a `ModificationTimestamp` for replication:

| Field | Type | Nullable | Max length | Definition |
| :--- | :--- | :--- | :--- | :--- |
| LookupKey | String | No | 255 | The unique key of the Lookup resource item the relationship originates from. |
| RelatedLookupKey | String | No | 255 | The unique key of the Lookup resource item the relationship terminates at. |
| ModificationTimestamp | Timestamp | No | 27 | The ISO 8601 timestamp for when the relationship was last modified. |

In the OData XML metadata the two keys are `Edm.String` and the timestamp is `Edm.DateTimeOffset`, each with `Nullable="false"`. The names, types and lengths are Data Dictionary 2.1's; this endorsement edits the definition of `RelatedLookupKey` and changes nothing else about the three fields.

The entity key of the RelatedLookup Resource is the combination of `LookupKey` and `RelatedLookupKey`, so a provider serves at most one record for a given pair of lookups. In the OData XML metadata the `Key` lists both as `PropertyRef` elements; in the Field Resource each of the two carries `PrimaryKeyYN` = `true` and, because the key is compound, the RelatedLookup Model record's `PrimaryKeyFieldKey` is null, as [RCP-42 Model and Field Resources](https://github.com/RESOStandards/transport/blob/b21579a147a476a95fb0a9457ab6b23d5b7afc0a/proposals/model-and-field-resources.md) defines.

`LookupKey` and `RelatedLookupKey` MUST each be the `LookupKey` of a record in the provider's Lookup Resource. A consumer resolves either value by fetching that record from the Lookup Resource, as shown in [Section 2.3](#section-23-examples).

A relationship is directed and is read from `LookupKey` to `RelatedLookupKey`. The presence of a record from lookup A to lookup B does not imply a record from B to A; a provider that wants a relationship to be discoverable from either lookup publishes both records.

A provider MAY add local fields to the RelatedLookup Resource. Because the resource can be very large, a provider SHOULD keep local fields to a minimum.

A common use case is that a data consumer already knows the `LookupKey` it is interested in and retrieves the `RelatedLookupKey` values for it from the RelatedLookup Resource, then retrieves those records from the Lookup Resource.

## Section 2.2: Query Support

A provider of this endorsement MUST support the following queries.

**RelatedLookup Resource**

* Replication using the OData `$top` and `$skip` query options, `$count=true`, and `$filter` on `ModificationTimestamp`, in the same way the [Lookup Resource](https://transport.reso.org/proposals/data-dictionary/#section-22-lookup-resource-for-enumeration-metadata) is replicated. The consumer MUST be able to retrieve the advertised count of records.
* `$filter` on `LookupKey` using the `eq` comparison operator, including comparisons combined with the `or` logical operator.

**Lookup Resource**

* `$filter` on `LookupKey` using `eq`, including comparisons combined with `or`.
* `$filter` on `LookupName` using `eq`, combined with a `LookupKey` filter using `and`, so a consumer can restrict related lookups to one enumeration.

The Data Dictionary endorsement makes filtering the Lookup Resource by `LookupName` optional; this endorsement requires it, and the `LookupKey` filter above, for providers of this endorsement.

**The `in` operator**

The OData 4.01 `in` operator is a shorthand for a chain of `eq` comparisons joined by `or`: `LookupKey in ('ABC123', 'X22')` is equivalent to `LookupKey eq 'ABC123' or LookupKey eq 'X22'`. A provider of Web API Core 2.1.0 or later that advertises OData 4.01 in its response headers MUST support `in` on `LookupKey` for both resources, and it MUST return the same records for the `in` form as for the equivalent `or` form. This is the condition under which Web API Core 2.1.0 tests the `in` operator. Any other provider, one on Web API Core 2.0.0 or one that does not advertise OData 4.01, MAY support `in`, but it is not tested. Providers MUST support the `or` form in every case: the `or` logical operator is part of Web API Core 2.0.0+, and it is the common case.

Web API Core does not specify string comparison on fields that are not enumerations (Web API Core 2.1.0, Section 2.5.9), although its Lookup Resource validation fetches the Lookup Resource by `LookupName`; the `eq` comparisons on `LookupKey` and `LookupName` above are therefore requirements of this endorsement rather than of Web API Core.

## Section 2.3: Examples

Requests and responses are shown against `https://api.example.com`. Keys and timestamps are illustrative. In these examples "ABC123" is the `LookupKey` of the `CountyOrParish` value "Los Angeles County", as in the Lookup Resource examples of the Data Dictionary endorsement.

### Example 1: Get the RelatedLookup Records for a Given Lookup

**REQUEST**
```
GET https://api.example.com/RelatedLookup?$filter=LookupKey eq 'ABC123'
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "https://api.example.com/$metadata#RelatedLookup",
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

In this example:
* "X22" is the `LookupKey` of the `StateOrProvince` value "CA", the state in which Los Angeles County is located.
* "Y33" is the `LookupKey` of the `City` value "City of Los Angeles".
* "Z44" is the `LookupKey` of a local `Subdivision` value "Sawtelle".

`Subdivision` is not an enumerated field in the Data Dictionary; the Data Dictionary field is [SubdivisionName](https://dd.reso.org/DD2.1/Property/SubdivisionName/), a string. There have been requests to add an enumerated subdivision field to the Data Dictionary, and nothing prevents a provider from adding a local enumeration for subdivisions, as in this example.

### Example 2: Get the Lookup Records for Given RelatedLookup Records

Once the related lookups have been retrieved, the data consumer queries the Lookup Resource for those records, unless it has already replicated the Lookup Resource and is processing it locally.

**REQUEST**
```
GET https://api.example.com/Lookup?$filter=LookupKey eq 'ABC123' or LookupKey eq 'X22' or LookupKey eq 'Y33' or LookupKey eq 'Z44'
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "https://api.example.com/$metadata#Lookup",
  "value": [
    {
      "LookupKey": "ABC123",
      "LookupName": "CountyOrParish",
      "LookupValue": "Los Angeles County",
      "StandardLookupValue": null,
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

A Web API Core 2.1.0 provider that advertises OData 4.01 also accepts the `in` form of the same query, which MUST return the same records ([Section 2.2](#section-22-query-support)):

```
GET https://api.example.com/Lookup?$filter=LookupKey in ('ABC123', 'X22', 'Y33', 'Z44')
HTTP/2
```

The examples in this document use the `or` form because it is supported by every provider.

### Example 3: Get All City Values for a Given CountyOrParish

To retrieve only the `City` lookups related to the `CountyOrParish` value "Los Angeles County" (`LookupKey` "ABC123"), the consumer adds `LookupName eq 'City'` to the query from Example 2:

**REQUEST**
```
GET https://api.example.com/Lookup?$filter=LookupName eq 'City' and (LookupKey eq 'X22' or LookupKey eq 'Y33' or LookupKey eq 'Z44')
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "https://api.example.com/$metadata#Lookup",
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

Only one record is returned because only "Y33" is a `City` lookup. A `LookupName` can be used by more than one field; the Field Resource links each field to the `LookupName` it uses.

## Section 2.4: Navigation Property Path Queries

The examples in [Section 2.3](#section-23-examples) need more than one query to fetch the related lookups. A provider MAY define an OData navigation property from the Lookup Resource to the RelatedLookup Resource so that a lookup and its related lookups can be fetched in a single query using an OData navigation property path. Support for this navigation property is OPTIONAL.

When defined, the navigation property is a field of the Lookup Resource:

| Field | Type | Nullable | Definition |
| :--- | :--- | :--- | :--- |
| RelatedLookup | Collection (RelatedLookup) | No | A collection of RelatedLookup items for the relationships that originate from the Lookup record. |

For a given Lookup record it contains the RelatedLookup records whose `LookupKey` equals that record's `LookupKey`, that is, the relationships that originate from the lookup. It is a collection and is never null: a lookup with no relationships carries the empty collection, `[]`, as for every collection in the Data Dictionary. It is the one element this endorsement adds to the Data Dictionary, and it is OPTIONAL.

A provider that does not define the navigation property returns a non-2XX status code for requests that use it.

### Example 4: Get a Lookup and Its Related City Lookups in a Single Query

In this example the provider publishes each relationship in both directions ([Section 2.1](#section-21-relatedlookup-resource)), so the `City` record that Example 1 relates to Los Angeles County also carries a RelatedLookup record that terminates at "ABC123". The query returns the county and the cities whose relationships terminate at it.

**REQUEST**
```
GET https://api.example.com/Lookup?$filter=LookupKey eq 'ABC123' or (RelatedLookup/any(a:a/RelatedLookupKey eq 'ABC123') and LookupName eq 'City')
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "https://api.example.com/$metadata#Lookup",
  "value": [
    {
      "LookupKey": "ABC123",
      "LookupName": "CountyOrParish",
      "LookupValue": "Los Angeles County",
      "StandardLookupValue": null,
      "ModificationTimestamp": "2020-07-07T17:36:14Z"
    },
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

## Section 2.5: Impact

This is a MAY endorsement for Web API Core 2.0.0+ and Data Dictionary 2.1+. It is additive: the RelatedLookup Resource and its fields are defined in Data Dictionary 2.1, no existing field, lookup or resource is renamed, removed or changed in type, and the Lookup Resource is unchanged apart from the query support in [Section 2.2](#section-22-query-support) and the optional navigation property in [Section 2.4](#section-24-navigation-property-path-queries), both of which apply only to providers of this endorsement.

<br />

# Section 3: Certification

RESO will validate the following during certification, when the RelatedLookup Resource is present:

**Metadata**
* The RelatedLookup Resource MUST be defined in the server metadata with the field names and types of the Data Dictionary 2.1 definition, `LookupKey` (`Edm.String`), `RelatedLookupKey` (`Edm.String`) and `ModificationTimestamp` (`Edm.DateTimeOffset`), each non-nullable, and with the entity key of [Section 2.1](#section-21-relatedlookup-resource). Local fields MAY be present.

**Replication**
* All records MUST be retrievable from the RelatedLookup Resource using `$top` and `$skip`, and the number of records retrieved MUST equal the count advertised by `$count=true`.
* Every record retrieved MUST carry a non-null `LookupKey`, `RelatedLookupKey` and `ModificationTimestamp`, and the payload MUST match the server metadata.
* Every `LookupKey` and `RelatedLookupKey` value in the records sampled from the RelatedLookup Resource MUST be present as a `LookupKey` in the Lookup Resource.

**Queries**
* `$filter=LookupKey eq '<key>'` on the RelatedLookup Resource, alone or joined by `or` with a second key, MUST return only records with the requested `LookupKey` values.
* `$filter` on the Lookup Resource with `LookupKey eq` comparisons joined by `or` MUST return exactly the requested records; the `or` form is tested for Web API Core 2.0.0+.
* `$filter` on the Lookup Resource combining `LookupName eq '<name>'` with a `LookupKey` filter using `and` MUST return only records with that `LookupName`.
* When the server is tested for Web API Core 2.1.0 or later and advertises OData 4.01, the `in` form of the `LookupKey` filter MUST return the same records as the `or` form, on both resources. This is the condition under which Web API Core 2.1.0 tests `in`; a server on Web API Core 2.0.0, or one that does not advertise OData 4.01, is not tested for it.

**Navigation property**
* When the server metadata defines an OData `NavigationProperty` from the Lookup Resource to the RelatedLookup Resource, the navigation property path query of [Section 2.4](#section-24-navigation-property-path-queries) MUST be tested and MUST return exactly the Lookup records for which the RelatedLookup Resource holds a record whose `LookupKey` is that record's `LookupKey` and whose `RelatedLookupKey` is the requested key.

<br />

# Section 4: Contributors
This document was written by [Joshua Darnell (RESO)](mailto:josh@reso.org) and [Ryan Yates (Rapattoni Corporation)](mailto:ryates@rapattoni.com).

Thanks to the RESO Transport Workgroup for the feedback they gave during the creation of this proposal.

<br />

# Section 5: References

Please see the following references for more information regarding topics covered in this document:
* [RESO Data Dictionary 2.1 – Lookup Resource](https://dd.reso.org/DD2.1/Lookup/)
* [RESO Data Dictionary 2.1 – RelatedLookup Resource](https://dd.reso.org/DD2.1/RelatedLookup/)
* [RESO Data Dictionary endorsement – Section 2.2: Lookup Resource for Enumeration Metadata](https://transport.reso.org/proposals/data-dictionary/#section-22-lookup-resource-for-enumeration-metadata)
* [RESO Web API Core 2.1.0](https://transport.reso.org/proposals/web-api-core/)
* [RESO Validation Expressions (RCP-19)](https://transport.reso.org/proposals/validation-expressions/)
* [OData Version 4.01 Part 2: URL Conventions](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html) (the `in` operator, the `any` lambda operator and navigation property paths)
* [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601)
* [Original proposal: WEBAPI-032 Lookup and RelatedLookup Resources for Lookup Metadata (RESO Confluence, login required)](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2275152879/RCP+-+WEBAPI-032+Lookup+and+RelatedLookup+Resources+for+Lookup+Metadata)
* [Original proposal in PDF format](https://github.com/RESOStandards/transport/files/14968861/RESOWebAPIRCP-RCP.-.WEBAPI-032.Lookup.and.RelatedLookup.Resources.for.Lookup.Metadata-130424-224129.pdf)

<br />

# Section 6: Appendices

## Proposed Data Dictionary elements

The elements this endorsement relies on are defined in [Section 2.1](#section-21-relatedlookup-resource) (the RelatedLookup Resource and its three fields, which Data Dictionary 2.1 already carries; this endorsement edits the definition of `RelatedLookupKey`) and [Section 2.4](#section-24-navigation-property-path-queries) (the OPTIONAL `RelatedLookup` navigation property on the Lookup Resource, the one new element). No lookups or lookup values are proposed. Nothing is deprecated.

<br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO Transport](mailto:transport@reso.org) with questions about this proposal, or [RESO developer support](mailto:dev@reso.org) with specific development questions.
