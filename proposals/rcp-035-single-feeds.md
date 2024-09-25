# RESO (Endorsement Name) Endorsement

| **RCP** | 035 |
| :--- | :--- |
| **Version** | **DD 2.1** |
| **Authors** | Eric Finlay |
| **Specification** | [RCP-035](https://github.com/RESOStandards/transport/issues/96) |
| **Status** | IN PROGRESS |
| **Date Ratified** | Pending |
| **Dependencies** | None |
| **Related Links** | [Single Feed Discussion](https://github.com/RESOStandards/transport/discussions/71)|


<br /><br />

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

<br /><br />

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

<br /><br />

# Summary of Changes

A new field called `FeedTypes` of type Open Mutli Enum String will be included in every data resource and the `Field` metadata resource. Also, a new field called `FeedRules` of type Collection(ComplexType) will be included in every data resource.

The new field `FeedTypes` will have slightly different meanings when included on data resources vs on the `Field` metadata resource.

On data resources, this `FeedTypes` field will be used by the Data Provider to indicate which "feed" includes the given record. For instances, is this record part of an IDX or VOW Feed?

On the metadata `Field` resource, the `FeedTypes` field will indicate which feeds give access to the given field. For example, the `StandardStatus` object could have `FeedTypes` set to `['IDX', 'VOW']` whereas `CloseDate` may be set to `['VOW']` indicating that the `CloseDate` field is not part of the `IDX` feed.

The `FeedRules` complex type will have 3 sub-fields - `FieldName` (String), `FeedType` (Open Enum String), and `Visible` (Boolean). The purpose of this field is to communicate overrides specific to a given record. An example of this would be an IDX Feed where `ExpirationDate` is only included based on the value of `StandardStatus`.

<br /><br />

# Introduction

Currently it is common for Data Consumers to have multiple different types of data access (hereafter referred to as "feeds") for a given MLS. An example of this could be a brokerage which could have access to an IDX, BBO, VOW, and PDAP feed, where each one is a separate set of credentials and imports that the brokerage has to be manage.

The goal of this proposal is to provide mechanisms that will allow Data Providers and Data Consumers to combine all approved feeds into a single set of credentials while still giving the Data Consumer enough information to respect the usage agreements for each record and field. If the goal is realized it will reduce duplicated effort from Data Consumers needing to download redundant data and manage multiple credentials per MLS, and it will reduce the bandwidth/computation burden on Data Providers.

<br /><br />

# Section 1: Purpose
The current problem is that some Data Consumers and Data Providers must manage multiple credentials for a single MLS/Data Consumer relationship to support sharing redundant data under different data licensing agreements, causing increased load, complexity, and duplication.

The goal of this proposal is to provide a mechanism so that each Data Consumer only needs a single set of credentials for a given MLS, while giving enough information for the Data Consumer to respect all data licensing requirements.

This will be done by the creation of the `FeedTypes` field of type Open Mutli Enum String on all data resources and the `Field` metadata resource and the creation of the `FeedRules` field of type Collection(ComplexType) on all data resources. The purpose of these fields will be to communicate which feed a given record or field is part of, and when it may be used.

<br /><br />

# Section 2: Specification

The proposal is to add two fields: `FeedTypes` of type Open Mutli Enum String to all data resources and the `Field` metadata resource amd `FeedRules` of type Collection(ComplexType) to all data resources.

| Field | Resource/Model | Data Type | Sample Value | Nullable | Description |
| - | - | - | - | - | - |
| **FeedTypes** | Any Data Resource | Collection(Edm.String) | ["IDX", "BBO"] | False | A list of feeds that the given record is included in. |
| **FeedTypes** | Field Resource | Collection(Edm.String) | ["IDX"] | False | A list of feeds that the given field is part of. |
| **FeedRules** | Any Data Resource | Collection(ComplexTypes.FeedRule) | [{"FieldName": "PrivateRemarks", "FeedType": "IDX", "Visible": false}] | True | A collection of rules describing whether a given field on the record should be visible or not for a given feed. |
| **FieldName** | ComplexTypes.FeedRule | Edm.String | "PrivateRemarks" | False | The field that the given rule applies to. |
| **FeedType** | ComplexTypes.FeedRule | Edm.String | "IDX" | False | The feed that the given rule applies to. |
| **Visible** | ComplexTypes.FeedRule | Edm.Boolean | false | False | Whether the `FieldName` should be displayed as part of the `FeedType` for the containing Data record. |

## Section 2.1 FeedTypes

The initial list of potential values would be `IDX`, `VOW`, `BBO`, and `PDAP`. These values are labels and have no guarantee of matching industry-wide understandings of these terms. The only requirement is that they are meaningful to the Data Consumer and they are able to discern which data license agreement a given `FeedTypes` value is referencing, so they can correctly meet the restrictions of the data license agreement.

## Section 2.2 FeedRules

The `FeedRules` Collection(ComplexType) will have 3 sub-fields: `FieldName` (String), `FeedType` (Oepn Enum String), and `Visible` (Boolean). The `FeedType` enum will have the same potential values as the `FeedTypes` enum. The purpose of this field will be to communicate overrides specific to a given record, usually based on the values of other fields.

Example response for a data resource:

```
@odata.context: "https://host.com/path...",
value: [{
    "ListPrice": 10000,
    "StandardStatus": "Active",
    "PrivateRemarks": "This is a private remark",
    "FeedTypes": ["IDX", "VOW"],
    "FeedRules": [{
        "FieldName": "PrivateRemarks",
        "FeedType": "IDX",
        "Visible": false
    }]
    ...
}, {
    "ListPrice": 20000,
    "StandardStatus": "Closed",
    "FeedTypes": ["VOW"],
    "FeedRules": null
    ...
}]
```

Example response for the `Field` metadata resource:

```
@odata.context: "https://host.com/path...",
value: [{
    "FieldName": "ListPrice",
    "FeedTypes": ["IDX", "VOW"],
    ...
}, {
    "FieldName": "CloseDate",
    "FeedTypes": ["VOW"],
    ...
}]
```

<br /><br />

# Section 3: Certification

If the `FeedTypes` field is present on a data resource or the `Field` metadata resource, it must be of type Open Mutli Enum String.

If the `FeedRules` field is present on a data resource, it must be of type Collection(ComplexType - FieldName String, FeedType Open Enum String, Visible Boolean).

<br /><br />

# Section 4. Contributors
This document was written by [Eric Finlay](ericfi@zillowgroup.com), [Author 1](mailto:author1@company.org), and [Author 2](mailto:author2@company.org).

Thanks to the following contributors for their help with this project:

| Contributor | Company |
| --- | --- |
| Joshua Darnell | RESO |
| Geoff Rispin | Templates 4 Business |
| Dave Bills | Pike's Peak Association of REALTORS® |
| AJ Sulayman | Canopy MLS |
| Eric Stegemann | TRIBUS |
| John Breault | State-Wide MLS |

<br /><br />

# Section 5: References

<br /><br />

# Section 6: Appendices

The following RCPs are related to Data Dictionary 2.1:
* [RCP-042 - Model and Field Resources](https://github.com/RESOStandards/transport/issues/76)
* [RCP-043 - Local Fields and Predictability](https://github.com/RESOStandards/transport/issues/77)
* [RCP-044 - Metadata Internationalization and Locale](https://github.com/RESOStandards/transport/issues/67)
* [RCP-045 - Legacy and/or Deprecated Data Elements](https://github.com/RESOStandards/transport/pull/104)

<br /><br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO](mailto:info@reso.org) if you have any questions.
