# Legacy and Deprecated Data Elements

| **RCP** | 45 |
| :--- | :--- |
| **Version** | **1.0.0** |
| **Authors** | Paul Hethmon ([AMP Systems](mailto:paul@amptech.us)) |
| **Status** | IN PROGRESS |
| **Date Ratified** | |
| **Dependencies** | Data Dictionary 1.7+ |
| **Related Links** | [**RCP 42**](https://github.com/RESOStandards/transport/issues/76)<br/>[**RCP 44**](https://github.com/RESOStandards/transport/issues/67)|

<br /><br />

# RESO End User License Agreement (EULA)

[**This End User License Agreement (the "EULA")**](https://www.reso.org/eula/) is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

The keywords "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be interpreted as described in [RFC 2119](https://www.ietf.org/rfc/rfc2119.txt).

<br />

# Table of Contents
- [Introduction](#introduction)
- [Section 1: Purpose](#section-1-purpose)
- [Section 2: Specification](#section-2-specification)
- [Section 3: Certification](#section-3-certification)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: Appendices](#section-6-appendices)
- [Section 7: License](#section-7-license)

# Introduction

This specification extends the Data Dictionary Field and Lookup resource to convey information about new and deprecated models, fields, and lookups.

# Section 1: Purpose

Over time the fields and related lookup values in use by an MLS organization evolve and change. In the 2000's, the field **BusinessType** having a lookup value of **Video Store** was applicable. In the 2020's, video stores have become an echo of the past. Our data in the MLS world though still contains historical listing data where that lookup choice is correct. In our systems today, we may not allow a new listing to use that lookup choice, but we want to inform the data consumers that it was a valid choice at one time and may appear in older data.

Fields as well evolve over time. Whether it is a field that has been retired from use or a field that has been renamed to a Data Dictionary compliant name. It is useful for an MLS organization to be able to convey to data consumers when these changes happen. 

This specification introduces additions to the [Field](https://ddwiki.reso.org/display/DDW17/Field+Resource), [Lookup](https://ddwiki.reso.org/display/DDW17/Lookup+Resource), and upcoming Model resources to allow this information to be conveyed.

# Section 2: Specification

## Field Resource

The Data Dictionary 1.7 specification defines the **Field** Resource with the following fields:

- **DisplayName**
- **FieldKey**
- **FieldName**
- **ModificationTimestamp**
- **ResourceName**

_Note: ResourceName will be deprecated in favor of ModelKey starting in Data Dictionary 2.1._

The following fields are proposed as additions:

- **FieldStatus**: closed enumeration with allowed values: **Obsolete**, **Deprecated**, **Active**, **Transition**
- **StatusDate**: the date that the give **FieldStatus** value is effective.
- **ReplacedByFieldKey**: references the field that replaced this field, if applicable.

For the **FieldStatus** lookups, the following definitions apply:

- **Obsolete**: a status of obsolete indicates that the field is no longer supported. The field will NOT appear in records. A data consumer may have older data where this field is present, but requesting that field after the given **StatusDate** will result in an error. An older record that once contained the given field will no longer contain that field after the **StatusDate** has passed.
- **Deprecated**: indicates the field is present in the system and may be present in historical data but records after the **StatusDate** will no longer have a value for this field. Records created prior to the **StatusDate** may have this field with a value.
- **Active**: an active field is the default case. The field may be present in all data. The **StatusDate** may be null when a field is active. If **StatusDate** has a value, it indicates the date on which the field first became available.
- **Transition**: indicates that the given field will have a change on the given **StatusDate**. This can be used to inform data consumers of a **new** field being added or of a field moving to **Deprecated** or **Obsolete** status.

## Lookup Resource

The Data Dictionary 1.7 specification defines a **Lookup** resource with the following fields:

- **LegacyODataValue**
- **LookupKey**
- **LookupName**
- **LookupValue**
- **ModificationTimestamp**
- **StandardLookupValue**

The following fields are proposed as additions:

- **LookupStatus**: closed enumeration with allowed values: **Obsolete**, **Deprecated**, **Active**, **Transition**
- **StatusDate**: the date that the give **LookupStatus** value is effective.
- **ReplacedByLookupKey**: references the lookup that replaced this lookup, if applicable.

For the **LookupStatus** lookups, the following definitions apply:

- **Obsolete**: a status of obsolete indicates that the lookup is no longer supported. The lookup value will NOT appear in records. A data consumer may have older data where this lookup value is present. An older record that once contained the given lookup value will no longer contain that value after the **StatusDate** has passed.
- **Deprecated**: indicates the lookup value is present in the system and may be present in data but that records after the **StatusDate** will no longer have this lookup value for the field(s) that use the lookup. Records created prior to the **StatusDate** may have a field with the given lookup value.
- **Active**: an active lookup is the default case. The lookup may be present in records. The **StatusDate** may be null when a lookup is active. If **StatusDate** has a value, it indicates the date on which the lookup first became available.
- **Transition**: this value is used to indicate that the given lookup will have a change on the given **StatusDate**. This can be used to inform data consumers of a **new** lookup being added or of a lookup moving to **Deprecated** or **Obsolete** status.

## Model Resource
Starting in Data Dictionary 2.1, there will be a new resource called Model which supports both resources and custom data types.

- **ModelKey**
- **ModelName**
- **ModelType**
- **ModificationTimestamp**

The following fields are proposed as additions:
- **ModelStatus**: closed enumeration with allowed values: **Obsolete**, **Deprecated**, **Active**, **Transition**
- **StatusDate**: the date that the give **ModelStatus** value is effective.
- **ReplacedByModelKey**: references the model that replaced this model, if applicable.

For the **ModelStatus** lookups, the following definitions apply:

- **Obsolete**: a status of obsolete indicates that the lookup is no longer supported. The model will NOT appear in the system metadata. A data consumer may have older records that used this model, but requesting that model after the given **StatusDate** will result in an error. 
- **Deprecated**: indicates the model is present and may be present in historical data but records after the **StatusDate** will no longer use this model. Records created prior to the **StatusDate** may have data in this model.
- **Active**: an active model is the default case. The **StatusDate** may be null when a field is active. If **StatusDate** has a value, it indicates the date on which the model first became available.
- **Transition**: indicates that the given model will have a change on the given **StatusDate**. This can be used to inform data consumers of a **new** model being added or of a model moving to **Deprecated** or **Obsolete** status.

## Examples

### Example 1: Field in Transition
The following Field metadata shows a field in transition:

```json
{ 
  "@odata.context":"https://example.api.com/Field('789eb75282')",
  "FieldKey": "789eb75282",
  "FieldName": "DistanceToVolcano",
  "DisplayName": "Distance To Volcano",
  "ResourceName": "Property",
  "ModificationTimestamp": "2023-09-30T21:18:00Z",
  "FieldStatus": "Transition",
  "StatusDate": "2023-10-15",
  "ReplacedByFieldKey": null
}
```

If today's date is `2023-10-07` a record with an **OriginalEntryTimestamp** of `2023-08-15T12:12:12Z` MUST not have the field **DistanceToVolcano** present in the payload. 

If today's date is `2023-10-25` a record with an **OriginalEntryTimestamp** of `2023-08-15T12:12:12Z` MAY have the field **DistanceToVolcano** present and the field MAY have a value in the payload.

Please note that the field data for **DistanceToVolcano** SHOULD have been updated on `2023-10-15` to have a FieldStatus value of **Active**. This indicates that a new field was introduced to the system and is now in use.

### Example 2: Deprecated Field
The following Field metadata shows a deprecated field:

```json
{
  "@odata.context":"https://example.api.com/Field('6c5fca4adc')",
  "FieldKey": "6c5fca4adc",
  "FieldName": "DistanceToVideoStore",
  "DisplayName": "Distance To Video Store",
  "ResourceName": "Property",
  "ModificationTimestamp": "2023-09-30T21:18:00Z",
  "FieldStatus": "Deprecated",
  "StatusDate": "2023-10-15",
  "ReplacedByFieldKey": null
}
```

If today's date is `2023-10-07` a record with an **OriginalEntryTimestamp** of `2023-07-30T12:30:30Z` MAY have the field **DistanceToVideoStore** present and it MAY have a value.

If today's date is `2023-11-09` a record with an **OriginalEntryTimestamp** of `2023-10-29T12:30:15Z` MAY have the field **DistanceToVideoStore**, but the field MUST NOT have a value present.

### Example 3: Deprecated Lookup 
Given the field **BusinessType** and the following lookup definition:

```json
{
  "@odata.context": "https://example.api.org/Lookup('3e56115fde')",
  "LookupKey": "3e56115fde",
  "LookupName": "Business Types",
  "LookupValue": "Video Store",
  "LegacyODataValue": null,
  "ModificationTimestamp": "2023-09-30T19:35:30Z",
  "LookupStatus": "Deprecated",
  "StatusDate": "2023-09-30",
  "ReplaceByLookupKey": null
}
```

If today's date is `2023-09-15`, a record with an **OriginalEntryTimestamp** of `2023-09-01T10:30:15Z` MAY have the lookup **Video Store** for the field **BusinessType**.

If today's date is `2023-10-01`, a record with an **OriginalEntryTimestamp** of `2023-10-01:17:30:30Z` MUST NOT have the lookup **Video Store** for the field **BusinessType**.

### Example 4: Obsolete Lookup
Given the field **BusinessType** and the following lookup definition:

```json
{
  "@odata.context": "https://example.api.com/Lookup?$filter=LookupName eq 'Business Types'",
  "value": [{
      "LegacyODataValue": null,
      "LookupKey": "b6e859ebf1",
      "LookupName": "Business Types",
      "LookupValue": "Disco Club",
      "ModificationTimestamp": "2023-09-30T19:35:30Z",
      "LookupStatus": "Obsolete",
      "StatusDate": "2023-09-30",
      "ReplaceByLookupKey": "712b6094e3"
    },
    {
      "LegacyODataValue": null,
      "LookupKey": "712b6094e3",
      "LookupName": "Business Types",
      "LookupValue": "Night Club",
      "ModificationTimestamp": "2023-09-30T19:35:30Z",
      "LookupStatus": "Active",
      "StatusDate": "2023-09-30",
      "ReplaceByLookupKey": null
    }
  ]
}
```

If today's date is `2023-09-29`, a record with an **OriginalEntryTimestamp** of `2023-09-15T14:14:14Z` MAY have the lookup **Disco Club** for the field **BusinessType**.

If today's date is `2023-09-30`, a record with an **OriginalEntryTimestamp** of `2023-09-15T14:14:14Z` that previously had the lookup of **Disco Club** for the field **BusinessType** MUST now have the lookup **Night Club** for the field **BusinessType**. This means the **Obsolete** lookup of **Disco Club** has been replaced with the lookup **Night Club** for any record that previously had **Disco Club** specified.

# Section 3: Certification

## Field
For Data Dictionary 1.7+, Certification MUST check the following:

- A field with a **FieldStatus** of **Obsolete** MUST NOT appear in any payload/record.
- A field with a **FieldStatus** of **Deprecated** MUST NOT have a value in a record with an **OriginalEntryTimestamp** after the **StatusDate**.

## Lookup
For Data Dictionary 1.7+, Certification MUST check the following:

- A lookup with a **LookupStatus** of **Obsolete** MUST NOT appear in any payload/record.
- A lookup with a **LookupStatus** of **Deprecated** MUST NOT appear in a record with an **OriginalEntryTimestamp** after the **StatusDate**.

## Model
Starting in Data Dictionary 2.1, Certification MUST check the following:

- A model with a **ModelStatus** of **Obsolete** MUST NOT appear in any payload/record.
- A model with a **ModelStatus** of **Deprecated** MUST NOT have a value in a record with an **OriginalEntryTimestamp** after the **StatusDate**.

For models without an OriginalEntryTimestamp field, the corresponding Model/Field/Lookup status will be checked to ensure data elements are present in the given payload when they're supposed to be and that no obsolete items are present.

# Section 4: Contributors

This document was written by [Paul Hethmon](mailto:paul@amptech.us).

| Contributor | Company |
| --- | --- |
| Paul Hethmon | AMP Systems |

# Section 5: References

* https://ddwiki.reso.org/display/DDW17/Field+Resource
* https://ddwiki.reso.org/display/DDW17/Lookup+Resource

# Section 6: Appendices

None

# Section 7: License

This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO](mailto:info@reso.org) if you have any questions.
