# Legacy/Deprecated Fields and Lookups

| **RCP** | 045 |
| :--- | :--- |
| **Version** | **1.0.0** |
| **Authors** | Paul Hethmon ([AMP Systems](mailto:paul@amptech.us)) |
| **Specification** | |
| **Status** | IN PROGRESS |
| **Date Ratified** | |
| **Dependencies** | Data Dictionary 1.7+ |
| **Related Links** | |

<br /><br />

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.
<br /><br />
The keywords "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be interpreted as described in [RFC 2119](https://www.ietf.org/rfc/rfc2119.txt).

# Table of Contents
- [Summary of Changes](#summary-of-changes)
- [Introduction](#introduction)
- [Section 1: Field](#section-1-field)
- [Section 2: Lookup](#section-2-lookup)
- [Section 3: Examples](#section-3-examples)
- [Section 4: Certification](#section-4-certification)
- [Section 5: Contributors](#section-5-contributors)
- [Section 6: References](#section-6-references)
- [Section 7: License](#section-7-license)

# Summary of Changes

This specification extends the Data Dictionary Field and Lookup resource to convey information about new and deprecated fields and lookups.

# Introduction

Over time the fields and related lookup values in use by an MLS organization evolve and change. In the 2000's, the field **BusinessType** having a lookup value of **VideoStore** was applicable. In the 2020's, video stores have become an echo of the past. Our data in the MLS world though still contains historical listing data where that lookup choice is correct. In our systems today, we may not allow a new listing to use that lookup choice, but we want to inform the data consumers that it was a valid choice at one time and may appear in older data.

Fields as well evolve over time. Whether it is a field that has been retired from use or a field that has been renamed to a Data Dictionary compliant name. It is useful for an MLS organization be able to convey to data consumers when these changes happen. This specification introduces additions to the ([Field](https://ddwiki.reso.org/display/DDW17/Field+Resource)) and ([Lookup](https://ddwiki.reso.org/display/DDW17/Lookup+Resource)) resources to allow this information to be conveyed.

# Section 1: Field

The Data Dictionary 1.7 specification defines a **Field** resource with the following fields:

- DisplayName
- FieldKey
- FieldName
- ModificationTimestamp
- ResourceName

The following fields are proposed as additions:

- FieldStatus. This is a single value lookup field. The allowed values are **Obsolete**, **Deprecated**, **Active**, **Transition**
- StatusDate. The date that the give **FieldStatus** value is effective.
- ReplacedByFieldKey. References the field that replaced this field, if applicable.

For the **FieldStatus** lookups, the following definitions apply:

- Obsolete. A deprecated field indicates that the field is no longer supported. The field will NOT appear in listing data. A data consumer may have older data where this field is present, but requesting that field after the given **StatusDate** will result in an error. An older listing that once contained the given field will no longer contain that field after the **StatusDate** has past.
- Deprecated. This indicates the field is present and may be present in data but that listings after the **StatusDate** will no longer have a value for this field. Listings created prior to the **StatusDate** may have this field with a value.
- Active. An active field is the default case. The field may be present in all listing data. The **StatusDate** may be null when a field is active. If **StatusDate** has a value, it indicates the date on which the field first became available.
- Transition. This value is used to indicate that the given field will have a change on the given **StatusDate**. This can be used to inform data consumers of a **new** field being added or of a field moving to **Deprecated** or **Obsolete** status.

# Section 2: Lookup

The Data Dictionary 1.7 specification defines a **Lookup** resource with the following fields:

- LegacyODataValue
- LookupKey
- LookupName
- LookupValue
- ModificationTimestamp
- StandardLookupValue

The following fields are proposed as additions:

-- LookupStatus.  This is a single value lookup field. The allowed values are **Obsolete**, **Deprecated**, **Active**, **Transition**
- StatusDate. The date that the give **FieldStatus** value is effective.
- ReplacedByLookupKey. References the lookup that replaced this lookup, if applicable.

- Obsolete. A deprecated lookup indicates that the lookup value is no longer supported. The lookup value will NOT appear in listing data. A data consumer may have older data where this lookup value is present. An older listing that once contained the given lookup value will no longer contain that value after the **StatusDate** has past.
- Deprecated. This indicates the lookup value is present and may be present in data but that listings after the **StatusDate** will no longer have this lookup value for the field(s) that use the lookup. Listings created prior to the **StatusDate** may have a field with the given lookup value.
- Active. An active lookup is the default case. The lookup may be present in all listing data. The **StatusDate** may be null when a lookup is active. If **StatusDate** has a value, it indicates the date on which the lookup first became available.
- Transition. This value is used to indicate that the given lookup will have a change on the given **StatusDate**. This can be used to inform data consumers of a **new** lookup being added or of a lookup moving to **Deprecated** or **Obsolete** status.

# Section 3: Examples

## Field

#### Example 1
Given the following field definition:

- DisplayName -- Distance To Volcano
- FieldKey -- DistanceToVolcano
- FieldName -- DistanceToVolcano
- ModificationTimestamp -- 2023-09-30T21:18:00Z
- ResourceName -- Property
- FieldStatus -- Transition
- StatusDate -- 2023-10-15
- ReplacedByFieldKey -- (null)

If today's date is 2023-10-07 a listing with an **OriginalEntryTimestamp** of 2023-08-15T12:12:12Z MUST not have the field **DistanceToVolcano** present in the payload. 

If today's date is 2023-10-25 a listing with an **OriginalEntryTimestamp** of 2023-08-15 MAY have the field **DistanceToVolcano** present and the field MAY have a value in the payload.

Please note that the field data for **DistanceToVolcano** SHOULD have been updated on 2023-10-15 to have a FieldStatus value of **Active**. This indicates that a new field was introduced to the system and is now in use.

#### Example 2
Given the following field definition:

- DisplayName -- Distance To Video Store
- FieldKey -- DistanceToVideoStore
- FieldName -- DistanceToVideoStore
- ModificationTimestamp -- 2023-09-30T21:18:00Z
- ResourceName -- Property
- FieldStatus -- Deprecated
- StatusDate -- 2023-10-15
- ReplacedByFieldKey -- (null)

If today's date is 2023-10-07 a listing with an **OriginalEntryTimestamp** of 2023-07-30T12:30:30Z MAY have the field **DistanceToVideoStore** present and it MAY have a value.

If today's date is 2023-11-09 a listing with an **OriginalEntryTimestamp** of 2023-10-29T12:30:15Z MAY have the field **DistanceToVideoStore**, but the field MUST NOT have a value present.

## Lookup

#### Example 3
Given the field **BusinessType** and the following lookup definition:

- LegacyODataValue -- (null)
- LookupKey -- VideoStore
- LookupName -- Video Rental Store
- LookupValue -- VideoStore
- ModificationTimestamp -- 2023-09-30T19:35:30Z
- LookupStatus -- Deprecated
- StatusDate -- 2023-09-30
- ReplaceByLookupKey -- (null)

If today's date is 2023-09-15, a listing with an **OriginalEntryTimestamp** of 2023-09-01T10:30:15Z MAY have the lookup **VideoStore** for the field **BusinessType**.

If today's date is 2023-10-01, a listing with an **OriginalEntryTimestamp** of 2023-10-01:17:30:30Z MUST NOT have the lookup **VideoStore** for the field **BusinessType**.

#### Example 4
Given the field **BusinessType** and the following lookup definition:

- LegacyODataValue -- (null)
- LookupKey -- DiscoClub
- LookupName -- Disco Club
- LookupValue -- DiscoClub
- ModificationTimestamp -- 2023-09-30T19:35:30Z
- LookupStatus -- Obsolete
- StatusDate -- 2023-09-30
- ReplaceByLookupKey -- NightClub

If today's date is 2023-09-29, a listing with an **OriginalEntryTimestamp** of 2023-09-15T14:14:14Z MAY have the lookup **DiscoClub** for the field **BusinessType**.

If today's date is 2023-09-30, a listing with an **OriginalEntryTimestamp** of 2023-09-15T14:14:14Z that previously had the lookup of **DiscoClub** for the field **BusinessType** MUST now have the lookup **NightClub** for the field **BusinessType**. This means the **Obsolete** lookup of **DiscoClub** has been replaced with the lookup **NightClub** for any listing that previously had **DiscoClub** specified.

# Section 4: Certification

## Field

Certification MUST check the following:

- A field with a **FieldStatus** of **Obsolete** MUST not appear in any payload/listing.
- A field with a **FieldStatus** of **Deprecated** MUST not have a value in a listing with an **OriginalEntryTimestamp** after the **StatusDate**.

## Lookup

Certification MUST check the following:

- A lookup with a **LookupStatus** of **Obsolete** MUST not appear in any payload/listing.
- A lookup with a **LookupStatus** of **Deprecated** MUST not appear in a listing with an **OriginalEntryTimestamp** after the **StatusDate**.

# Section 5: Contributors

This document was written by [Paul Hethmon](mailto:paul@amptech.us).

| Contributor | Company |
| --- | --- |
| Paul Hethmon | AMP Systems |

# Section 6: References

* https://ddwiki.reso.org/display/DDW17/Field+Resource
* https://ddwiki.reso.org/display/DDW17/Lookup+Resource

# Section 7: License

This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO](mailto:info@reso.org) if you have any questions.