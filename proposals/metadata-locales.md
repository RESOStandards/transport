# RESO Metadata Internationalization and Locale

| **RCP** | 44 |
| :--- | :--- |
| **Version** | **1.0.0** |
| **Authors** | [Josh Darnell (RESO)](mailto:josh@reso.org) |
| **Status** | IN PROGRESS |
| **Date Approved** | TBD |
| **Dependencies** | Data Dictionary 2.2+ (Shapes/OData ComplexTypes)|


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
* Provides a way of communicating locale-specific display names for models, fields, and enumerations using standard ISO identifiers that's easily searchable.
* Adds a new OData ComplexType (shape) called I18nString that encapsulates the locale and value of the display name. 
* Adds a new OData ComplexType (shape) to the Model, Field, and Lookup resources called DisplayNames.

<br />

# Section 1: Purpose

Supporting regional and international metadata allows systems to display localized names for RESO Data Dictionary elements, which are based on U.S. English, while preserving interoperability of standard data elements. 

This RCP adds fields in the Model, Field, and Lookup resources that allow a collection of name/value pairs to be communicated and searched. It uses ISO standards for locales, which are common in internationalization (i18n) applications, making localization (i10n) much easier. 

A collection of `DisplayNames` was used rather than a map to support filtering with any/all queries in OData, providing a simpler and more dynamic interface for end users. Clients can request metadata display names in the locale(s) of their choosing without metadata introspection to determine whether they're supported or not. If not, then no value matching the requested locale will be returned. 

<br />

# Section 2: Specification

## Section 2.1: Metadata Definition of `I18nString` and `DisplayNames` Collection

This specification creates a new shape called `I18nString`, defined as follows: 

```xml
<ComplexType Name="I18nString">
  <Property Name="Locale" Nullable="false" Type="Edm.String" />
  <Property Name="Value" Nullable="false" Type="Edm.String" />
</ComplexType>
```

Each metadata resource defines a new field called `DisplayNames`, which has a type of `Collection(I18nString)`: 

```xml
<edmx:Edmx xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx" Version="4.0">
  <edmx:DataServices>
    <Schema xmlns="http://docs.oasis-open.org/odata/ns/edm" Namespace="org.reso.metadata">

      <ComplexType Name="I18nString">
        <Property Name="Locale" Nullable="false" Type="Edm.String" />
        <Property Name="Value" Nullable="false" Type="Edm.String" />
      </ComplexType>

      <EntityType Name="Model">
        <Key>
          <PropertyRef Name="ModelKey"/>
        </Key>
        <Property Name="ModelKey" Type="Edm.String" />
        <Property Name="ModelName" Type="Edm.String" />
        <Property Name="DisplayNames" Type="Collection(org.reso.metadata.I18nString)" />
      </EntityType>

      <EntityType Name="Field">
        <Key>
          <PropertyRef Name="FieldKey"/>
        </Key>
        <Property Name="FieldKey" Type="Edm.String" />
        <Property Name="FieldName" Type="Edm.String" />
        <Property Name="DisplayNames" Type="Collection(org.reso.metadata.I18nString)" />
      </EntityType>

      <EntityType Name="Lookup">
        <Key>
          <PropertyRef Name="LookupKey"/>
        </Key>
        <Property Name="LookupKey" Type="Edm.String" />
        <Property Name="FieldName" Type="Edm.String" />
        <Property Name="DisplayNames" Type="Collection(org.reso.metadata.I18nString)" />
      </EntityType>

      <EntityContainer Name="RESO">
        <EntitySet Name="Model" EntityType="org.reso.metadata.Model" />
        <EntitySet Name="Field" EntityType="org.reso.metadata.Field" />
        <EntitySet Name="Lookup" EntityType="org.reso.metadata.Lookup" />
      </EntityContainer>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>
```

## Section 2.2 Required Data Elements

| ModelName | ModelType | FieldName | Data Type | Definition |
|---|---|---|---|---|
| I18nString | Shape | Locale | String | The ISO 639 and 3166 encoding of the locale (e.g., `en-US`).  |
| I18nString | Shape | Value | String | The string value of the metadata element for the given locale. |
| Model | Shape | `DisplayNames` | `Collection(I18nString)` | Collection of possible ISO 639/3166 locales for the given metadata element. Non-nullable. The absence of values is an empty collection. |
| Field | Resource | `DisplayNames` | `Collection(I18nString)` | Collection of possible ISO 639/3166 locales for the given metadata element. Non-nullable. The absence of values is an empty collection. |
| Lookup | Resource | `DisplayNames` | `Collection(I18nString)` | Collection of possible ISO 639/3166 locales for the given metadata element. Non-nullable. The absence of values is an empty collection. |

**Note**: All data elements are non-nullable. For items with a type of `Collection(I18nString)`, the empty list `[]` is returned when there are no values. 

## Section 2.3 Locale Queries

Each metadata resource supporting the `DisplayNames` collection MUST be searchable using locale codes. Providers MAY rate limit queries if they exceed quotas. 

### Model Resource

Filter models that have locale values of Canadian English (`en-CA`) or French Canadian `fr-CA`. 

```

GET /Model?$filter=DisplayNames/any(item: item/Locale in ('en-CA', 'fr-CA'))

{
  "@odata.context": "/Model?$filter=DisplayNames/any(item: item/Locale in ('en-CA', 'fr-CA'))",
  "@reso.context": "urn:reso:metadata:2.2:resource:model",
  "value": [
    {
      "ModelKey": "m1",
      "ModelName": "Property",
      "ModelType": "Resource",
      "DisplayNames": [
        {
          "Locale": "en-CA",
          "Value": "Property"
        },
        {
          "Locale": "fr-CA",
          "Value": "Propriété"
        }
      ]
    }
  ]
}
```

### Field Resource

Filter fields that have locale values of Canadian English (`en-CA`) or French Canadian `fr-CA`. 

```

GET /Field?$filter=DisplayNames/any(item: item/Locale in ('en-CA', 'fr-CA'))
{
  "@odata.context": "/Field?$filter=DisplayNames/any(item: item/Locale in ('en-CA', 'fr-CA'))",
  "@reso.context": "urn:reso:metadata:2.2:resource:field",
  "value": [
    {
      "FieldKey": "f1",
      "FieldName": "ListPrice",
      "DisplayNames": [
        {
          "Locale": "en-CA",
          "Value": "List Price"
        },
        {
          "Locale": "fr-CA",
          "Value": "Liste des Prix"
        }
      ]
    }
  ]
}

```


### Lookup Resource

Filter lookups that have locale values of Canadian English (`en-CA`) or French Canadian `fr-CA`. 

```
GET /Lookup?$filter=DisplayNames/any(item: item/Locale in ('en-CA', 'fr-CA'))

{
  "@odata.context": "/Lookup?$filter=DisplayNames/any(item: item/Locale in ('en-CA', 'fr-CA'))",
  "@reso.context": "urn:reso:metadata:2.2:resource:lookup",
  "value": [
    {
      "LookupKey": "l1",
      "LookupName": "StandardStatus",
      "LookupValue": "Pending",
      "DisplayNames": [
        {
          "Locale": "en-CA",
          "Value": "Pending"
        },
        {
          "Locale": "fr-CA",
          "Value": "En Attente"
        }
      ]
    }
  ]
}
```

<br />

# Section 3: Certification
RESO will validate the following during certification:
* Metadata definition of `I18nString` and `DisplayNames` collection match the specification and are non-nullable. 
* Model, Field, and Lookup resources are queryable by locale using `any` and `all` operators, [**outlined in section 2.3**](#section-23-locale-queries).
* Replicate all metadata and validate that all `DisplayNames` values follow ISO 639/3166 standards. 
* ISO 639 and 3166 both support local extension so that a local dialect can be encoded for a geographic region like a state. See ISO standards for more information. Format validation includes local extension. 

<br />

# Section 4. Contributors
Thanks to Paul Stusiak and others in the Transport Workgroup for their contributions to this proposal. 

<br />

# Section 5: References

Please see the following references for more information regarding topics covered in this document:
* [**ISO 639 Language Codes**](https://en.wikipedia.org/wiki/List_of_ISO_639_language_codes)
* [**ISO 3166 Country Codes**](https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes)
* [**Uniform Resource Name (URN)**](https://datatracker.ietf.org/doc/html/rfc8141)
* [**RESO URN Assignment**](https://www.iana.org/assignments/urn-formal/reso)


<br />

# Section 6: Appendices

<br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO](mailto:info@reso.org) if you have any questions.
