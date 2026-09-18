# RESO Metadata Internationalization and Locale Endorsement

| **RCP** | 44 |
| :--- | :--- |
| **Version** | **1.0.0** |
| **Authors** | [Josh Darnell (RESO)](mailto:josh@reso.org) |
| **Status** | IN PROGRESS |
| **Date Ratified** | TBD |
| **Dependencies** | [Data Dictionary 2.2+](https://dd.reso.org/) (the Model, Field and Lookup Resources, with Shape support for the I18nString complex type)<br />[Web API Core 2.1.0+](https://transport.reso.org/proposals/web-api-core/) (the `any` and `all` lambda operators; the `in` operator when OData 4.01 is advertised) |
| **Related Links** | [RCP-42 Model and Field Resources](https://github.com/RESOStandards/transport/blob/b21579a147a476a95fb0a9457ab6b23d5b7afc0a/proposals/model-and-field-resources.md)<br />[RCP-45 Legacy and Deprecated Data Elements](https://github.com/RESOStandards/transport/pull/104)<br />[RCP-47 RelatedLookup Resource](https://github.com/RESOStandards/transport/blob/015bd6181a3a9f56b1c5b767685761a842e0284c/proposals/related-lookups.md) |

<br />

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

The keywords "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be interpreted as described in [RFC 2119](https://www.ietf.org/rfc/rfc2119.txt).

<br />

# Table of Contents
- [Summary of Changes](#summary-of-changes)
- [Introduction](#introduction)
- [Section 1: Purpose](#section-1-purpose)
- [Section 2: Specification](#section-2-specification)
  - [Section 2.1: The I18nString Shape](#section-21-the-i18nstring-shape)
  - [Section 2.2: Locale Identifiers](#section-22-locale-identifiers)
  - [Section 2.3: The DisplayNames Field](#section-23-the-displaynames-field)
  - [Section 2.4: Locale Queries](#section-24-locale-queries)
- [Section 3: Certification](#section-3-certification)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: Appendices](#section-6-appendices)
- [Section 7: License](#section-7-license)

<br />

# Summary of Changes
* Adds locale-specific display names for the metadata elements a provider serves – models, fields and lookup values – using standard locale identifiers (BCP 47 language tags, typically an ISO 639 language code followed by an ISO 3166-1 country code) that consumers can search.
* Adds a new shape (an OData complex type), **I18nString**, that pairs a `Locale` with a `Value`.
* Adds a new field, **DisplayNames**, a collection of I18nString, to the Model, Field and Lookup Resources, and requires each of those resources to be searchable by locale.
* All changes are additive: no existing element is renamed, removed or retyped. The deprecation of the Data Dictionary 1.7 Field Resource's single-locale `DisplayName` in favor of `DisplayNames` is carried by RCP-42.

<br />

# Introduction

The RESO Data Dictionary names its resources, fields and lookup values in U.S. English. Providers and consumers in other markets, and those serving users who work in other languages, need to show those elements under the names their users know, without changing the standard names and values that make the data interoperable.

The Data Dictionary already recognizes this need: the Data Dictionary 2.1 spreadsheet carries display names in Canadian French and Spanish for many of its fields and lookup values as fixed columns, and the Data Dictionary 2.0 specification says that display names for fields and lookups should be conveyed through the Field and Lookup Resources. This endorsement gives those translations, and any provider's own, a standard and searchable home in the metadata resources: a collection of localized display names on each model, field and lookup value, keyed by a standard locale identifier.

<br />

# Section 1: Purpose

Supporting regional and international metadata allows systems to display localized names for RESO Data Dictionary elements, which are based on U.S. English, while preserving the interoperability of standard data elements.

This endorsement adds a field to each of the Model, Field and Lookup Resources that carries a collection of locale and value pairs that can be communicated and searched. Locales are identified with the standard language tags that are common in internationalization (i18n) applications, which makes localization (l10n) of RESO metadata simpler for everyone who consumes it.

A collection of `DisplayNames` is used to support filtering with the `any` and `all` lambda operators in OData. A map keyed by locale was considered and rejected, since the collection provides a simpler and more dynamic interface for end users: a client requests metadata display names in the locales of its choosing without first inspecting the metadata to learn which locales are supported. When a requested locale is not supported, no value for that locale is returned.

<br />

# Section 2: Specification

## Section 2.1: The I18nString Shape

This endorsement defines a new shape, **I18nString**. A shape is a model whose `ModelType` is `Shape` in the Model Resource: a custom data type that appears as a field within a resource but is not available at the top level or as an expansion. [RCP-42 Model and Field Resources](https://github.com/RESOStandards/transport/blob/b21579a147a476a95fb0a9457ab6b23d5b7afc0a/proposals/model-and-field-resources.md) defines how a shape is carried: in OData it is a `ComplexType`, a shape-valued field is that complex type or a `Collection(...)` of it, it is returned inline without `$expand`, and a collection of shapes is never null. This endorsement follows that definition.

The shape is a Model record and its two fields are Field records of that model:

| Model | Model Type | Definition |
| :--- | :--- | :--- |
| I18nString | Shape | A display name for a metadata element in a single locale, pairing the locale with the text. |

| Field | Type | Nullable | Definition |
| :--- | :--- | :--- | :--- |
| Locale | String | No | The locale of the display name, as a BCP 47 language tag under RFC 5646, typically an ISO 639 language code followed by an ISO 3166-1 alpha-2 country code (e.g., en-US, fr-CA). |
| Value | String | No | The display name of the metadata element in the given locale. |

The rules for a `Locale` value are in [Section 2.2](#section-22-locale-identifiers). In OData the shape is a complex type:

```xml
<ComplexType Name="I18nString">
  <Property Name="Locale" Type="Edm.String" Nullable="false" />
  <Property Name="Value" Type="Edm.String" Nullable="false" />
</ComplexType>
```

## Section 2.2: Locale Identifiers

A `Locale` value is a language tag as defined by BCP 47 ([RFC 5646](https://datatracker.ietf.org/doc/html/rfc5646)): a primary language subtag from ISO 639, optionally followed by a region subtag, an ISO 3166-1 alpha-2 country code or a UN M.49 area code, for example `en-US`, `en-CA`, `fr-CA` or `es`. A tag MAY carry the other subtags BCP 47 defines, such as script, variant, extension and private-use subtags; a locale narrower than a country, such as a state or province, is expressed with those subtags rather than with a region subtag. ISO 3166-2 subdivision codes (for example `US-CA`) are not BCP 47 region subtags and do not appear in that position.

* A `Locale` value MUST be a well-formed language tag under the syntax in RFC 5646, Section 2.1.
* A `Locale` value MUST use the case format recommended in RFC 5646, Section 2.1.1: the language subtag in lowercase, a script subtag in title case and a region subtag in uppercase (`fr-CA`, not `fr-ca`). String comparison in the RESO Web API is case-sensitive, so a consistent case is what makes a locale filter behave the same way across providers.
* A `Locale` value SHOULD be valid, that is, composed of subtags registered in the [IANA Language Subtag Registry](https://www.iana.org/assignments/language-subtag-registry).

A consumer resolves a `Locale` value against the IANA Language Subtag Registry, which is the registry RFC 5646 defines for the subtags a tag is built from.

## Section 2.3: The DisplayNames Field

The following field is added to each of the [Model Resource](https://dd.reso.org/DD2.1/Model/), the [Field Resource](https://dd.reso.org/DD2.1/Field/) and the [Lookup Resource](https://dd.reso.org/DD2.1/Lookup/):

| Resource | Field | Type | Nullable | Definition |
| :--- | :--- | :--- | :--- | :--- |
| Model | DisplayNames | Collection (I18nString) | No | A collection of localized display names for the model, one per locale. |
| Field | DisplayNames | Collection (I18nString) | No | A collection of localized display names for the field, one per locale. |
| Lookup | DisplayNames | Collection (I18nString) | No | A collection of localized display names for the lookup value, one per locale. |

When the element has no localized display names, the value is the empty collection `[]`, never null.

Within one `DisplayNames` collection, each `Locale` MUST appear at most once, so that a consumer holding a locale finds at most one display name for it.

`DisplayNames` is an OData structural property, as RCP-42 defines for a shape-valued field: it is returned in the payload without `$expand`, and it MAY be named in `$select`. It carries display text only and changes nothing about the values used in payloads and queries: `ModelName`, `FieldName` and `LookupValue` remain the names and values a consumer sends and receives.

The Data Dictionary 1.7 Field Resource carries a single-locale `DisplayName`, which RCP-42 deprecates in favor of `DisplayNames`; the Data Dictionary 2.0 and 2.1 reference metadata do not define `DisplayName` on the Field Resource. The Lookup Resource's `LookupValue`, the human-friendly value used in payloads and queries, is unchanged.

In OData metadata the three resources are defined as follows (other properties omitted):

```xml
<edmx:Edmx xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx" Version="4.0">
  <edmx:DataServices>
    <Schema xmlns="http://docs.oasis-open.org/odata/ns/edm" Namespace="org.reso.metadata">

      <ComplexType Name="I18nString">
        <Property Name="Locale" Type="Edm.String" Nullable="false" />
        <Property Name="Value" Type="Edm.String" Nullable="false" />
      </ComplexType>

      <EntityType Name="Model">
        <Key>
          <PropertyRef Name="ModelKey" />
        </Key>
        <Property Name="ModelKey" Type="Edm.String" />
        <Property Name="ModelName" Type="Edm.String" />
        <!-- other properties omitted -->
        <Property Name="DisplayNames" Type="Collection(org.reso.metadata.I18nString)" Nullable="false" />
      </EntityType>

      <EntityType Name="Field">
        <Key>
          <PropertyRef Name="FieldKey" />
        </Key>
        <Property Name="FieldKey" Type="Edm.String" />
        <Property Name="FieldName" Type="Edm.String" />
        <!-- other properties omitted -->
        <Property Name="DisplayNames" Type="Collection(org.reso.metadata.I18nString)" Nullable="false" />
      </EntityType>

      <EntityType Name="Lookup">
        <Key>
          <PropertyRef Name="LookupKey" />
        </Key>
        <Property Name="LookupKey" Type="Edm.String" />
        <Property Name="LookupName" Type="Edm.String" />
        <Property Name="LookupValue" Type="Edm.String" />
        <!-- other properties omitted -->
        <Property Name="DisplayNames" Type="Collection(org.reso.metadata.I18nString)" Nullable="false" />
      </EntityType>

      <EntityContainer Name="Default">
        <EntitySet Name="Model" EntityType="org.reso.metadata.Model" />
        <EntitySet Name="Field" EntityType="org.reso.metadata.Field" />
        <EntitySet Name="Lookup" EntityType="org.reso.metadata.Lookup" />
      </EntityContainer>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>
```

## Section 2.4: Locale Queries

Each resource that carries `DisplayNames` MUST support filtering by locale with the `any` and `all` lambda operators over the collection, with the semantics OData defines for the lambda operators, as quoted in the Web API Core 2.1.0 specification, Section 2.5.9.9.2: `any` matches a record that carries at least one display name satisfying the expression, and `all` matches a record whose every display name satisfies it, which a record with an empty collection satisfies vacuously. Web API Core defines and tests these operators over enumeration collections; this endorsement applies them to a collection of the `I18nString` complex type. A server that advertises OData 4.01 MUST also support the `in` operator inside the lambda expression.

The examples below filter for elements that carry a display name in Canadian English (`en-CA`) or Canadian French (`fr-CA`). Requests and responses are shown against `https://api.example.com`; keys are illustrative. The `in` form of each request, for servers advertising OData 4.01, is `DisplayNames/any(item:item/Locale in ('en-CA', 'fr-CA'))`. Responses are shown with the `@reso.context` annotation that the RESO Common Format endorsement (RCP-25) defines; Web API Core does not require it.

### Model Resource

**REQUEST**
```
GET https://api.example.com/Model?$filter=DisplayNames/any(item:item/Locale eq 'en-CA' or item/Locale eq 'fr-CA')
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "https://api.example.com/$metadata#Model",
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

**REQUEST**
```
GET https://api.example.com/Field?$filter=DisplayNames/any(item:item/Locale eq 'en-CA' or item/Locale eq 'fr-CA')
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "https://api.example.com/$metadata#Field",
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
          "Value": "Prix de l’inscription"
        }
      ]
    }
  ]
}
```

### Lookup Resource

**REQUEST**
```
GET https://api.example.com/Lookup?$filter=DisplayNames/any(item:item/Locale eq 'en-CA' or item/Locale eq 'fr-CA')
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "https://api.example.com/$metadata#Lookup",
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
          "Value": "En attente"
        }
      ]
    }
  ]
}
```

<br />

# Section 3: Certification

RESO will validate the following during certification:

**Metadata**
* The server metadata MUST define a complex type named `I18nString` with the properties `Locale` and `Value`, each of type `Edm.String` and non-nullable. Additional local properties MAY be present.
* The `Model`, `Field` and `Lookup` entity types MUST each define `DisplayNames` as a collection of the `I18nString` complex type (`Collection(org.reso.metadata.I18nString)` in the reference namespace), non-nullable.

**Data**, validated against records replicated from the Model, Field and Lookup Resources:
* Every record MUST carry `DisplayNames` as a JSON array; a record with no localized display names MUST carry the empty collection `[]`, and the value MUST NOT be null.
* Every item in a `DisplayNames` collection MUST carry a non-null `Locale` and a non-null `Value`.
* Every `Locale` value MUST be a well-formed language tag under RFC 5646, Section 2.1, in the case format of RFC 5646, Section 2.1.1 ([Section 2.2](#section-22-locale-identifiers)).
* A `Locale` value MUST NOT appear more than once within one record's `DisplayNames` collection ([Section 2.3](#section-23-the-displaynames-field)).

**Queries**, run against each of the three resources with locales sampled from the replicated data:
* `$filter=DisplayNames/any(item:item/Locale eq '<locale>')` MUST return only records that carry a display name in that locale, and MUST return every such record found in the replicated data.
* `$filter=DisplayNames/any(item:item/Locale eq '<locale1>' or item/Locale eq '<locale2>')` MUST return only records that carry a display name in at least one of the two locales.
* `$filter=DisplayNames/all(item:item/Locale eq '<locale>')` MUST return only records whose every display name is in that locale; a record whose `DisplayNames` collection is empty satisfies this condition and is returned.
* When the server advertises OData 4.01, `$filter=DisplayNames/any(item:item/Locale in ('<locale1>', '<locale2>'))` MUST return the same records as the `or` form.

<br />

# Section 4: Contributors

This document was written by [Joshua Darnell](mailto:josh@reso.org).

Thanks to Paul Stusiak and others in the RESO Transport Workgroup for their contributions to this proposal.

| Contributor | Company |
| --- | --- |
| Paul Stusiak | Falcon Technologies |

<br />

# Section 5: References

Please see the following references for more information regarding topics covered in this document:
* [RFC 5646 – Tags for Identifying Languages (BCP 47)](https://datatracker.ietf.org/doc/html/rfc5646)
* [IANA Language Subtag Registry](https://www.iana.org/assignments/language-subtag-registry)
* [ISO 639 Language Codes](https://en.wikipedia.org/wiki/List_of_ISO_639_language_codes)
* [ISO 3166 Country Codes](https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes)
* [RESO Data Dictionary 2.1 – Model Resource](https://dd.reso.org/DD2.1/Model/)
* [RESO Data Dictionary 2.1 – Field Resource](https://dd.reso.org/DD2.1/Field/)
* [RESO Data Dictionary 2.1 – Lookup Resource](https://dd.reso.org/DD2.1/Lookup/)
* [RESO Web API Core 2.1.0](https://transport.reso.org/proposals/web-api-core/)
* [RESO Common Format (RCP-25)](https://transport.reso.org/proposals/reso-common-format/)
* [Uniform Resource Name (URN), RFC 8141](https://datatracker.ietf.org/doc/html/rfc8141)
* [RESO URN Assignment](https://www.iana.org/assignments/urn-formal/reso)
* [RCP-42 Model and Field Resources](https://github.com/RESOStandards/transport/blob/b21579a147a476a95fb0a9457ab6b23d5b7afc0a/proposals/model-and-field-resources.md)
* [RCP-45 Legacy and Deprecated Data Elements](https://github.com/RESOStandards/transport/pull/104)

<br />

# Section 6: Appendices

## Proposed Data Dictionary elements

The elements this endorsement proposes are defined in [Section 2.1](#section-21-the-i18nstring-shape) (the I18nString shape and its `Locale` and `Value` fields) and [Section 2.3](#section-23-the-displaynames-field) (`DisplayNames` on the Model, Field and Lookup Resources). No lookup is proposed: `Locale` carries an open, externally governed set of values and is therefore a String rather than an enumeration. Nothing is deprecated; the deprecation of the Data Dictionary 1.7 Field Resource's `DisplayName` in favor of `DisplayNames` is carried by RCP-42.

<br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO Transport](mailto:transport@reso.org) with questions about this proposal, or [RESO developer support](mailto:dev@reso.org) with specific development questions.
