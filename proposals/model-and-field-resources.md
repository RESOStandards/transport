# RESO Model and Field Resources Endorsement

| **RCP** | 42 |
| :--- | :--- |
| **Version** | **1.0.0** |
| **Authors** | [Joshua Darnell (RESO)](mailto:josh@reso.org) |
| **Status** | IN PROGRESS |
| **Date Ratified** | TBD |
| **Dependencies** | [Data Dictionary 2.2+](https://dd.reso.org/) (Model, Field, and Lookup Resources) |
| **Related Links** | [Web API Core 2.1.0](https://transport.reso.org/proposals/web-api-core/)<br />[RESO Common Format](https://transport.reso.org/proposals/reso-common-format/)<br />[RCP-44 Metadata Internationalization and Locale](https://github.com/RESOStandards/transport/blob/fb879c1e60d517e7f139d5face97262055ab7242/proposals/metadata-locales.md)<br />[RCP-45 Legacy and Deprecated Data Elements](https://github.com/RESOStandards/transport/blob/ddb8bc9792173ef1a1d57eb5cb560af21d0fc558/proposals/rcp-45-legacy-deprecated-fields-lookups.md)<br />[RCP-35 Supporting Single Feeds](https://github.com/RESOStandards/transport/blob/e222c800f2e6ef091fe6c334ce5160456a4e5c56/proposals/rcp-035-single-feeds.md)<br />[RCP-48 Add/Edit with Media](https://github.com/RESOStandards/transport/blob/fc35c711487bcd59ffecd02780feb56269fabd73/proposals/web-api-add-edit-with-media.md) |

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
  - [Section 2.1: Field Resource](#section-21-field-resource)
  - [Section 2.2: Model Resource](#section-22-model-resource)
  - [Section 2.3: Relationship to the OData XML Metadata](#section-23-relationship-to-the-odata-xml-metadata)
  - [Section 2.4: Examples](#section-24-examples)
- [Section 3: Certification](#section-3-certification)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: Appendices](#section-6-appendices)
- [Section 7: License](#section-7-license)

<br />

# Summary of Changes
* Adds the **Model Resource**, which describes each model a provider serves – a resource or a shape – with its definition, its primary key and modification timestamp fields, and whether it can be read, inserted, updated or deleted.
* Extends the **Field Resource** with each field's data type, lookup name, length, precision and scale, key membership, collection and expansion flags, nullability, and whether it can be read, ordered, updated or searched, so a server can advertise capabilities such as searchability and updatability in a standard, transport-agnostic format.
* Deprecates `ResourceName` on the Field Resource in favor of `ModelKey`, and `DisplayName` in favor of the `DisplayNames` collection defined in RCP-44. Providers MAY continue to serve the deprecated fields, but the replacements MUST also be present.
* Defines how the Model, Field and Lookup Resources relate to a server's OData XML metadata, and the certification rules that follow.
* Version impact: this is version 1.0.0 of a new endorsement. Its Data Dictionary changes are additive – a new resource and new fields on an existing resource – plus the two deprecations above, which remain usable for backward compatibility.

<br />

# Introduction

Server metadata describes the resources, fields and lookups a provider serves. To make that metadata richer, easier to work with, and more Data Dictionary friendly and transport agnostic, this endorsement adds a Model Resource and extends the Field Resource.

The term "resource" is used the same way the Data Dictionary uses it today. Because a provider can also serve other kinds of models, such as custom data types that appear only inside a resource, the terminology has been generalized: a **model** is either a resource or a shape.

The endorsement works with the [Web API Core](https://transport.reso.org/proposals/web-api-core/) standards as well as the [RESO Common Format](https://transport.reso.org/proposals/reso-common-format/). In the latter case a non-OData server can advertise its metadata in a standard format using the Model, Field and Lookup Resources.

The Model Resource and the Field Resource fields defined here ship in Data Dictionary 2.2; the Data Dictionary 2.1 reference sheets already carry them in draft form. This document is the Transport specification for them: what each field means, how the resources relate to one another and to the OData XML metadata, and what RESO validates.

<br />

# Section 1: Purpose

Today a consumer learns what a RESO Web API server offers by reading its OData XML metadata document. That document is specific to OData, so a provider using another transport has no standard way to publish the same information, and it describes names and types but not capabilities: a consumer cannot tell from it which fields can be searched, ordered or updated, or which resources accept inserts and deletes. The Field Resource defined in Data Dictionary 1.7 and 2.0 carries a field's key, name, resource, modification timestamp and, in 1.7, display name, but none of these attributes, and there is no resource that describes a resource itself.

This endorsement addresses these gaps by:

* adding a **Model Resource** that describes each resource and shape a provider serves, including its definition, key and modification timestamp fields, and its read, insert, update and delete capabilities;
* extending the **Field Resource** so each field carries its data type, lookup name, length, precision and scale, collection and expansion flags, nullability, and its read, order, update and search capabilities;
* defining the relationship between these resources and the OData XML metadata, so the two stay consistent on servers that provide both.

<br />

# Section 2: Specification

This specification adds the Model Resource and extends the existing [Field Resource](https://dd.reso.org/DD2.1/Field/), as currently defined in Data Dictionary 1.7 and 2.0. The [Lookup Resource](https://transport.reso.org/proposals/data-dictionary/#section-22-lookup-resource-for-enumeration-metadata), introduced in Data Dictionary 1.7 and defined in the Data Dictionary specification, Section 2.2, is referenced but not changed.

## Section 2.1: Field Resource

### Deprecated fields

The following fields of the Field Resource are **deprecated**:

* **ResourceName** – deprecated in favor of **ModelKey**, defined below.
* **DisplayName** – deprecated in favor of the **DisplayNames** collection, defined in [RCP-44 Metadata Internationalization and Locale](https://github.com/RESOStandards/transport/blob/fb879c1e60d517e7f139d5face97262055ab7242/proposals/metadata-locales.md).

Providers MAY continue to serve these fields for backward compatibility. A Field record that carries `ResourceName` MUST also carry `ModelKey`, and a Field record that carries `DisplayName` MUST also carry `DisplayNames`. Deprecated fields are treated as local fields in RESO Analytics.

### Added fields

The following fields are added to the Field Resource. Types are Data Dictionary simple data types; the OData type follows the [Data Dictionary data type mappings](https://transport.reso.org/proposals/data-dictionary/#data-type-mappings). Maximum lengths are suggested.

| Field | Type | Nullable | Max length | Definition |
| :--- | :--- | :--- | :--- | :--- |
| ModelKey | String | No | 50 | The ModelKey value from the Model Resource identifying the model the field belongs to. |
| LookupName | String | Yes | 100 | The LookupName in the Lookup Resource whose values the field uses; null when the field is not enumerated. |
| Type | String List, Single | No | 100 | The transport-specific data type of the field, from the FieldDataTypes lookup. |
| PrimaryKeyYN | Boolean | Yes | | Indicates whether or not the field is a member of its model's primary key. |
| CollectionYN | Boolean | Yes | | Indicates whether or not the field is a collection. |
| ExpandableYN | Boolean | Yes | | Indicates whether or not the field can be expanded into the records of another model. |
| NullableYN | Boolean | Yes | | Indicates whether or not the field accepts a null value. |
| Length | Number | Yes | | The maximum length of the field's value, in characters, when the data type has a length. |
| Precision | Number | Yes | | The total number of significant digits stored, both to the left and right of the decimal point, when the data type has a precision. |
| Scale | Number | Yes | | The number of digits to the right of the decimal point, when the data type has a scale. |
| ReadableYN | Boolean | Yes | | Indicates whether or not the field can be read by the current client. |
| OrderableYN | Boolean | Yes | | Indicates whether or not results can be ordered by the field. |
| UpdatableYN | Boolean | Yes | | Indicates whether or not the field can be updated by the current client. |
| SearchableYN | Boolean | Yes | | Indicates whether or not the field can be used in a query filter. |
| Definition | String | Yes | 200 | The human-friendly definition of the field. |

**LookupName.** Where the Data Dictionary defines the lookup, the name MUST match the Data Dictionary lookup name, for example `StandardStatus`; providers MAY define their own lookup names otherwise and MAY reuse one lookup across more than one field. For OData servers this is the value of the `RESO.OData.Metadata.LookupName` annotation on the field.

**Type.** The **FieldDataTypes** lookup is open with enumerations, so a provider MAY carry other values. For OData servers the value MUST match the type declared in the metadata, including its namespace when the type is not a primitive, for example `org.reso.metadata.enums.StandardStatus` for an enumeration or `org.reso.metadata.Media` for an expansion. For a collection, `Type` carries the element type and `CollectionYN` is `true`. For RESO Common Format the Data Dictionary types are referenced. The standard values are:

| Lookup Value | Definition |
| :--- | :--- |
| Edm.Boolean | The OData Edm.Boolean data type representing a Boolean true or false field. |
| Edm.Date | The OData Edm.Date data type representing an ISO 8601 date in YYYY-MM-DD format. Example: "2024-08-13" |
| Edm.DateTimeOffset | The OData Edm.DateTimeOffset data type representing an ISO 8601 date in YYYY-MM-DDTHH:MM:SS.F[Z\|+/-Offset] format. Example: "2024-08-13T21:55:08.002Z" |
| Edm.Decimal | The OData Edm.Decimal data type representing a floating-point decimal number. |
| Edm.Double | The OData Edm.Double data type representing a double precision floating point number. |
| Edm.Int16 | The OData Edm.Int16 data type representing a 16-bit signed integer. |
| Edm.Int32 | The OData Edm.Int32 data type representing a 32-bit signed integer. |
| Edm.Int64 | The OData Edm.Int64 data type representing a 64-bit signed integer. |
| Edm.String | The OData Edm.String data type representing a sequence of characters. |

**PrimaryKeyYN.** A model's key is the set of its Field records whose `PrimaryKeyYN` is `true`; a key of one field is also named by the model's `PrimaryKeyFieldKey`.

**CollectionYN, ExpandableYN and NullableYN.** The `Media` expansion in the Property Resource is an example of a field that is both a collection and expandable. A field whose `CollectionYN` is `true` is not nullable: its empty value is the empty list, `[]`. `ListPrice` in the Property Resource is an example of a nullable field.

**Length, Precision and Scale.** These are integers: under the Data Dictionary data type mappings a Number without precision and scale is an integer, carried in OData as `Edm.Int16`, `Edm.Int32` or `Edm.Int64`. For OData servers they are the `MaxLength`, `Precision` and `Scale` facets.

**OrderableYN and SearchableYN.** For OData servers these correspond to the `$orderby` and `$filter` query options.

The Data Dictionary 2.1 Field Resource also carries `FeedTypes`, which is defined in [RCP-35 Supporting Single Feeds](https://github.com/RESOStandards/transport/blob/e222c800f2e6ef091fe6c334ce5160456a4e5c56/proposals/rcp-035-single-feeds.md), not by this endorsement.

### Rules

* Every model of type `Resource` MUST have at least one Field record whose `PrimaryKeyYN` is `true`.
* Every field that is a member of its model's key MUST have `SearchableYN` = `true` and `NullableYN` = `false`; OData requires a key property to be non-nullable ([OData CSDL 4.01, Section 8](https://docs.oasis-open.org/odata/odata-csdl-xml/v4.01/os/odata-csdl-xml-v4.01-os.html)).
* Every `ModificationTimestamp` field MUST have `SearchableYN` = `true`, `OrderableYN` = `true` and `UpdatableYN` = `false`.
* A field whose `CollectionYN` is `true` MUST have `NullableYN` = `false`.
* A null value in any Boolean field of the Model or Field Resource means the provider has not stated it, as null means in OData ([OData 4.01 Part 2: URL Conventions, Section 5.1.1.1](https://docs.oasis-open.org/odata/odata/v4.01/os/part2-url-conventions/odata-v4.01-os-part2-url-conventions.html), where the null value is treated as unknown); it MUST NOT be read as `false`.

## Section 2.2: Model Resource

A new resource called **Model** describes each model a provider serves. It defines the following fields:

| Field | Type | Nullable | Max length | Definition |
| :--- | :--- | :--- | :--- | :--- |
| ModelKey | String | No | 50 | The key used to uniquely identify the model. |
| ModelName | String | No | 50 | The name of the model as expressed in the transport (e.g., Property). |
| ModelType | String List, Single | No | | The type of the model (i.e., Resource or Shape). |
| Definition | String | Yes | 200 | The human-friendly definition of the model. |
| PrimaryKeyFieldKey | String | Yes | 50 | The FieldKey value from the Field Resource identifying the model's primary key field when the key is a single field; null when the key is compound. |
| ModificationTimestampFieldKey | String | Yes | 50 | The FieldKey value from the Field Resource identifying the model's modification timestamp field; null when the model has no such field. |
| ReadableYN | Boolean | Yes | | Indicates whether or not the model can be read by the current client. |
| InsertableYN | Boolean | Yes | | Indicates whether or not records can be inserted into the model. |
| UpdatableYN | Boolean | Yes | | Indicates whether or not records in the model can be updated. |
| DeletableYN | Boolean | Yes | | Indicates whether or not records in the model can be deleted. |
| ModificationTimestamp | Timestamp | No | | The timestamp when the model metadata item was last modified, in ISO 8601 format. |

**ModelType.** The **ModelType** lookup is closed. A **Resource** means the same as it always has in the Data Dictionary: a model that can be served at the top level or as an expansion, containing fields and lookups. A **Shape** is a custom data type that appears as a collection or non-collection field within a resource; it is not available at the top level or as an expansion for those using the RESO Web API. In OData a shape is a `ComplexType`, and a shape-valued field is that complex type or a `Collection(...)` of it: it is returned inline, without `$expand`, and a collection of shapes is never null (its empty value is `[]`). Other endorsements that define a shape, for example RCP-44 and RCP-35, follow this definition.

| Lookup Value | Definition |
| :--- | :--- |
| Resource | A model that can be served at the top level of the transport or as an expansion and contains fields (e.g., Property). |
| Shape | A custom data type that appears as a field or collection within a resource and is not available at the top level or as an expansion. |

**PrimaryKeyFieldKey.** The members of a model's key, single or compound, are the Field records whose `PrimaryKeyYN` is `true`, matching OData, where an entity type's `Key` lists one or more `PropertyRef` elements.

**ModificationTimestampFieldKey.** Not every resource has a `ModificationTimestamp` field, for example `EntityEvent`. For local resources the timestamp field can be something other than `ModificationTimestamp`.

**ReadableYN, UpdatableYN and DeletableYN.** A model is readable when at least one of its fields is readable, and updatable when at least one of its fields is updatable. A model that is not readable is still advertised in the metadata, but a request for its records returns a non-2XX status code. Not all providers support delete operations, and in some cases a delete changes the record to a non-visible status rather than removing it from the underlying system.

The Data Dictionary 2.1 Model Resource also carries `HasStreamYN`, defined in [RCP-48 Add/Edit with Media](https://github.com/RESOStandards/transport/blob/fc35c711487bcd59ffecd02780feb56269fabd73/proposals/web-api-add-edit-with-media.md); it is not defined by this endorsement.

### Rules

* `PrimaryKeyFieldKey`, when present, MUST reference an existing Field record whose `ModelKey` is the model's `ModelKey` and whose `PrimaryKeyYN` is `true`, and MUST be present when exactly one Field record of the model has `PrimaryKeyYN` = `true`.
* A null value in any Boolean field of the Model or Field Resource means the provider has not stated it, as null means in OData ([OData 4.01 Part 2: URL Conventions, Section 5.1.1.1](https://docs.oasis-open.org/odata/odata/v4.01/os/part2-url-conventions/odata-v4.01-os-part2-url-conventions.html), where the null value is treated as unknown); it MUST NOT be read as `false`.
* `ModificationTimestampFieldKey`, when present, MUST reference an existing Field record whose `ModelKey` is the model's `ModelKey`.
* Every model of type `Resource` MUST have at least one field, standard or local, in the Field Resource. This follows from the key requirement in [Section 2.3](#section-23-relationship-to-the-odata-xml-metadata).

## Section 2.3: Relationship to the OData XML Metadata

RESO Web API servers describe their models in an OData XML metadata document ([Web API Core 2.1.0, Section 2.5.1](https://transport.reso.org/proposals/web-api-core/)). The following rules keep the Model and Field Resources consistent with that document.

* A server MAY support the Lookup Resource without the Model and Field Resources. A server that supports the Model Resource or the Field Resource MUST support all three: Model, Field and Lookup.
* A server that does not support the Model, Field or Lookup Resource SHOULD respond with a non-2XX HTTP status code when a client requests it. This gives the client a way to know whether to read the OData XML metadata instead.
* A server using the Model and Field Resources MUST still provide the minimum OData XML metadata Web API Core requires, that is, `EntityType` and `Property` definitions. Every attribute that can be expressed in both places, such as type, length, precision, scale and nullability, MUST have the same value in the Field Resource and in the OData XML metadata.
* For every Model record whose `ModelType` is `Resource`, the OData XML metadata MUST define an entity set of that name in its entity container, backed by an `EntityType`. Conversely, for every entity set in the entity container, a server using the Model Resource MUST have a Model record whose `ModelType` is `Resource`.
* Every `EntityType` MUST define a key ([Web API Core 2.1.0, Section 2.5.1](https://transport.reso.org/proposals/web-api-core/)). The `PropertyRef` elements of the `Key` MUST be exactly the Field records of the model whose `PrimaryKeyYN` is `true`, and `PrimaryKeyFieldKey`, when present, MUST name the single member.
* Every model of type `Resource` MUST have at least one field, standard or local, meaning there are Field records and corresponding `Property` definitions in the OData XML metadata. This follows from the key requirement.
* Standard fields MUST use the names and data types defined in the Data Dictionary, as required by the [Data Dictionary specification](https://transport.reso.org/proposals/data-dictionary/#section-3-certification).
* For local fields, the Field Resource definition and the OData XML metadata MUST agree. The [Data Dictionary data type mappings](https://transport.reso.org/proposals/data-dictionary/#data-type-mappings) apply. These mappings also map to JSON types and are part of the RESO Common Format.

## Section 2.4: Examples

Requests and responses are shown against `https://api.example.com`. Keys, timestamps and definitions are illustrative. The Property fields shown follow their Data Dictionary 2.1 definitions; the timestamp's Precision of 27 follows the Web API Core 2.1.0 metadata example (Section 2.5.1), where the Data Dictionary records a suggested maximum length of 27.

### Example 1: OData XML metadata for the Model and Field Resources

An illustrative definition of the two resources, using `Edm.Int32` for the integer fields and `Edm.String` lookups for `ModelType` and `Type`. The Lookup entity type, defined in the Data Dictionary specification, Section 2.2, is omitted.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<edmx:Edmx Version="4.0" xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx">
  <edmx:DataServices>
    <Schema Namespace="org.reso.metadata" xmlns="http://docs.oasis-open.org/odata/ns/edm">

      <EntityType Name="Model">
        <Key>
          <PropertyRef Name="ModelKey"/>
        </Key>
        <Property Name="ModelKey" Type="Edm.String" MaxLength="50" Nullable="false"/>
        <Property Name="ModelName" Type="Edm.String" MaxLength="50" Nullable="false"/>
        <Property Name="ModelType" Type="Edm.String" Nullable="false">
          <Annotation Term="RESO.OData.Metadata.LookupName" String="ModelType"/>
        </Property>
        <Property Name="Definition" Type="Edm.String" MaxLength="200"/>
        <Property Name="PrimaryKeyFieldKey" Type="Edm.String" MaxLength="50"/>
        <Property Name="ModificationTimestampFieldKey" Type="Edm.String" MaxLength="50"/>
        <Property Name="ReadableYN" Type="Edm.Boolean"/>
        <Property Name="InsertableYN" Type="Edm.Boolean"/>
        <Property Name="UpdatableYN" Type="Edm.Boolean"/>
        <Property Name="DeletableYN" Type="Edm.Boolean"/>
        <Property Name="ModificationTimestamp" Type="Edm.DateTimeOffset" Precision="27" Nullable="false"/>
      </EntityType>

      <EntityType Name="Field">
        <Key>
          <PropertyRef Name="FieldKey"/>
        </Key>
        <Property Name="FieldKey" Type="Edm.String" MaxLength="255" Nullable="false"/>
        <Property Name="FieldName" Type="Edm.String" MaxLength="255" Nullable="false"/>
        <Property Name="ModelKey" Type="Edm.String" MaxLength="50" Nullable="false"/>
        <Property Name="LookupName" Type="Edm.String" MaxLength="100"/>
        <Property Name="Type" Type="Edm.String" MaxLength="100" Nullable="false">
          <Annotation Term="RESO.OData.Metadata.LookupName" String="FieldDataTypes"/>
        </Property>
        <Property Name="PrimaryKeyYN" Type="Edm.Boolean"/>
        <Property Name="CollectionYN" Type="Edm.Boolean"/>
        <Property Name="ExpandableYN" Type="Edm.Boolean"/>
        <Property Name="NullableYN" Type="Edm.Boolean"/>
        <Property Name="Length" Type="Edm.Int32"/>
        <Property Name="Precision" Type="Edm.Int32"/>
        <Property Name="Scale" Type="Edm.Int32"/>
        <Property Name="ReadableYN" Type="Edm.Boolean"/>
        <Property Name="OrderableYN" Type="Edm.Boolean"/>
        <Property Name="UpdatableYN" Type="Edm.Boolean"/>
        <Property Name="SearchableYN" Type="Edm.Boolean"/>
        <Property Name="ModificationTimestamp" Type="Edm.DateTimeOffset" Precision="27" Nullable="false"/>
      </EntityType>

      <EntityContainer Name="Default">
        <EntitySet Name="Model" EntityType="org.reso.metadata.Model"/>
        <EntitySet Name="Field" EntityType="org.reso.metadata.Field"/>
      </EntityContainer>

    </Schema>
  </edmx:DataServices>
</edmx:Edmx>
```

### Example 2: A Model record for the Property Resource

**REQUEST**
```
GET https://api.example.com/Model('Property')
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "https://api.example.com/$metadata#Model/$entity",
  "ModelKey": "Property",
  "ModelName": "Property",
  "ModelType": "Resource",
  "Definition": "Real property listings and the attributes of the properties they describe.",
  "PrimaryKeyFieldKey": "Property.ListingKey",
  "ModificationTimestampFieldKey": "Property.ModificationTimestamp",
  "ReadableYN": true,
  "InsertableYN": false,
  "UpdatableYN": false,
  "DeletableYN": false,
  "ModificationTimestamp": "2025-03-01T12:00:00Z"
}
```

`PrimaryKeyFieldKey` and `ModificationTimestampFieldKey` reference Field records whose `ModelKey` is `Property`, as shown in Example 3.

### Example 3: Field records for the Property Resource

Five of the model's fields are shown: the key, a decimal, an enumeration served through the Lookup Resource, an expansion and the modification timestamp.

**REQUEST**
```
GET https://api.example.com/Field?$filter=ModelKey eq 'Property'
HTTP/2
```

**RESPONSE**
```json
{
  "@odata.context": "https://api.example.com/$metadata#Field",
  "value": [
    {
      "FieldKey": "Property.ListingKey",
      "FieldName": "ListingKey",
      "ModelKey": "Property",
      "LookupName": null,
      "Type": "Edm.String",
      "PrimaryKeyYN": true,
      "CollectionYN": false,
      "ExpandableYN": false,
      "NullableYN": false,
      "Length": 255,
      "Precision": null,
      "Scale": null,
      "ReadableYN": true,
      "OrderableYN": true,
      "UpdatableYN": false,
      "SearchableYN": true,
      "ModificationTimestamp": "2025-03-01T12:00:00Z"
    },
    {
      "FieldKey": "Property.ListPrice",
      "FieldName": "ListPrice",
      "ModelKey": "Property",
      "LookupName": null,
      "Type": "Edm.Decimal",
      "PrimaryKeyYN": false,
      "CollectionYN": false,
      "ExpandableYN": false,
      "NullableYN": true,
      "Length": null,
      "Precision": 14,
      "Scale": 2,
      "ReadableYN": true,
      "OrderableYN": true,
      "UpdatableYN": true,
      "SearchableYN": true,
      "ModificationTimestamp": "2025-03-01T12:00:00Z"
    },
    {
      "FieldKey": "Property.StandardStatus",
      "FieldName": "StandardStatus",
      "ModelKey": "Property",
      "LookupName": "StandardStatus",
      "Type": "Edm.String",
      "PrimaryKeyYN": false,
      "CollectionYN": false,
      "ExpandableYN": false,
      "NullableYN": true,
      "Length": 25,
      "Precision": null,
      "Scale": null,
      "ReadableYN": true,
      "OrderableYN": true,
      "UpdatableYN": true,
      "SearchableYN": true,
      "ModificationTimestamp": "2025-03-01T12:00:00Z"
    },
    {
      "FieldKey": "Property.Media",
      "FieldName": "Media",
      "ModelKey": "Property",
      "LookupName": null,
      "Type": "org.reso.metadata.Media",
      "PrimaryKeyYN": false,
      "CollectionYN": true,
      "ExpandableYN": true,
      "NullableYN": false,
      "Length": null,
      "Precision": null,
      "Scale": null,
      "ReadableYN": true,
      "OrderableYN": false,
      "UpdatableYN": false,
      "SearchableYN": false,
      "ModificationTimestamp": "2025-03-01T12:00:00Z"
    },
    {
      "FieldKey": "Property.ModificationTimestamp",
      "FieldName": "ModificationTimestamp",
      "ModelKey": "Property",
      "LookupName": null,
      "Type": "Edm.DateTimeOffset",
      "PrimaryKeyYN": false,
      "CollectionYN": false,
      "ExpandableYN": false,
      "NullableYN": false,
      "Length": null,
      "Precision": 27,
      "Scale": null,
      "ReadableYN": true,
      "OrderableYN": true,
      "UpdatableYN": false,
      "SearchableYN": true,
      "ModificationTimestamp": "2025-03-01T12:00:00Z"
    }
  ]
}
```

`ListingKey` is the model's key, so `SearchableYN` is `true`. `ModificationTimestamp` is searchable and orderable and not updatable, as the rules in [Section 2.1](#section-21-field-resource) require. `StandardStatus` is served as an `Edm.String` lookup, so `Type` is `Edm.String` and `LookupName` names its lookup. `Media` is a collection expansion, so `CollectionYN` and `ExpandableYN` are `true` and `NullableYN` is `false`.

### Example 4: A Model record in RESO Common Format

A non-OData provider advertises the same model using the RESO Common Format context variable.

```json
{
  "@reso.context": "urn:reso:metadata:2.1:resource:model",
  "ModelKey": "Property",
  "ModelName": "Property",
  "ModelType": "Resource",
  "Definition": "Real property listings and the attributes of the properties they describe.",
  "PrimaryKeyFieldKey": "Property.ListingKey",
  "ModificationTimestampFieldKey": "Property.ModificationTimestamp",
  "ReadableYN": true,
  "InsertableYN": false,
  "UpdatableYN": false,
  "DeletableYN": false,
  "ModificationTimestamp": "2025-03-01T12:00:00Z"
}
```

<br />

# Section 3: Certification

RESO will validate the following during certification, for Data Dictionary 2.2 and later.

**Model, Field and Lookup Resources**
* When the Model Resource or the Field Resource is present, all three of the Model, Field and Lookup Resources MUST be present.
* A Field record that carries `ResourceName` MUST also carry a non-null `ModelKey`. A Field record that carries `DisplayName` MUST also carry `DisplayNames`.
* `ModelType` MUST be `Resource` or `Shape`.
* Every model of type `Resource` MUST have at least one Field record with `PrimaryKeyYN` = `true`; `PrimaryKeyFieldKey`, when present, MUST reference such a Field record of the same model, and MUST be present when the model has exactly one; `ModificationTimestampFieldKey`, when present, MUST reference a Field record of the same model.
* `LookupName`, when present, MUST match a `LookupName` in the Lookup Resource; where the Data Dictionary defines the lookup, it MUST match the Data Dictionary lookup name.
* Every field with `PrimaryKeyYN` = `true` MUST have `SearchableYN` = `true` and `NullableYN` = `false`.
* A null Boolean value is read as not stated, never as `false`.
* Every `ModificationTimestamp` field MUST have `SearchableYN` = `true`, `OrderableYN` = `true` and `UpdatableYN` = `false`.
* Every field with `CollectionYN` = `true` MUST have `NullableYN` = `false`.
* Every model of type `Resource` MUST have at least one Field record.

**Parity with the OData XML metadata** (RESO Web API servers)
* The OData XML metadata MUST contain `EntityType` and `Property` definitions for every model of type `Resource` and its fields, as Web API Core requires.
* For every Model record of type `Resource` there MUST be an entity set of that name in the entity container, and for every entity set there MUST be a Model record of type `Resource`.
* The `PropertyRef` elements of each `EntityType`'s `Key` MUST be exactly the model's Field records with `PrimaryKeyYN` = `true`.
* For every Field record, the type, length, precision, scale and nullability in the Field Resource MUST equal the corresponding `Type`, `MaxLength`, `Precision`, `Scale` and `Nullable` values in the OData XML metadata, for standard and local fields alike. For a `Collection(...)` property, `Type` MUST equal the element type and `CollectionYN` MUST be `true`.
* Standard fields MUST use Data Dictionary names and data types, as tested under the Data Dictionary endorsement.

<br />

# Section 4: Contributors
This document was written by [Joshua Darnell (RESO)](mailto:josh@reso.org).

Thanks to the RESO Transport Workgroup and others in the community for their feedback on the proposal.

<br />

# Section 5: References

Please see the following references for more information regarding topics covered in this document:
* [RESO Data Dictionary 2.1 – Field Resource](https://dd.reso.org/DD2.1/Field/)
* [RESO Data Dictionary 2.1 – Model Resource](https://dd.reso.org/DD2.1/Model/)
* [RESO Data Dictionary 2.1 – Lookup Resource](https://dd.reso.org/DD2.1/Lookup/)
* [RESO Data Dictionary Endorsement (RCP-40), including the Lookup Resource and data type mappings](https://transport.reso.org/proposals/data-dictionary/)
* [RESO Web API Core 2.1.0 (RCP-39)](https://transport.reso.org/proposals/web-api-core/)
* [RESO Common Format (RCP-25)](https://transport.reso.org/proposals/reso-common-format/)
* [RCP-44 Metadata Internationalization and Locale](https://github.com/RESOStandards/transport/blob/fb879c1e60d517e7f139d5face97262055ab7242/proposals/metadata-locales.md)
* [RCP-45 Legacy and Deprecated Data Elements](https://github.com/RESOStandards/transport/blob/ddb8bc9792173ef1a1d57eb5cb560af21d0fc558/proposals/rcp-45-legacy-deprecated-fields-lookups.md)
* [RCP-35 Supporting Single Feeds](https://github.com/RESOStandards/transport/blob/e222c800f2e6ef091fe6c334ce5160456a4e5c56/proposals/rcp-035-single-feeds.md)
* [RCP-48 Add/Edit with Media](https://github.com/RESOStandards/transport/blob/fc35c711487bcd59ffecd02780feb56269fabd73/proposals/web-api-add-edit-with-media.md)
* [OData Common Schema Definition Language (CSDL) XML 4.01](https://docs.oasis-open.org/odata/odata-csdl-xml/v4.01/odata-csdl-xml-v4.01.html)
* [OData JSON Format 4.01](https://docs.oasis-open.org/odata/odata-json-format/v4.01/odata-json-format-v4.01.html)
* [ISO 8601 (Wikipedia)](https://en.wikipedia.org/wiki/ISO_8601)
* [Original Ticket and Discussion](https://github.com/RESOStandards/transport/issues/76)

<br />

# Section 6: Appendices

## Proposed Data Dictionary elements

The fields and lookup values this endorsement proposes are defined in [Section 2.1](#section-21-field-resource) (the Field Resource additions and the FieldDataTypes lookup) and [Section 2.2](#section-22-model-resource) (the Model Resource and the ModelType lookup). They ship in Data Dictionary 2.2; the Data Dictionary 2.1 reference sheets already carry them in draft form, and by decision of RESO Transport this endorsement owns their definitions: where a definition differs from the current Data Dictionary 2.1 text, the sheet follows this document.

The deprecated fields are:

| Resource | Deprecated Field | Replaced by | Note |
| :--- | :--- | :--- | :--- |
| Field | ResourceName | ModelKey | MAY be served for backward compatibility; ModelKey MUST also be present. |
| Field | DisplayName | DisplayNames (RCP-44) | Defined in Data Dictionary 1.7; not listed on the Field Resource in the Data Dictionary 2.0 or 2.1 reference metadata. |

<br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO Transport](mailto:transport@reso.org) with questions about this proposal, or [RESO developer support](mailto:dev@reso.org) with specific development questions.
