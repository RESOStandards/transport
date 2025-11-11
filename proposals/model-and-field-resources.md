# Model and Field Resources

| **RCP** | 42 |
| :--- | :--- |
| **Version** | **1.0.0** |
| **Authors** | [Joshua Darnell (RESO)](mailto:josh@reso.org) |
| **Specification** | [**LINK TO RCP**](./model-and-field-resources.md) |
| **Status** | IN PROGRESS |
| **Date Ratified** | TBD |
| **Dependencies** | [Data Dictionary 2.0+](./data-dictionary.md) |
| **Related Links** | [RESO Common Format](./reso-common-format.md) |


<br />

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

<br />

# Table of Contents
- [Summary of Changes](#summary-of-changes)
- [Section 1: Introduction](#section-1-introduction)
- [Section 2: Specification](#section-2-specification)
- [Section 3: Certification](#section-3-certification)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: Appendices](#section-6-appendices)
- [Section 7: License](#section-7-license)

<br />

# Summary of Changes
* Adds support for advertising capabilities such as searchability and updatability in a standard format.
* Adds a new resource called Model, which can be a resource or shape, and adds other information to top-level models like Definition.
* ResourceName has become ModelKey. Providers can still use ResourceName, but ModelKey must be present.
* DisplayName has been deprecated in favor of the internationalized DisplayNames collection.

<br />

# Section 1: Introduction
In order to make server metadata richer, easier to work with, and more Data Dictionary friendly and transport agnostic, this proposal adds support for a new Model Resource and additions to the Field Resource. 

The term "Resource" is used the same way the Data Dictionary uses it now, but there can be other kinds of models as well so the terminology has been generalized.

This proposal works with [Web API Core](https://github.com/RESOStandards/transport/blob/rcp-39-web-api-core-2-1-0/web-api-core.md) standards as well as [RESO Common Format](./reso-common-format.md), and in the latter case a non-OData server could advertise their metadata in standard format using the Model, Field, and Lookup Resources.

<br />

# Section 2: Specification
This specification adds the Model Resource and makes some additions and changes to the existing Field Resource, as currently defined in Data Dictionary 1.7 and 2.0. 

## 2.1: Field Resource

The following fields will be **removed** from the Field Resource:
* **[ResourceName](https://ddwiki.reso.org/pages/viewpage.action?pageId=395529)** will be deprecated in favor of **ModelKey**. 
* **[DisplayName](https://ddwiki.reso.org/pages/viewpage.action?pageId=395531)** will be deprecated in favor of internationalization and locale support being added in #67 with the DisplayNames field.

Providers MAY still use these fields for backwards compatibility, if needed, but the standard versions of these items MUST also be present in that case. Deprecated items will be treated as local in RESO Analytics.

The following fields will be **added** to the [Field Resource](https://ddwiki.reso.org/display/DDW20/Field+Resource):

* **LookupName**
  * Definition: Identifies which sets of standard or local lookups are referenced in the Lookup Resource.
  * Data Dictionary Type: String
  * OData Type: `Edm.String`
  * Suggested Max Length: `100`
  * Note: The Data Dictionary currently defines lookup names, such as StandardStatus, but LookupName will not be validated against the Data Dictionary Lookup Name. Providers are free to define their own and reuse across enumerated fields, if possible, or not.
* **Type**
  * Definition: The transport-specific data type.
  * Data Dictionary Type: String
  * OData Type: `Edm.String`
  * Nullable: `false`
  * Suggested Max Length: `100`
  * Note: In OData, this should match the types defined in the metadata, including namespaces. For RESO Common Format, the Data Dictionary types should be referenced.
  * Examples: `org.yourOrgName.CustomTypeABC123`, `org.reso.metadata.StandardStatus`, `Property`
* **CollectionYN** 
  * Definition: Boolean field describing whether a given field is a collection. 
  * Data Dictionary Type: Boolean
  * OData Type: `Edm.Boolean`
  * Default: `false`
  * Example: Media expansion in the Property Resource.
* **ExpandableYN**
  * Definition: Boolean field describing whether a given field is expandable.
  * Data Dictionary Type: Boolean
  * OData Type: `Edm.Boolean`
  * Default: `false`
  * Example: Media expansion in the Property Resource.
* **NullableYN**
  * Definition: Boolean field describing whether a given field is nullable.
  * Data Dictionary Type: Boolean
  * OData Type: `Edm.Boolean`
  * Default: `true`
  * Example: ListPrice field in the Property Resource.
  * Note: Collection-based fields are not nullable. Their empty value is `[]`.
* **Length**
  * Definition: Numeric length of data in field. 
  * Data Dictionary Type: Numeric
  * OData Type: `Edm.Int16`, `Edm.Int32`, or `Edm.Int64`
  * Nullable: `true`
* **Precision**
  * Definition: Numeric precision of the given value.
  * Data Dictionary Type: Numeric
  * OData Type: `Edm.Int16`, `Edm.Int32`, or `Edm.Int64`
  * Nullable: `true`
* **Scale** 
  * Definition: Numeric precision of the given value.
  * Data Dictionary Type: Numeric
  * OData Type: `Edm.Int16`, `Edm.Int32`, or `Edm.Int64`
  * Nullable: `true`
* **ReadableYN** 
  * Definition: Boolean value describing whether something is readable.
  * Data Dictionary Type: Boolean
  * OData Type: `Edm.Boolean`
  * Nullable: `true`
  * Default: `false`
* **OrderableYN**
  * Definition: Boolean value describing whether something is orderable.
  * Data Dictionary Type: Boolean
  * OData Type: `Edm.Boolean`
  * Nullable: `true`
  * Default: `false`
* **UpdatableYN**
  * Definition: Boolean value describing whether something is updatable.
  * Data Dictionary Type: Boolean
  * OData Type: `Edm.Boolean`
  * Nullable: `true`
  * Default: `false`
* **SearchableYN** 
  * Definition: Boolean value describing whether something is searchable.
  * Data Dictionary Type: Boolean
  * OData Type: `Edm.Boolean`
  * Nullable: `true`
  * Default: `false`

**Notes**
  * All Keys MUST have `SearchableYN = true`
  * **ModificationTimestamp** fields MUST have `SearchableYN = true`, `OrderableYN = true`, and `UpdatableYN = false`.


## 2.2: Model Resource
A new resource called "Model" will be created with the following fields.

* **ModelKey**
  * Definition: The primary key of the corresponding model.
  * Data Dictionary Type: String
  * OData Type: `Edm.String`
  * Nullable: `false`
  * Suggested Max Length: `50`
  * Note: This doesn't handle compound keys. 
* **ModelName**
  * Definition: The name of the model. 
  * Data Dictionary Type: String
  * OData Type: `Edm.String`
  * Nullable: `false`
  * Suggested Max Length: `50`
* **ModelType**
  * Definition: The type of model. Options are `Resource` or `Shape`. A resource means the same as it always has in the Data Dictionary. A Shape is a custom data type which would show up as a collection or non-collection based field in a resource but wouldn't be available at the top level itself or as an expansion for those using the RESO Web API. 
  * Data Dictionary Type: String List, Single. Closed. 
  * Allowed Values: `Resource`, `Shape`
  * OData Type: `Edm.String`
  * Nullable: `false`
* **Definition**
  * Definition: The human-friendly definition of the given model.
  * Data Dictionary Type: String
  * OData Type: `Edm.String`
  * Nullable: `true`
  * Suggested Max Length: `200`
* **PrimaryKeyFieldKey**
  * Definition: The `FieldKey` value from the Field Resource identifying the primary key.
  * Data Dictionary Type: String
  * OData Type: `Edm.String`
  * Nullable: `false`
  * Suggested Max Length: `50`
* **ModificationTimestampFieldKey**
  * Definition: The `FieldKey` value from the Field Resource identifying the `ModificationTimestamp` field.
  * Data Dictionary Type: String
  * OData Type: `Edm.String`
  * Nullable: `true`
  * Suggested Max Length: `50`
  * Note: Not every resource has a `ModificationTimestamp` field, e.g. `EntityEvent`. In this case, the field key can be null there is no timestamp field. For local resources, the timestamp field could be something besides `ModificationTimestamp`.
* **ReadableYN**
  * Definition: Boolean value representing whether the resource is readable by the current client. 
  * Data Dictionary Type: Boolean
  * OData Type: `Edm.Boolean`
  * Nullable: `true`
  * Default: `false`
  * Note: True if at least one field in the resource is readable. Resources that aren't readable will still be advertised in the metadata, but will return a non-2XX status code if the user tries to access them.
* **InsertableYN**
  * Definition: Boolean value representing whether this resource supports insert operations.
  * Data Dictionary Type: Boolean
  * OData Type: `Edm.Boolean`
  * Nullable: `true`
  * Default: `false`
* **UpdatableYN**
  * Definition: Boolean value representing whether this resource supports update operations.
  * Data Dictionary Type: Boolean
  * OData Type: `Edm.Boolean`
  * Nullable: `true`
  * Default: `false`
  * Note: True if at least one field in the resource is updatable.
* **DeletableYN**
  * Definition: Boolean value representing whether this resource supports delete operations. Not all providers will support delete operations, and in some cases they may mean changing a record to a non-visible status rather than actually removing them in the underlying system.
  * Data Dictionary Type: Boolean
  * OData Type: `Edm.Boolean`
  * Nullable: `true`
  * Default: `false`
* **ModificationTimestamp** - Edm.DateTimeOffset, not nullable.
  * Definition: The ISO 8601 timestamp when the record was last updated.
  * Data Dictionary Type: Timestamp
  * OData Type: `Edm.DateTimeOffset`
  * Nullable: `false`

<br />

# Section 3: Certification

## 3.1: Usage Requirements for RESO Web API Providers
* The Lookup Resource may be supported even if the server is not using the Model and Field Resources, but if so, all three MUST be present. 
* If a server does not support the Model, Field, or Lookup Resources, they should respond with a non-2XX HTTP status if a client tries to access them. This gives a way for the client to know whether to query the OData XML Metadata or not.
* RESO Web API providers using the Model and Field Resources MUST provide the minimum metadata required in OData XML Metadata, i.e. EntityType and Property definitions. Any attributes that can be expressed in OData Metadata such as type and length MUST match the values exactly for those using the Model and Field resources.
* If "Property" exists in the Model Resource with Type "Resource," for example, the OData XML Metadata MUST have an EntityType of Property in its default entity container.
* For those using the Model Resource, if the OData XML Metadata has an EntityType of Property in its default entity container, the provider MUST have a corresponding entry in the Model Resource.
* All Resource/EntityType definitions MUST also have at least one PrimaryKey, but composite keys are possible. The key(s) MUST be present in the Model Resource and match what's in the EntityType definition.
* All models of type "Resource" MUST have at least one standard or local field, meaning there will also be entries in the Field Resource and corresponding Property definitions in the OData XML Metadata. This follows from the PrimaryKey requirement of EntityType definitions. 
* RESO requires an exact match between the field names and data types used in the Data Dictionary and OData names and types. 
* For local fields, there MUST be parity between the Field Resource definition and the OData XML Metadata. [Data type mappings may be found here](./data-dictionary.md#data-type-mappings). These items also map to JSON types and are part of the RESO Common Format.

<br />

# Section 4. Contributors
This document was written by [Joshua Darnell (RESO)](mailto:josh@reso.org).

Thanks to the RESO Transport Workgroup and others in the community for their feedback on the proposal.

<br />

# Section 5: References

Please see the following references for more information regarding topics covered in this document:
* [Data Dictionary Field Resource (DD 1.7+)](https://ddwiki.reso.org/display/DDW20/Field+Resource)
* [RESO Common Format](https://github.com/RESOStandards/transport/blob/main/proposals/reso-common-format.md)
* [Original Ticket and Discussion](https://github.com/RESOStandards/transport/issues/76)

<br />

# Section 6: Appendices
* [ISO 8601 (Wikipedia)](https://en.wikipedia.org/wiki/ISO_8601)

<br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO](mailto:info@reso.org) if you have any questions.
