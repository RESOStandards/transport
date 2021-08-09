**Table of Contents**
- [Introduction](#introduction)
- [Section 1: Purpose](#section-1-purpose)
- [Section 2: Specification](#section-2-specification)
  - [2.1 Terminology](#21-terminology)
  - [2.2 HTTP Protocol](#22-http-protocol)
    - [2.2.1 Version Header](#221-version-header)
    - [2.2.2 Optional OData Headers](#222-optional-odata-headers)
  - [2.3 URL Formatting](#23-url-formatting)
    - [2.3.1 Hostname](#231-hostname)
    - [2.3.2 URI Conventions](#232-uri-conventions)
    - [2.3.3 Metadata URI Conventions](#233-metadata-uri-conventions)
    - [2.3.4 Resource Endpoint](#234-resource-endpoint)
    - [2.4 Data Types](#24-data-types)

# Introduction
The Web API Core Endorsement provides a subset of functionality from the OASIS OData specification relevant to those who need to perform live queries or replicate data using the RESO Web API. This includes the ability to express metadata and provide query support for primitive OData types and enumerations. This document offers normative examples of what these items should look like, both in the metadata and payload. 


# Section 1: Purpose
The RESO Web API defines a standard for creating, updating, reading, or deleting real estate data from web or mobile applications through open standards and JSON Web APIs.

The goals of this specification are to:

* Adopt existing open standards rather than creating new specifications, when possible.
* Leverage existing software toolkits and libraries.
* Favor convention over configuration, which increases predictability by reducing the number of decisions data consumers need to make.

The Web API uses the [Open Data Protocol (OData)](https://www.odata.org/documentation/), which:
* Is an established, existing, open standard.
* Has well-defined functionality that supports significant RESO use cases.
* Has existing open source server and client implementations to promote community adoption.
* Provides extensibility to handle industry-specific use cases, as needed.


Compatible RESO OData Transport client and server applications MUST be implemented according to versions "4.0" or "4.01" of the OData specification. 

All references to the OData specification contained within this document assume version 4.0 of the OData specification by default, unless otherwise specified.

Compatible server and client applications MUST support OData XML Metadata for schema representation and MUST use the JSON response format for non-metadata payloads.

In keeping with OData, both the client and server applications will use standard HTTP methods to perform the operations outlined by this document. RESO will follow the OData standard and extend it, as needed, to fulfill additional industry needs in OData compliant ways. 

RESO Web API servers MUST conform to OData conventions with respect to metadata, query, and response formats as well as HTTP, TLS, and OAuth2 for application layer protocol, transport security, and authorization requirements.


# Section 2: Specification
This specification outlines the requirements for the RESO Web API Core Endorsement, which is a subset of the OData 4.0 specification.

The OData specification is divided into three main sections:
* [Protocol](http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part1-protocol.html)
* [URL Conventions](http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part2-url-conventions.html)
* [Common Schema Definition Language (CSDL)](http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part3-csdl.html)

While there is currently no official RESO reference server at this time, reference servers have been provided by vendors and have been certified with RESO’s new testing tools. 

There is also reference material that should be helpful for developers implementing the Web API Core specification:
* [Data Dictionary 1.7 Reference XML Metadata](https://github.com/RESOStandards/web-api-commander/blob/master/src/main/resources/RESODataDictionary-1.7.xml)
* [Data Dictionary 1.7 Common Schema Open API Reference](https://app.swaggerhub.com/apis/darnjo/RESO-Web-API-Common-Schema/1.7)
* [Web API 2.0.0 Core Testing Specification](https://docs.google.com/document/d/1btCduOpWWzeadeMcSviA8M9dclIz23P-bPUGKwcD0NY/edit?usp=sharing)

Please [contact RESO](mailto:dev@reso.org) if you have questions about the Web API Core specification or testing rules.

## 2.1 Terminology
The following terminology is used within this specification:

| Term | Definition |
| --- | --- |
| **REST** | Representational State Transfer.  More information. |
| **Resource** | A resource is an object with a type, associated data, relationships to other resources, and a set of methods that may operate on it. |
| **RESO Data Dictionary** | A uniform set of field names and data type conventions that set a baseline across the real estate industry for how real estate data will be defined. See the Data Dictionary Overview and DD Wiki (v 1.7) for more information. |
| **Standard Resource** | A data source or collection of data that is represented using the resource definitions defined in the RESO Data Dictionary (e.g. Property, Member, Office). |
| **Local Resource** | A data source or collection of data that is represented using resources not defined in the RESO Data Dictionary. This may also be localized data, such as language localization. |
| **Metadata** | Descriptive information about a data set, object, or resource that helps a recipient understand how resources, fields, and lookups are defined, and relationships between resources. This information contains field names, data types, and annotations that help data producers and consumers understand what’s available on a given server. In OData, metadata is always located at the path /$metadata relative to the provider's service root URL. |
| **Payload** | The term “payload” generally refers to the JSON response returned by the server for a given request. The term is also used when creating or updating data, in which case the payload would be the data provided for create or update. |
| **Schema** | A way of logically defining, grouping, organizing and structuring information about data so it may be understood by different systems. The schema defines the payload a given server is expected to support. |
| **Authorization** | Authorization defines a set of protocols and processes for verifying that a given user has server access to one or more server resources. At the time of writing, the RESO Web API uses the OAuth2 Bearer Token and Client Credentials standards for authorization. |
| **Bearer Token** | A type of authorization that provides simple token-based authentication. More information. |
| **Client Credentials** | A type of authorization grant that uses a client_id and client_secret (essentially username and password) as an additional layer of security in order to provide a Bearer Token upon request. This method is more resilient against man-in-the-middle attacks than Bearer Tokens since there is an additional token request step involved, and tokens may be expired and refreshed programmatically using this approach. More information. |
| **MUST** | The given item is an absolute requirement of the specification. A feature that the specification states MUST be implemented is required in an implementation in order to be considered compliant. If the data is available in the system AND the data is presented for search then it MUST be implemented in the manner described in the specification. See Notes (1), below. |
| **SHOULD** | A feature that the specification states SHOULD be implemented is treated for compliance purposes as a feature that may be implemented. There may exist valid reasons in particular circumstances to ignore an item classified as SHOULD, but the full implications should be understood and the case carefully weighed before choosing not to implement the given feature.  See Notes (1), below. |
| **MAY** | This term means that an item is truly optional. A feature that the specification states MAY be implemented need not be implemented in order to be considered compliant. However, if it is implemented, the feature MUST be implemented in accordance with the specification. See Notes (1), below. |
| **Out of Scope** | This statement means that the specific topic has not been addressed in the current specification but may be addressed in future versions. |
| **N/A** | This term means “not applicable” to the scope of this standard and will not be addressed by this standard specification. |


## 2.2 HTTP Protocol
A compatible RESO Web API server MUST use HTTPS as the protocol declared by the server URL. 

The version MUST be [HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2616) or above, which includes [HTTP/2](https://en.wikipedia.org/wiki/HTTP/2) at the time of writing. 

While OData supports HTTP/1.0, there are many limitations in the HTTP/1.0 specification that we want to avoid. Therefore, we are limiting compatible implementations to HTTP/1.1 or above. For specific HTTP references, please see the references section.

Since the RESO Web API requires that [HTTPS](https://en.wikipedia.org/wiki/HTTPS) and the [OAuth2](https://oauth.net/2/) protocols are used, all server implementations MUST implement [Transport Layer Security (TLS)](https://en.wikipedia.org/wiki/Transport_Layer_Security). 

### 2.2.1 Version Header
The OData version header is used by the server to communicate the currently supported version of the specification:

`OData-Version: [Version]`

where

`[Version] = MAJOR.MINOR`

Examples

`OData-Version: 4.0`

`OData-Version: 4.01`


From [MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers): 

> HTTP headers let the client and the server pass additional information with an HTTP request or response. An HTTP header consists of its case-insensitive name followed by a colon (`:`), then by its value. 
> [Whitespace before the value is ignored](https://developer.mozilla.org/en-US/docs/Glossary/Whitespace).


This means that `odata-version:     4.0` and `OdAtA-vErSiOn:4.01` are also valid, though not recommended. Those using headers should be prepared to process them accordingly.

Requirements
* *Servers MUST provide an OData-Version header in their responses.*
* *Clients MAY specify a given OData-Version in their requests.*


| Client Request | Result |
| --- | --- |
| **No Version** | Server MUST return the current supported version |
| **Current Version** | Server MUST return the current version |
| **Older Version, Still Supported** | Server MUST return requested version |
| **Older Version, Not Supported** | Server MUST return HTTP 400 Bad Request |
| **Newer Version** | Server MUST return HTTP 400 Bad Request |

See Response Message Bodies for details on expected responses.


### 2.2.2 Optional OData Headers
The following optional headers are defined in the specification:

| **Header** | **Functionality** |
| --- | --- |
| `omit=nulls` | It is recommended that that servers fully support this functionality in order to reduce the outbound payload size. |
| `omit=defaults` | It is recommended that servers do not support this functionality in order to ensure that clients get important default values that are integral to the service. |


## 2.3 URL Formatting
The OData transport protocol defines a few standardized URL formatting requirements for ease of use and application interoperability.


### 2.3.1 Hostname 
The hostname of the URL is arbitrary and no naming convention is required. 

The following example protocol and hostname are used in the examples in this document. HTTPS is required.

```https://api.reso.org```

### 2.3.2 URI Conventions
The OData transport protocol defines the following URI conventions:

| **Item** | **URI** |
| --- | --- |
| **Metadata Path** | `https://api.reso.org/reso/odata/$metadata` |
| **Resource Path** | `https://api.reso.org/reso/odata/Resource` |
| **Service Root** | `https://api.reso.org/reso/odata/` |
| **Singleton Resource Path (String Key)** | `https://api.reso.org/reso/odata/Resource('ID')` |
| **Singleton Resource Path (Numeric Key)** | `https://api.reso.org/reso/odata/Resource(123)` 

RESO uses **TitleCase** for Resources, Fields, OData Lookup Values, and Navigation Properties.

### 2.3.3 Metadata URI Conventions
OData offers a [special endpoint](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_AddressingtheModelforaService) for conveying server metadata, located at:

`https://<service root>/$metadata`

The metadata document MUST be located relative to the [OData service root](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_ServiceRootURL), as shown above.

**Example**

Assume the OData service root is located at `https://api.reso.org`

The metadata document MUST be located at `https://api.reso.org/$metadata`

**Example**

Service root definitions MUST not overlap, meaning that one service root cannot be nested in another.  A request to https://api.reso.org/systemA/$metadata would fail if used in conjunction with the service root definition above. 

Vendors who offer multiple endpoints should structure their services accordingly. If there are two systems, systemA and systemB, their service roots would be:

**System A**: `https://api.reso.org/systemA`

**System B**: `https://api.reso.org/systemB`


### 2.3.4 Resource Endpoint
Resources are defined by the server’s XML Metadata document, which also defines the URLs used to query those resources. 

In the language of OData, resource definitions use the `EntityType` tag.

**Example**
Assume a given server defines a Property resource as follows, using the XML Metadata example from [section 2.3.3](#233-metadata-uri-conventions):

```xml
<?xml version="1.0" encoding="UTF-8"?>
<edmx:Edmx Version="4.0" xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx">
  <edmx:DataServices>
    <Schema Namespace="org.reso.metadata" xmlns="http://docs.oasis-open.org/odata/ns/edm">
      <EntityType Name="Property">
        <Key>
          <PropertyRef Name="ListingKey"/>
        </Key>
        <Property MaxLength="255" Name="ListingKey" Type="Edm.String"/>
        <Property Name="ListPrice" Precision="14" Scale="2" Type="Edm.Decimal"/>
        <Property Name="StandardStatus" Type="org.reso.metadata.enums.StandardStatus"/>
        <Property Name="ModificationTimestamp" Precision="27" Type="Edm.DateTimeOffset"/>
      </EntityType>
    </Schema>
    <Schema Namespace="org.reso.metadata.enums" xmlns="http://docs.oasis-open.org/odata/ns/edm">
      <EnumType Name="StandardStatus">
        <Member Name="Active"/>
        <Member Name="Closed"/>
        <Member Name="ComingSoon"/>
        <Member Name="Pending"/>
      </EnumType>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>
```
This metadata defines the following items:

| **Item** | **XML Schema** | **Data Type** | **Attributes** | **Comments** |
| --- | --- | --- | --- | --- |
| **Property Resource** | EntityType | N/A | Key field of ListingKey | Each entity MUST define a Key property. |
| **ListingKey Field** | Property | `Edm.String` | MaxLength of 255 | MaxLength is an optional attribute. |
| **ListPrice Field** | Property | `Edm.Decimal` | Precision is 14, Scale is 2 | Precision and Scale are optional attributes. |
| **StandardStatus Field** | Property | `org.reso.metadata.enums.StandardStatus` | N/A | The type in an `Edm.EnumType` definition is defined by the namespace and references the StandardStatus EnumType defined on line 16. |
| **ModificationTimestamp Field** | Property | `Edm.DateTimeOffset` | Precision of 27 to support the ISO 8601 format | Supported timestamps in this case would be: `2021-05-21T06:28:34+00:00` OR `2021-05-21T06:28:34Z` either of which MAY have a trailing millisecond component, for example: `2021-05-21T06:28:34+00:00.108` OR `2021-05-21T06:28:34.007Z` |
| **StandardStatus Enumeration** | EnumType | `Edm.EnumType` | N/A | Defines enumerations for: Active, Closed, ComingSoon, and Pending using the SimpleIdentifier format. |

**Request Data from the Property Resource without an OData `$filter` Expression**

```json
GET https://api.reso.org/Property
200 OK

{
  "@odata.context": "https://api.reso.org/Property",
  "value": [
    {
      "ListingKey": "a1",
      "ListPrice": 100000.00,
      "ModificationTimestamp": "2020-04-02T02:02:02.02Z",
      "StandardStatus": "Active"
    },
    {
      "ListingKey": "b2",
      "ListPrice": 100001.00,
      "ModificationTimestamp": "2020-04-03T02:02:02.02Z",
      "StandardStatus": "Pending"
    }
  ]
}
```

**Request Data from the Property Resource using an OData `$filter` Expression**
```json
GET https://api.reso.org/Property?$filter=ListPrice gt 100000.00
200 OK

{
  "@odata.context": "https://api.reso.org/Property?$filter=ListPrice gt 100000.00",
  "value": [
    {
      "ListingKey": "b2",
      "ListPrice": 100001.00,
      "ModificationTimestamp": "2020-04-03T02:02:02.02Z",
      "StandardStatus": "Pending"
    }
  ]
}
```

### 2.4 Data Types

This section outlines the standard data types supported by the  Web API Core specification. 

**Data Type Mappings**

The following mappings exist between the RESO Data Dictionary and OData data types, as outlined in RCP-031:

| **Data Dictionary 1.6+** | **Web API 1.0.2+** | **Notes** | 
| --- | --- | --- |
| Boolean | `Edm.Bool` | MUST be one of the literals “true” or “false” (case-sensitive). |
| Collection | `Edm.Collection` | Only supported for Edm.EnumType in Web API Core, and only for those using Collection(Edm.EnumType) to represent lookups. Vendors MAY use collection data types for their own resources. RESO also has defined standard NavigationProperty definitions, which allow expansion between related resources. See RESO’s reference metadata and search for “NavigationProperty” for normative XML Metadata references. |
| Date | `Edm.Date` | MUST be in YYYY-MM-DD format according to the ISO 8601 date format. | 
| Number | `Edm.Decimal` OR `Edm.Double` for decimal values; `Edm.Int64` OR `Edm.Int32` OR `Edm.Int16` for integers. | Numbers that require decimal precision MUST use Edm.Decimal or Edm.Double, whose query and payload semantics are the same. Integers MAY be sized accordingly to support the data in a given field. |
| String | `Edm.String` | MUST be case-sensitive by the OData specification. Field names are also case sensitive when used in the $select, $filter, and $orderby query operators and clients MUST respect case sensitivity defined in the resource metadata. |
| String List, Single | `Edm.EnumType` OR `Edm.String` with the Lookup Resource (RCP-032) | RESO supports either Edm.EnumType OR Edm.String lookups. The former MUST conform to OData SimpleIdentifier conventions, which essentially means they begin with a letter or underscore, followed by at most 127 letters, underscores or digits. Deprecation Notice applies. See Notes. |
| Sting List, Multi | `Edm.EnumType` with `IsFlags=true` OR `Collection(Edm.EnumType)` OR `Collection(Edm.String)` with the Lookup Resource (RCP-032) | RESO supports three kinds of multi-valued enumerations at the moment. Deprecation Notice applies. See Notes. |
| Timestamp | `Edm.DateTimeOffset` | Timestamps also use the ISO 8601 format. Examples: `2021-05-21T16:43:43+00:00` and `2021-05-21T16:43:43Z`. Millisecond precision: `2021-05-21T16:43:43.108+00:00` and `2021-05-21T16:43:43.007Z` |

Notes
* A server MAY return HTTP 413 - Request Entity Too Large if the `$filter` or `$orderby` expressions are too large or complex for the server to process.
* **Deprecation Notice**: OData `Edm.EnumType` definitions will soon be deprecated within RESO standards due to the fact that the `Edm.EnumType` portion usually requires additional knowledge or discovery of vendor-specific namespaces, and human-friendly lookup names are not allowed. RESO is currently migrating to `Edm.String` lookups, and new implementations should use this approach. Please contact RESO with further questions. 
* **Deprecation Notice**: Similar to (2), OData `Edm.EnumType` definitions with `IsFlags=true` will soon be deprecated within RESO standards, with `Collection(Edm.EnumType)` being the current default. However, RESO is currently migrating to `Collection(Edm.String)` for these lookups, which new implementations should use instead. Please contact RESO with further questions. 





