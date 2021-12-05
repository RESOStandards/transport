# RESO Web API Core Specification
|     |     |
| --- | --- |
| **Version** | 2.0.0 |
| **Submitter** | [Joshua Darnell](mailto:josh@reso.org) |
| **Written** | August 2020 |
| **Ratified** | August 2020 |
| **Related RCPs** | [Related RCP](/DATA-DICTIONARY.md) |

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

<br />

# Summary of Changes
* Providers MUST use either OAuth2 Bearer tokens or Client Credentials for authentication (RCP-026).
* Strict Validation of OASIS XML Metadata.
* Added support for `Collection(Edm.EnumType)` for String List, Multi Data Dictionary data types. (RCP-031)
* Removed `DataSystem` endpoint in favor of OData Service Document.
* Removed metallic certification levels in favor of modular Endorsements to provide additional functionality.

<br /><br />

# Introduction
The Web API Core Endorsement provides a subset of functionality from the OASIS OData specification relevant to those who need to perform live queries or replicate data using the RESO Web API. This includes the ability to express metadata and provide query support for primitive OData types and enumerations. 

This document offers normative examples of what these items should look like, both in the metadata and payload.

<br /><br />

# Section 1: Purpose

In general, the RESO Web API defines mechanisms for creating, reading, updating, or deleting data from web or mobile applications using open standards and JSON Web APIs. 
  
The purpose of the Web API Core specification is to establish a set of queries that can be used to retrieve information related to the RESO Data Dictionary and local data elements

The goals of the RESO Web API are as follows:

* Adopt existing open standards rather than creating new specifications, when possible.
* Leverage existing software toolkits and libraries.
* Favor convention over configuration, which increases predictability by reducing the number of decisions data consumers need to make.

The Web API uses the [Open Data Protocol (OData)](https://www.odata.org/documentation/), which:
* Is an established, existing, open standard.
* Has well-defined functionality that supports primary RESO use cases.
* Has existing open source server and client implementations to promote community adoption.
* Provides extensibility to handle industry-specific use cases, as needed.


Compatible RESO OData Transport client and server applications MUST be implemented according to versions "4.0" or "4.01" of the OData specification.

All references to the OData specification contained within this document assume version 4.0 of the OData specification, unless otherwise specified.

Compatible server and client applications MUST support [OData XML Metadata](http://docs.oasis-open.org/odata/odata-csdl-xml/v4.01/odata-csdl-xml-v4.01.html) for schema representation and MUST use the JSON response format for data requests.

RESO Web API servers MUST conform to OData conventions with respect to metadata, query, and response formats as well as HTTP, TLS, and OAuth2 for application layer protocol, transport security, and authentication requirements.

<br /><br />

# Section 2: Specification
This specification outlines the requirements for the RESO Web API Core Endorsement, which is a subset of the OData 4.0 specification.

The OData specification is divided into three main sections:
* [Protocol](http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part1-protocol.html)
* [URL Conventions](http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part2-url-conventions.html)
* [Common Schema Definition Language (CSDL)](http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part3-csdl.html)

While there is no official RESO reference server at this time, [reference servers have been provided](https://www.reso.org/web-api-developer-reference-server/) and have been certified with RESO’s new testing tools.

There is also reference material that should be helpful for developers implementing the Web API Core specification:
* [Data Dictionary 1.7 Reference XML Metadata](https://github.com/RESOStandards/web-api-commander/blob/master/src/main/resources/RESODataDictionary-1.7.xml)
* [Data Dictionary 1.7 Common Schema Open API Reference](https://app.swaggerhub.com/apis/darnjo/RESO-Web-API-Common-Schema/1.7)
* [Web API 2.0.0 Core Testing Specification](https://docs.google.com/document/d/1btCduOpWWzeadeMcSviA8M9dclIz23P-bPUGKwcD0NY/edit?usp=sharing)

Please [contact RESO](mailto:dev@reso.org) if you have questions about the Web API Core specification or testing rules.

<br />

## 2.1 Terminology
The following terminology is used within this specification:

| Term | Definition |
| --- | --- |
| **REST** | Representational State Transfer. [More information](https://en.wikipedia.org/wiki/Representational_state_transfer). |
| **Resource** | A resource is an object with a type, associated data, relationships to other resources, and a set of methods that may operate on it. |
| **RESO Data Dictionary** | A uniform set of field names and data type conventions that set a baseline across the real estate industry for how real estate data will be defined. See the Data Dictionary Overview and DD Wiki (v 1.7) for more information. |
| **Standard Resource** | A data source or collection of data that is represented using the resource definitions defined in the RESO Data Dictionary (e.g. Property, Member, Office). |
| **Local Resource** | A data source or collection of data that is represented using resources not defined in the RESO Data Dictionary. This may also be localized data, such as language localization. |
| **Metadata** | Descriptive information about a data set, object, or resource that helps a recipient understand how resources, fields, and lookups are defined, and relationships between resources. This information contains field names, data types, and annotations that help data producers and consumers understand what’s available on a given server. In OData, metadata is always located at the path /$metadata relative to the provider's service root URL. |
| **Payload** | The term "payload" generally refers to the JSON response returned by the server for a given request. The term is also used when creating or updating data, in which case the payload would be the data provided for create or update. |
| **Schema** | A way of logically defining, grouping, organizing and structuring information about data so it may be understood by different systems. The schema defines the payload a given server is expected to support. |
| **Authorization** | Authorization defines a set of protocols and processes for verifying that a given user has server access to one or more server resources. At the time of writing, the RESO Web API uses the OAuth2 Bearer Token and Client Credentials standards for authorization. |
| **Bearer Token** | A type of authorization that provides simple token-based authentication. More information. |
| **Client Credentials** | A type of authorization grant that uses a client_id and client_secret (essentially username and password) as an additional layer of security in order to provide a Bearer Token upon request. This method is more resilient against man-in-the-middle attacks than Bearer Tokens since there is an additional token request step involved, and tokens may be expired and refreshed programmatically using this approach. More information. |
| **MUST** | The given item is an absolute requirement of the specification. A feature that the specification states MUST be implemented is required in an implementation in order to be considered compliant. If the data is available in the system AND the data is presented for search then it MUST be implemented in the manner described in the specification. See Notes (1), below. |
| **SHOULD** | A feature that the specification states SHOULD be implemented is treated for compliance purposes as a feature that may be implemented. There may exist valid reasons in particular circumstances to ignore an item classified as SHOULD, but the full implications should be understood and the case carefully weighed before choosing not to implement the given feature.  See Notes (1), below. |
| **MAY** | This term means that an item is truly optional. A feature that the specification states MAY be implemented need not be implemented in order to be considered compliant. However, if it is implemented, the feature MUST be implemented in accordance with the specification. See Notes (1), below. |
| **Out of Scope** | This statement means that the specific topic has not been addressed in the current specification but may be addressed in future versions. |
| **N/A** | This term means "not applicable" to the scope of this standard and will not be addressed by this standard specification. |

<br />

## 2.2 HTTP Protocol
A compatible RESO Web API server MUST use HTTPS as the protocol declared by the server URL.

The HTTP version MUST be [HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2616) or above, which includes [HTTP/2](https://en.wikipedia.org/wiki/HTTP/2) at the time of writing.

While OData supports HTTP/1.0, there are many limitations in the HTTP/1.0 specification that we want to avoid. Therefore, we are limiting compatible implementations to HTTP/1.1 or above. For specific HTTP references, please see the references section.

Since the RESO Web API requires that [HTTPS](https://en.wikipedia.org/wiki/HTTPS) and the [OAuth2](https://oauth.net/2/) protocols are used, all server implementations MUST implement [Transport Layer Security (TLS)](https://en.wikipedia.org/wiki/Transport_Layer_Security).

<br />

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

<br />

### 2.2.2 Optional OData Headers
The following optional headers are defined in the specification:

| **Header** | **Functionality** |
| --- | --- |
| `omit=nulls` | It is recommended that that servers fully support this functionality in order to reduce the outbound payload size. |
| `omit=defaults` | It is recommended that servers do not support this functionality in order to ensure that clients get important default values that are integral to the service. |

<br />

## 2.3 URL Formatting
The OData transport protocol defines a few standardized URL formatting requirements for ease of use and application interoperability.

<br />

### 2.3.1 Hostname
The hostname of the URL is arbitrary and no naming convention is required.

The following example protocol and hostname are used in the examples in this document. HTTPS is required.

```https://api.reso.org```

<br />

### 2.3.2 URI Conventions
The OData transport protocol defines the following URI conventions:

| **Item** | **URI** |
| --- | --- |
| **Metadata Path** | `https://api.reso.org/reso/$metadata` |
| **Resource Path** | `https://api.reso.org/reso/Resource` |
| **Service Root** | `https://api.reso.org/reso/` |
| **Singleton Resource Path (String Key)** | `https://api.reso.org/reso/Resource('ID')` |
| **Singleton Resource Path (Numeric Key)** | `https://api.reso.org/reso/Resource(123)`

RESO uses **TitleCase** for Resources, Fields, OData Lookup Values, and Navigation Properties.

<br />

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

with metadata endpoints,

**System A**: `https://api.reso.org/systemA/$metadata`

**System B**: `https://api.reso.org/systemB/$metadata`

respectively.

<br />

### 2.3.4 Resource Endpoint
Resources are defined by the server’s XML Metadata document, which also defines the URLs used to query those resources.

In the language of OData, resource definitions use the `EntityType` tag.

**Example**
Assume a given server defines a Property resource as follows, using the XML Metadata example from [section 2.3.3](#233-metadata-uri-conventions):

```
GET https://api.reso.org/$metadata&$format=application/xml
HTTP/2 200 OK
```

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

<br />

**Request Data from the Property Resource without an OData `$filter` Expression**

```
GET https://api.reso.org/Property
HTTP/2 200 OK
```
```json
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

<br />

**Request Data from the Property Resource using an OData `$filter` Expression**
```
GET https://api.reso.org/Property?$filter=ListPrice gt 100000.00
200 OK
```
```json
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

<br />

## 2.4 Data Types

This section outlines the standard data types supported by the  Web API Core specification.

**Data Type Mappings**

The following mappings exist between the RESO Data Dictionary and OData data types, as outlined in RCP-031:

| **Data Dictionary <img width=200px /> 1.6+** | **Web API 2.0.0+** | **Notes** <img width=1000px /> |
| --- | --- | --- |
| Boolean | [Edm.Bool](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Boolean) | MUST be one of the litera`true` or `false` (case-sensitive). |
| Collection | [Edm.Collection](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752651) | Only supportfor `Edm.EnumType` in Web API Core, and only for those using `Collection(Edm.EnumType)` to represent lookups. <br /><br />Providers MAY use collection data types for their own resources. <br /><br />RESO also has defined standard [NavigationProperty](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Element_edm:NavigationProperty) definitions, which allow expansion between related resources. See [RESO’s reference metadata](https://raw.githubusercontent.com/RESOStandards/web-api-commander/main/src/main/resources/RESODataDictionary-1.7.xml) and search for "NavigationProperty" for normative XML Metadata references. |
| Date | [Edm.Date](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752636) | MUST be in YYYY-MM-DD formaccording to the [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) date format. |
| Number | [Edm.Decimal](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752638) OR [Edm.Double](http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part3-csdl.html#_Toc453752517) for decimal values; [Edm.Int64](http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part3-csdl.html#_Toc453752517) OR [Edm.Int32](http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part3-csdl.html#_Toc453752517) OR [Edm.Int16](http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part3-csdl.html#_Toc453752517) for integers. | Numbers that require decimal precision MUST use Edm.Decimal or Edm.Double, whose query and payload semantics are the same. Integers MAY be sized accordingly to support the data in a given field. |
| String | [Edm.String](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752644) | MUST be case-sensitiby the OData specification. Field names are also case sensitive when used in the `$select`, `$filter`, and `$orderby` query operators and clients MUST respect case sensitivity defined in the resource metadata. |
| String List, Single | [Edm.EnumType](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_8.1_The_edm:EnumType) OR [Edm.String](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752644) with the [Lookup Resource (RCP-032)](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2275152879/RCP+-+WEBAPI-032+Lookup+and+RelatedLookup+Resources+for+Lookup+Metadata) | RESO supports either `Edm.EnumType` OR `EString` lookups. The former MUST conform to OData SimpleIdentifier conventions, which essentially means they begin with a letter or underscore, followed by at most 127 letters, underscores or digits. Deprecation Notice applies. See Notes. |
| Sting List, Multi | [Edm.EnumType](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_8.1_The_edm:EnumType) with `IsFlags=true` OR [Collection(Edm.EnumType)]((http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_8.1_The_edm:EnumType)) OR [Collection(Edm.String)](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752644) with the [Lookup Resource (RCP-032)](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2275152879/RCP+-+WEBAPI-032+Lookup+and+RelatedLookup+Resources+for+Lookup+Metadata) | REsupports three kinds of multi-valued enumerations at the moment. Deprecation Notice applies. See Notes. |
| Timestamp | [Edm.DateTimeOffset](http://docs.oasis-open.org/odata/odata-csdl-xml/v4.01/csprd02/odata-csdl-xml-v4.01-csprd02.html#sec_DateTimeOffset) | Timestamps also use the [ISO 86format](https://en.wikipedia.org/wiki/ISO_8601). Examples: `2021-05-21T16:43:43+00:00` and `2021-05-21T16:43:43Z`. Millisecond precision: `2021-05-21T16:43:43.108+00:00` and `2021-05-21T16:43:43.007Z` |

**Notes**
* _A server MAY return HTTP 413 - Request Entity Too Large if the `$filter` or `$orderby` expressions are too large or complex for the server to process._
* _**Deprecation Notice**: OData `Edm.EnumType` definitions will soon be deprecated within RESO standards due to the fact that the `Edm.EnumType` portion usually requires additional knowledge or discovery of vendor-specific namespaces, and human-friendly lookup names are not allowed. RESO is currently migrating to `Edm.String` lookups, and new implementations should use this approach. Please contact RESO with further questions._
* _**Deprecation Notice**: Similar to (2), OData `Edm.EnumType` definitions with `IsFlags=true` will soon be deprecated within RESO standards, with `Collection(Edm.EnumType)` being the current default. However, RESO is currently migrating to `Collection(Edm.String)` for these lookups, which new implementations should use instead. Please contact RESO with further questions._

<br />

## 2.5 Query Support
Each OData data type supports query operators relevant to its type. For instance, dates, timestamps, and numbers allow for greater than and less than comparisons.

Specifics regarding data types and query operators are outlined in the sections that follow.

See the OData specification for further details regarding Query Support.

_The query operators shown in this section are MUST requirements for the Web API Core Endorsement unless otherwise specified._

<br />

### 2.5.1 Metadata Request
OData supports both XML and JSON metadata formats.

Servers MAY support JSON metadata, but RESO requires they MUST support the XML metadata format.

The [OData format parameter](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#_Toc31361046) allows clients to request a specific format, such as `$format=application/xml` for XML Metadata.

If no `$format` parameter is passed, the server MUST return the `application/xml` format of the metadata when a request is made to the `/$metadata` endpoint.

**Example**
```
GET https://api.reso.org/$metadata?$format=application/xml
HTTP/2 200 OK
```
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
**Notes**
* _For more information about the XML Metadata format, see the [OData 4.0 Errata 03 Specification](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752500)._
* _The `edmx:DataServices` element MUST contain one or more [`edm:Schema` elements](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_4.1_The_edm:Schema) which define the schemas exposed by the OData service._
* _Each EntityType definition MUST define a Key. [More information](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752555)._
* _A schema is identified by a [namespace](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Namespace). All `edm:Schema` elements MUST have a namespace defined through a Namespace attribute which MUST be unique within the document, and SHOULD be globally unique. A schema cannot span more than one document. [More information](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752522)._
* _OData EntityType, Property, and EnumType Member elements MUST conform to OData’s [SimpleIdentifier](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_SimpleIdentifier) naming conventions._
* _OASIS publishes [XML Metadata XSD definitions](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/schemas/) that can be used to validate the syntax of an XML Metadata document._
* _RESO offers [reference metadata in XML format](https://raw.githubusercontent.com/RESOStandards/web-api-commander/main/src/main/resources/RESODataDictionary-1.7.xml) which can be used as a guide, and corresponds to RESO Data Dictionary 1.7._
* _The above example does not demonstrate the use of annotations, which are outlined in the [reference XML Metadata document](https://raw.githubusercontent.com/RESOStandards/web-api-commander/main/src/main/resources/RESODataDictionary-1.7.xml)._

<br />

### 2.5.2 Service Document Request
Servers MUST support a service document request, according to the OData Minimal Conformance Rules.

> The service root URL identifies the root of an OData service. A GET request to this URL returns the format-specific service document, see [OData-JSON](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#ODataJSONRef).
>
> The service root URL MUST terminate in a forward slash.
>
> The service document enables simple hypermedia-driven clients to enumerate and explore the resources published by the OData service.

RESO validates that the service document request can be made and that it produces a valid JSON response, but does not have any additional requirements about what the document must contain. Data providers may choose which entities they want advertise in their service document according to their business needs.

**Example**

Assuming the metadata in [section 2.5.1](#251-metadata-request),

```
GET https://api.reso.org/$metadata?$format=application/xml
HTTP/2 200 OK
```

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

The Service Document would be as follows:
```
GET https://api.reso.org
HTTP/2 200 OK
```
```json
{
  "@odata.context": "$metadata",
  "value": [{
    "name": "Property",
    "url": "Property"
  }]
}
```

<br />

### 2.5.3 Fetch by Key
OData provides a way to access a single record by its key, called a singleton record.

How the key is referenced depends on its type.

The following examples assume that a resource called Property is defined.

**Note**: _unlike requests that return a collection of items in a value array, singleton requests return a instance of the requested type at the top level if a given record exists._

**String Keys**

String keys are surrounded with single quotes when used in an OData key query:
```
GET https://api.reso.org/Property('a1')
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property('a1')",
  "ListingKey": "a1",
  "BedroomsTotal": 5,
  "ListPrice": 100000.00,
  "StreetName": "Main",
  "ModificationTimestamp": "2020-04-02T02:02:02.02Z",
  "ListingContractDate": "2020-04-02",
  "StandardStatus": "ActiveUnderContract",
  "AccessibilityFeatures": ["AccessibleApproachWithRamp", "AccessibleEntrance", "Visitable"]
}
```

**Numeric Keys**

Numeric keys do not use any special characters:
```
GET https://api.reso.org/Property(123)
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property(123)",
  "ListingKeyNumeric": 123,
  "BedroomsTotal": 5,
  "ListPrice": 100000.00,
  "StreetName": "Main",
  "ModificationTimestamp": "2020-04-02T02:02:02.02Z",
  "ListingContractDate": "2020-04-02",
  "StandardStatus": "ActiveUnderContract",
  "AccessibilityFeatures": ["AccessibleApproachWithRamp", "AccessibleEntrance", "Visitable"]
}
```

<br />

### 2.5.4 `$select` Operator

OData allows clients to specify which fields they would like returned in a given payload through the use of the [$select operator](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#_Toc31361040).

```
GET https://api.reso.org/Property?$select=ListingKey,ModificationTimestamp
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$select=ListingKey,ModificationTimestamp",
  "value": [
    {
      "ListingKey": "a1",
      "ModificationTimestamp": "2020-04-02T02:02:02.02Z"
    },
    {
      "ListingKey": "b2",
      "ModificationTimestamp": "2020-04-02T02:02:02.007Z"
    }
  ]
}
```
RESO Web API Core servers MUST support the `$select` operator.

<br />

### 2.5.5 `$top` Operator

The [OData `$top` operator](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#_Toc31361042) allows clients to specify the number of records they would like to request from a given server.

Servers MAY respond with a page size different than the one requested, and clients should be prepared to respond accordingly.

```
GET https://api.reso.org/Property?$select=ListingKey,ModificationTimestamp&$top=1
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$select=ListingKey,ModificationTimestamp&$top=1",
  "value": [
    {
      "ListingKey": "a1",
      "ModificationTimestamp": "2020-04-02T02:02:02.02Z"
    }
  ]
}
```

RESO Web API Core servers MUST support the `$top` operator.

<br />

### 2.5.6 `$count` Operator

The [`$count` system query option](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_SystemQueryOptioncount) allows clients to request a count of the matching resources included with the resources in the response.

The `$count` query option has a Boolean value of `true` or `false`.

The semantics of $count are covered in the [OData-Protocol](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#odata) document.

```
GET https://api.reso.org/Property?$select=ListingKey,ModificationTimestamp&$top=1&$count=true
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$select=ListingKey,ModificationTimestamp&$top=1&$count=true",
  "@odata.count": 2,
  "value": [
    {
      "ListingKey": "a1",
      "ModificationTimestamp": "2020-04-02T02:02:02.02Z"
    }
  ]
}
```
RESO Web API Core servers MUST support the `$count` operator.

<br />

### 2.5.7 `$skip` Operator
The [`$skip` query option](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#_Toc31361042) requests the number of items in the queried collection that are to be skipped and not included in the result. A client can request a particular page of items by combining `$top` and `$skip`.

The semantics of `$top` and `$skip` are covered in the [OData-Protocol](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#odata) document. The [OData-ABNF](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#ABNF) top and skip syntax rules define the formal grammar of the `$top` and `$skip` query options respectively.

```
GET https://api.reso.org/Property?$select=ListingKey,ModificationTimestamp&$count=true&$top=1&$skip=1
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$select=ListingKey,ModificationTimestamp&$count=true&$top=1&$skip=1",
  "@odata.count": 2,
  "value": [
    {
      "ListingKey": "b2",
      "ModificationTimestamp": "2020-04-02T02:02:02.007Z"
    }
  ]
}
```

 RESO Web API Core servers MUST support the `$skip` operator but providers are allowed to decide for themselves how many records they want to allow skipping over.

 <br />

 ### 2.5.8 `$orderby` Operator

 The [`$orderby` system query option](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_SystemQueryOptionorderby) allows clients to request resources in a particular order.

The semantics of `$orderby` are covered in the [OData-Protocol](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#odata) document.

The [OData-ABNF](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#ABNF) orderby syntax rule defines the formal grammar of the `$orderby` query option.

```
GET https://api.reso.org/Property?$select=ListingKey,ModificationTimestamp&$orderby=ModificationTimestamp asc
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$select=ListingKey,ModificationTimestamp&$orderby=ModificationTimestamp asc",
  "value": [
    {
      "ListingKey": "b2",
      "ModificationTimestamp": "2020-04-02T02:02:02.007Z"
    }
     {
      "ListingKey": "a1",
      "ModificationTimestamp": "2020-04-02T02:02:02.02Z"
    }
  ]
}
```

RESO Web API Core servers MUST support the `$orderby` operator.

<br />

### 2.5.9 `$filter` Operator

OData `$filter` expressions provide query support for boolean search expressions.

This includes logical operators such as AND, OR, and NOT, as well as greater than, greater than or equal, less than, less than or equal, and not equals for applicable OData primitive types, and query support for enumerations.

**OData Primitive Types**

Primitive types are things like integers, decimal numbers, dates, and timestamps. 

Strings are not included in the RESO Web API Core specification at this time, but are expected to behave [according to the OData specification](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_StringandCollectionFunctions), including case sensitivity.

**Enumerations**

Enumerations define the allowed values in a given lookup field.

They can either be single enumerations, where only one value is allowed within a given field, or multiple enumerations, in which case there is a list of values.

The standard values used in transport are determined by a given lookup field’s underlying [data type](#24-data-types).

At this document’s time of writing, most implementations use `Edm.EnumType` enumerations since they’ve historically been the only supported lookup data type. As such, the single and multiple enumeration examples contained in this document use `Edm.EnumType` and `Collection(Edm.EnumType)`, respectively.

_**Note**: support for `Edm.String` versions of enumerations, which use human-friendly display names as values, has recently been added and is the preferred approach for new implementations. The RESO community is in the process of moving away from `Edm.EnumType` lookups to simplify implementations and improve user friendliness. See [RCP-032](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2275152879) for information about string lookups._

<br />

#### 2.5.9.1 OData Primitive Types

This section outlines logical operators and query expressions available in Web API Core for the following [OData Primitive Types](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752517):
* Integers: `Edm.Int16`, `Edm.Int32`, and `Edm.Int64`
* Decimals: `Edm.Decimal` and `Edm.Double`
* Dates: `Edm.Date`
* Timestamps: `Edm.DateTimeOffset`

**Note**: _String query operators are not part of the RESO Web API Core specification at this time._


The quoted descriptions outlined in this document contain excerpts from the [OData 4.01 Specification](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_QueryOptions). Please see the sections linked to in each of the following sections for the most up-to-date information.

The examples presented here assume that the server is using a subset of [RESO’s reference XML Metadata](https://raw.githubusercontent.com/RESOStandards/web-api-commander/main/src/main/resources/RESODataDictionary-1.7.xml) with the following Primitive Type fields available:
* BedroomsTotal (`Edm.Int64`)
* ListPrice (`Edm.Decimal`)
* ListingContractDate (`Edm.Date`)
* ModificationTimestamp (`Edm.DateTimeOffset`)

Enumerations are also shown in the sample payloads. Queries for enumerations are covered in later sections of this document.

<br />

#### 2.5.9.2 Equals

[OData Documentation](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_Equals)
> The `eq` operator returns true if the left operand is equal to the right operand, otherwise it returns `false`.
>
> When applied to operands of entity types, the `eq` operator returns `true` if both operands represent the same entity, or both operands represent `null`.
>
> When applied to operands of complex types, the `eq` operator returns `true` if both operands have the same structure and same values, or both operands represent `null`.
>
> When applied to ordered collections, the `eq` operator returns `true` if both operands have the same cardinality and each member of the left operand is equal to the corresponding member of the right operand.
>
> For services that support comparing unordered collections, the `eq` operator returns `true` if both operands are equal after applying the same ordering on both collections.
>
>Each of the special values `null`, `-INF`, and `INF` is equal to itself, and only to itself.
>
>The special value `NaN` is not equal to anything, even to itself.

**Example**

_Total number of bedrooms equals 3._

```
GET https://api.reso.org/Property?$filter=BedroomsTotal eq 3
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=BedroomsTotal eq 3",
  "value": [
    {
      "ListingKey": "a1",
      "BedroomsTotal": 3,
      "ListPrice": 100000.00,
      "StreetName": "Main",
      "ModificationTimestamp": "2020-04-02T02:02:02.02Z",
      "ListingContractDate": "2020-04-02",
      "StandardStatus": "ActiveUnderContract",
      "AccessibilityFeatures": ["AccessibleApproachWithRamp", "AccessibleEntrance", "Visitable"]
    }
  ]
}
```

<br />

#### 2.5.9.3 Not Equals

[OData Documentation](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_NotEquals)
> The `ne` operator returns `true` if the left operand is not equal to the right operand, otherwise it returns `false`.
>
> When applied to operands of entity types, the `ne` operator returns `true` if the two operands do not represent the same entity.
>
> When applied to operands of complex types, the `ne` operator returns `true` if the operands do not have the same structure and same values.
>
> When applied to ordered collections, the `ne` operator returns `true` if both operands do not have the same cardinality or any member of the left operand is not equal to the corresponding member of the right operand.
>
> For services that support comparing unordered collections, the `ne` operator returns `true` if both operands do not have the same cardinality or do not contain the same members, in any order.
>
> Each of the special values `null`, `-INF`, and `INF` is not equal to any value but itself.
>
> The special value `NaN` is not equal to anything, even to itself.
>
> The `null` value is not equal to any value but itself.

**Example**

_Total number of bedrooms does not equal 3._

```
GET https://api.reso.org/Property?$filter=BedroomsTotal ne 3
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=BedroomsTotal ne 3",
  "value": [
    {
      "ListingKey": "a2",
      "BedroomsTotal": 4,
      "ListPrice": 100000.00,
      "StreetName": "Main",
      "ModificationTimestamp": "2020-05-02T02:02:02.02Z",
      "ListingContractDate": "2020-05-02",
      "StandardStatus": "Active",
      "AccessibilityFeatures": ["AccessibleApproachWithRamp"]
    }
  ]
}
```

<br />

#### 2.5.9.4 Greater Than

[OData Documentation](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_GreaterThan)

> The `gt` operator returns `true` if the left operand is greater than the right operand, otherwise it returns `false`.
>
> The special value `INF` is greater than any number, and any number is greater than `-INF`.
>
> The Boolean value `true` is greater than `false`.
>
> If any operand is `null`, the operator returns `false`.

**Example**

_List price is greater than $100,000.00._

```
GET https://api.reso.org/Property?$filter=ListPrice gt 100000.00
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=ListPrice gt 100000.00",
  "value": [
    {
      "ListingKey": "a3",
      "BedroomsTotal": 4,
      "ListPrice": 100000.01,
      "StreetName": "Main",
      "ModificationTimestamp": "2020-06-02T02:02:02.02Z",
      "ListingContractDate": "2020-06-02",
      "StandardStatus": "Closed",
      "AccessibilityFeatures": ["AccessibleApproachWithRamp", "AccessibleEntrance", "Visitable"]
    }
  ]
}
```

<br />

#### 2.5.9.5 Greater Than or Equal

[OData Documentation](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_GreaterThanorEqual)
> The `ge` operator returns `true` if the left operand is greater than or equal to the right operand, otherwise it returns `false`.
>
> See rules for `gt` and `eq` for details.

**Example**

_Modification timestamp is greater than or equal to May 22 2022 at midnight in UTC time._

```
GET https://api.reso.org/Property?$filter=ModificationTimestamp ge 2021-05-22T00:00:00Z
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=ModificationTimestamp ge 2021-05-22T00:00:00Z",
  "value": [
    {
      "ListingKey": "a4",
      "BedroomsTotal": 4,
      "ListPrice": 100000.01,
      "StreetName": "1st",
      "ModificationTimestamp": "2021-05-22T00:01:01.01.123Z",
      "ListingContractDate": "2021-05-01",
      "StandardStatus": "Active",
      "AccessibilityFeatures": ["Visitable"]
    }
  ]
}
```

<br />

#### 2.5.9.6 Less Than

[OData Documentation](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_LessThan)
> The `lt` operator returns `true` if the left operand is less than the right operand, otherwise it returns `false`.
>
> The special value `-INF` is less than any number, and any number is less than `INF`.
>
> The Boolean value `false` is less than `true`.
>
>If any operand is `null`, the operator returns `false`.

**Example**

_Listing contract date is less than Jan 1 2021._

```
GET https://api.reso.org/Property?$filter=ListingContractDate lt 2021-01-01
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=ListingContractDate lt 2021-01-01",
  "value": [
    {
      "ListingKey": "a5",
      "BedroomsTotal": 4,
      "ListPrice": 100000.01,
      "StreetName": "1st",
      "ModificationTimestamp": "2020-12-31T00:01:01.01.007Z",
      "ListingContractDate": "2020-12-31",
      "StandardStatus": "Closed",
      "AccessibilityFeatures": []
    }
  ]
}
```

<br />

#### 2.5.9.7 Less Than or Equal

[OData Documentation](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_LessThanorEqual)
The `le` operator returns `true` if the left operand is less than or equal to the right operand, otherwise it returns `false`.

See rules for [`lt`](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_LessThan) and [`eq`](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_Equals) for details.

**Example**

_Listing contract date is less than or equal to Dec 31 2020._

```
GET https://api.reso.org/Property?$filter=ListingContractDate le 2020-12-31
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=ListingContractDate le 2020-12-31",
  "value": [
    {
      "ListingKey": "a5",
      "BedroomsTotal": 4,
      "ListPrice": 100000.01,
      "StreetName": "1st",
      "ModificationTimestamp": "2020-12-31T00:01:01.01.007Z",
      "ListingContractDate": "2020-12-31",
      "StandardStatus": "Closed",
      "AccessibilityFeatures": []
    }
  ]
}
```

<br />

#### 2.5.9.8 Single Enumerations

These are single-valued lookups, such as the [StandardStatus](https://ddwiki.reso.org/display/DDW17/StandardStatus+Field) field.

There are two ways to express single enumerations in the RESO Web API Core specification:
* `Edm.EnumType`
* `Edm.String`, as outlined in [RCP-032](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2275152879)

`Edm.EnumType` enumerations are the most prevalent at the time of writing.

RESO is transitioning to `Edm.String` enumerations and new implementations should follow that path.

<br />

##### 2.5.9.8.1 `Edm.EnumType` Enumerations
OData provides the `Edm.EnumType` data type to express enumerations. [More information](https://docs.oasis-open.org/odata/odata-csdl-xml/v4.01/odata-csdl-xml-v4.01.html#sec_EnumerationType).

The `Edm.EnumType` data type supports `has`, `eq`, and `ne` queries.

The StandardStatus field is used in the examples in this section.

Assume given server has the following XML Metadata:

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

<br />

**`has` Operator**

The [OData has operator](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#_Toc31360967) is defined for items of `Edm.EnumType` as follows:

> The `has` operator returns `true` if the right operand is an enumeration value whose flag(s) are set on the left operand.
>
> The `null` value is treated as unknown, so if one operand evaluates to `null`, the has operator returns `null`.

**Example**

_Find listings where StandardStatus is Active._

```
GET https://api.reso.org/Property?$filter=StandardStatus has org.reso.metadata.enums.StandardStatus'Active'
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=StandardStatus has org.reso.metadata.enums.StandardStatus'Active'",
  "value": [
    {
      "ListingKey": "a1",
      "BedroomsTotal": 2,
      "ListPrice": 100000.01,
      "StreetName": "Main",
      "ModificationTimestamp": "2020-06-02T02:02:02.02Z",
      "ListingContractDate": "2020-06-02",
      "StandardStatus": "Active",
      "AccessibilityFeatures": ["AccessibleEntrance", "Visitable"]
    }
  ]
}
```
**Note**: _one of the drawbacks of the OData `Edm.EnumType` data type is that it’s dependent on the namespace it was defined in. This is the reason the preceding query uses `org.reso.metadata.enums.StandardStatus'Active'` as part of the filter expression. There is no RESO standard for a single namespace to put standard enumerations in, so they vary among implementations. This is one reason RESO is migrating to `Edm.String` values instead, which don’t require a namespace._

<br />

**`eq` Operator**

[OData added support](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_PrimitiveLiterals) for the `eq` operator on `Edm.EnumType` in version 4.01. It is included in the Web API Core specification. The syntax is similar to the `has` operator, and right hand values MUST use the correct namespaces as well.

**Example**

_Find listings where StandardStatus is Active._

```
GET https://api.reso.org/Property?$filter=StandardStatus eq org.reso.metadata.enums.StandardStatus'Active'
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=StandardStatus eq org.reso.metadata.enums.StandardStatus'Active'",
  "value": [
    {
      "ListingKey": "a1",
      "BedroomsTotal": 2,
      "ListPrice": 100000.01,
      "StreetName": "Main",
      "ModificationTimestamp": "2020-06-02T02:02:02.02Z",
      "ListingContractDate": "2020-06-02",
      "StandardStatus": "Active",
      "AccessibilityFeatures": ["AccessibleEntrance", "Visitable"]
    }
  ]
}
```

<br />

**`ne` Operator**

[OData also added support](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_PrimitiveLiterals) for the `ne` operator on `Edm.EnumType` in version 4.01. It is included in the Web API Core specification. This allows consumers to filter on enumerations using `ne` rather than `not (has ...)`.

Right hand values MUST use correct namespaces.

**Example**

_Find listings where StandardStatus is not Active._

```
GET https://api.reso.org/Property?$filter=StandardStatus ne org.reso.metadata.enums.StandardStatus'Active'
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=StandardStatus ne org.reso.metadata.enums.StandardStatus'Active'",
  "value": [
    {
      "ListingKey": "a2",
      "BedroomsTotal": 3,
      "ListPrice": 100000.00,
      "StreetName": "1st",
      "ModificationTimestamp": "2020-07-02T02:02:02.02Z",
      "ListingContractDate": "2020-07-02",
      "StandardStatus": "ActiveUnderContract",
      "AccessibilityFeatures": []
    }
  ]
}
```

<br />

##### 2.5.9.8.2 `Edm.String` Enumerations

Support for string-based enumerations was added in Web API Core through the use of the Lookup resource, [as outlined in RCP-032](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2275152879).

This resource is still in DRAFT status. Please [contact RESO](mailto:dev@reso.org) if you are interested in being certified using `Edm.String` lookups.

<br />

#### 2.5.9.9 Multiple Enumerations

The Web API Core specification currently offers three ways to express multiple enumerations:
* [`Edm.EnumType`, with or without `IsFlags=true`](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#25991-odata-isflagstrue)
* [`Collection(Edm.EnumType)`](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#25992-collection-of-edmenumtype)
* [`Collection(Edm.String)`](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#25993-collection-of-edmstring)

The `IsFlags=true` method is not recommended for current implementations due to size limitations of the underlying data type and will be deprecated in future versions of the Web API.

Currently, [RESO’s reference XML metadata](https://raw.githubusercontent.com/RESOStandards/web-api-commander/main/src/main/resources/RESODataDictionary-1.7.xml) uses `Collection(Edm.EnumType)` for normative examples of multiple enumerations.

RESO is in the process of transitioning to human-friendly string values using `Collection(Edm.String)` for multiple enumerations, [as outlined in RCP-032](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2275152879). New implementations are encouraged to take this approach. Please [contact RESO](mailto:dev@reso.org) if you are interested in being certified using string lookups.

<br />

##### 2.5.9.9.1 OData `IsFlags=true`

In the past, it was common for implementations to use OData IsFlags enumerations. While this type is still supported for backwards compatibility, multiple enumerations should use `Collection(Edm.EnumType)` or `Collection(Edm.String)` instead.

The current [RESO XML reference metadata](https://raw.githubusercontent.com/RESOStandards/web-api-commander/main/src/main/resources/RESODataDictionary-1.7.xml) uses `Collection(Edm.EnumType)`.

One of the main reasons the `IsFlags=true` approach is being deprecated is that if the OData specification were followed strictly, it limits the total number of possible values to 64.

To understand why this is the case, OData uses an underlying type of at most `Edm.Int64` to represent flag based enumerations. The design of the `IsFlags=true` lookup suggests that bitwise comparisons across multiple entries should be possible using this approach, meaning that if each item in the list is mutually exclusive there are only 64 choices.

The [OData specification](https://docs.oasis-open.org/odata/odata-csdl-xml/v4.01/odata-csdl-xml-v4.01.html#sec_FlagsEnumerationType) shows the following example for Enumeration Types:

```xml
<EnumType Name="Pattern" UnderlyingType="Edm.Int32" IsFlags="true">
  <Member Name="Plain"             Value="0" />
  <Member Name="Red"               Value="1" />
  <Member Name="Blue"              Value="2" />
  <Member Name="Yellow"            Value="4" />
  <Member Name="Solid"             Value="8" />
  <Member Name="Striped"           Value="16" />
  <Member Name="SolidRed"          Value="9" />
  <Member Name="SolidBlue"         Value="10" />
  <Member Name="SolidYellow"       Value="12" />
  <Member Name="RedBlueStriped"    Value="19" />
  <Member Name="RedYellowStriped"  Value="21" />
  <Member Name="BlueYellowStriped" Value="22" />
</EnumType>
```

Notice how Red has a value of 1 (2<sup>0</sup>), Blue has a value of 2 (2<sup>1</sup>), and Striped has a value of 16 (2<sup>4</sup>), which is equivalent to the value of RedBlueStriped, with a value of 19 (0001 0011).

The underlying value in the payload from the example above would be 19, the `Edm.Int64` representation of the value defined in the lookup. This MUST match what’s defined in the server metadata.

RESO does not explicitly validate bitmapped values to ensure that lookup choices do not overlap.

<br />

**`has` Operator**

The [OData `has` operator](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#_Toc31360967) is defined for items of `Edm.EnumType` as follows:

> The `has` operator returns true if the right operand is an enumeration value whose flag(s) are set on the left operand.
>
> The `null` value is treated as unknown, so if one operand evaluates to `null`, the has operator returns  `null`.

**Example**

_Find listings with AccessibleEntrance in the AccessibilityFeatures field._

Assume AccessibleEntrance is defined with an underlying value of 4:

```
GET https://api.reso.org/Property?$filter=AccessibilityFeatures has org.reso.metadata.enums.AccessibilityFeatures'AccessibleEntrance'
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=AccessibilityFeatures has org.reso.metadata.enums.AccessibilityFeatures'AccessibleEntrance'",
  "value": [
    {
      "ListingKey": "a1",
      "BedroomsTotal": 2,
      "ListPrice": 100000.01,
      "StreetName": "Main",
      "ModificationTimestamp": "2020-06-02T02:02:02.02Z",
      "ListingContractDate": "2020-06-02",
      "StandardStatus": "Active",
      "AccessibilityFeatures": "AccessibleBedroom,AccessibleDoors"
    }
  ]
}
```

<br />

##### 2.5.9.9.2 Collection of `Edm.EnumType`

The RESO Web API Core specification allows for multiple enumerations to be defined in terms of collections of OData `Edm.EnumType` definitions.

While this approach is not well documented in OData reference material, it is nonetheless valid and was previously offered in order to overcome the [64-value limitation of IsFlags enumerations](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#25991-odata-isflagstrue).

Let’s consider the [AccessibilityFeatures](https://ddwiki.reso.org/display/DDW17/AccessibilityFeatures+Field) field, which is defined as follows in the [RESO reference XML metadata](https://raw.githubusercontent.com/RESOStandards/web-api-commander/main/src/main/resources/RESODataDictionary-1.7.xml):

```xml
  <EntityType Name="Property">
    <Property Name="AccessibilityFeatures" Type="Collection(org.reso.metadata.enums.AccessibilityFeatures)" />
  </EntityType>
  <EnumType Name="AccessibilityFeatures">
    <Member Name="AccessibleEntrance"/>
    <Member Name="AccessibleFullBath"/>
    <Member Name="AccessibleHallways"/>
    <Member Name="AccessibleKitchen"/>
  </EnumType>
```

<br />

**Collection Queries**

Multiple enumerations with the `Collection(Edm.EnumType)` data type use `any()` and `all()` lambda operators rather than the `has` operator, as outlined in `IsFlags` Enumerations.

Similar to other `Edm.EnumType` definitions, queries rely on namespaces being present, when defined.

From the [OData specification](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#_Toc31361024):

> OData defines two operators that evaluate a Boolean expression on a collection. Both must be prepended with a navigation path that identifies a collection.
>
> 4.01 Services MUST support case-insensitive lambda operator names. Clients that want to work with 4.0 services MUST use lower case lambda operator names.
>
> The argument of a lambda operator is a case-sensitive lambda variable name followed by a colon (:) and a Boolean expression that uses the lambda variable name to refer to properties of members of the collection identified by the navigation path.
>
> If the name chosen for the lambda variable matches a property name of the current resource referenced by the resource path, the lambda variable takes precedence. Clients can prefix properties of the current resource referenced by the resource path with [`$it`](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_it).
>
> Other path expressions in the Boolean expression neither prefixed with the lambda variable nor $it are evaluated in the scope of the collection instances at the origin of the navigation path prepended to the lambda operator.

<br />

**`any()` Operator**

> The any operator applies a Boolean expression to each member of a collection and returns `true` if and only if the expression is true for any member of the collection, otherwise it returns `false`. This implies that the any operator always returns `false` for an empty collection.
>
> The `any` operator can be used without an argument expression. This short form returns `false` if and only if the collection is empty.

**Example**

_Find listings where AccessibilityFeatures has AccessibleEntrance, including other values:_

```
GET https://api.reso.org/Property?$filter=AccessibilityFeatures/any(enum:enum eq org.reso.metadata.enums.AccessibilityFeatures'AccessibleEntrance')
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=AccessibilityFeatures/any(enum:enum eq org.reso.metadata.enums.AccessibilityFeatures'AccessibleEntrance')",
  "value": [
    {
      "ListingKey": "a1",
      "BedroomsTotal": 3,
      "ListPrice": 100000.00,
      "StreetName": "Main",
      "ModificationTimestamp": "2020-04-02T02:02:02.02Z",
      "ListingContractDate": "2020-04-02",
      "StandardStatus": "ActiveUnderContract",
      "AccessibilityFeatures": ["AccessibleApproachWithRamp", "AccessibleEntrance", "Visitable"]
    }
  ]
}
```

<br />

**`all()` Operator**

> The `all` operator applies a Boolean expression to each member of a collection and returns `true` if the expression is true for all members of the collection, otherwise it returns `false`. This implies that the `all` operator always returns `true` for an empty collection.
>
> The `all` operator cannot be used without an argument expression.

**Example**

_Find all listings with only the AccessibilityFeatures Visitable flag set:_

```
GET https://api.reso.org/Property?$filter=AccessibilityFeatures/all(enum:enum eq org.reso.metadata.enums.AccessibilityFeatures'Visitable')
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=AccessibilityFeatures/all(enum:enum eq org.reso.metadata.enums.AccessibilityFeatures'Visitable')",
  "value": [
    {
      "ListingKey": "a39",
      "BedroomsTotal": 3,
      "ListPrice": 100000.00,
      "StreetName": "Main",
      "ModificationTimestamp": "2020-04-02T02:02:02.02Z",
      "ListingContractDate": "2020-04-02",
      "StandardStatus": "ActiveUnderContract",
      "AccessibilityFeatures": ["Visitable"]
    }
  ]
}
```

<br />

##### 2.5.9.9.3 Collection of `Edm.String`

Support for string-based enumerations was added in Web API Core through the use of the Lookup resource, [as outlined in RCP-032](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2275152879).

This resource is still in DRAFT status. Please [contact RESO](mailto:dev@reso.org) if you are interested in being certified using `Edm.String` lookups.

<br />

## 2.6 Response Codes and Error Message Bodies
This section describes expected response codes and error message bodies.

<br />

### 2.6.1 HTTP Response Codes
A compatible server implementation MUST return a valid HTTP status code for each request indicating the status of the request.

If the response was not successful the server MAY include an [error message](https://reso.atlassian.net/wiki/spaces/WebAPI2/pages/8163003779) in the body of the HTTP response. There is a defined response body for JSON but there is no explicit requirement in the OData standard.

| Code | Short Description | Detail |
| ---- | ----------------- | ------ |
| 200 | OK | Returned by GET method when retrieving a record or records. If no records are found an empty result set is returned. |
| 202 | Accepted | Returned by GET method to indicate that the server received the request but that it may take time to fulfill a response. |
| 400 | Bad Request | Returned by GET method calls when the data fails validation and more detail on the error may be found in the body of the response. |
| 403 | Forbidden | Returned when the selected Authentication mechanism is not successful. |
| 404 | Not Found | Returned when a GET cannot find a resource or collection. |
| 413 | Request Entity Too Large | Returned at the discretion of the server. Used to indicate when the server cannot handle the complexity of the specific request. |
| 415 | Unsupported Media | Returned when a media format requested is not supported by the system. |
| 429 | Too Many Requests | Returned at the discretion of the server. Used to indicate that the user / licensee has met or exceeded their allowed usage (transactions per second, per day, per month, etc. |
| 500 | Internal Server Error | Returned when an unexpected error is encountered and more detail may be provided in the response body. |
| 501 | Not Implemented | Returned when the requested method is not available. |

<br />

### 2.6.2 Error Message Bodies
When the client makes a request which cannot be satisfied or produces an error condition, a compliant server MUST follow the OData error handling guidelines.

Full details of this mechanism may be found in the [JSON Error Response](http://docs.oasis-open.org/odata/odata-json-format/v4.0/errata03/os/odata-json-format-v4.0-errata03-os-complete.html#_Toc453766668) section of the OData specification.

The following example includes a client request and a compliant server error response for reference:

```
GET https://api.reso.org/reso/odata/Member?$orderby=ModificationTimestamp&$top=5&$skip=5
HTTP/2 200 OK
```
```json
{
  "error": {
    "code": "501",
    "message": "Unsupported functionality",
    "target": "query",
    "details": [
      {
       "code": "501",
       "target": "$skip", 
       "message": "Resource does not support the $skip parameter"
      }
    ],
    "innererror": {
      "trace": [],
      "context": {}
    } 
  }
}
```

<br />

## 2.7 Standard Resources

In general, the expectation is for the RESO Web API to output data as per the RESO Data Dictionary when a given data element is defined.

As of Web API 1.0.2+ and Data Dictionary 1.6+, at least ONE of the following Data Dictionary resources MUST be present for a server to be considered compliant for Certification:
* Property - a _Property_ resource [based on the RESO Data Dictionary Specification](https://ddwiki.reso.org/display/DDW17/Property+Resource).
* Member - a _Member_ resource [based on the RESO Data Dictionary Specification](https://ddwiki.reso.org/display/DDW17/Member+Resource).
* Office - an _Office_ resource [based on the RESO Data Dictionary Specification](https://ddwiki.reso.org/display/DDW17/Office+Resource).
* Media - a _Media_ resource [based on the RESO Data Dictionary Specification](https://ddwiki.reso.org/display/DDW17/Media+Resource).
* InternetTracking - an _InternetTracking_ resource [based on the RESO Data Dictionary Specification](https://ddwiki.reso.org/display/DDW17/InternetTracking+Resource).

_**Note**: Additional standard resources may be added to this list in future versions of the specification._

Servers MAY support more than one version of the RESO Data Dictionary and may also define additional resources to support specific use cases. For example, a server could provide a "Mobile" resource that returns a condensed list of fields to reduce the size of a response. However, if this resource contains standard Property resource fields, the Property resource MUST also be available on the given server during Certification even if it's not visible in production for certain roles.

Servers MAY support local resources, fields, or lookups that don't follow the RESO Data Dictionary specifications, and may extend any of the existing standard resources or lookups with their own localized values, except where otherwise noted. For instance, `StandardStatus` is a closed enumeration in the Property resource and may not be extended. 

<br />

## 2.8 Core Query Examples
The following examples show how Core queries can be used to query data on a given RESO Web API server.

<br />

**Get Properties Listed in December of 2020**
```
GET https://api.reso.org/Property?$filter=ListingContractDate ge 2020-12-01 and ListingContractDate lt 2021-01-01
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=ListingContractDate ge 2020-12-01 and ListingContractDate lt 2021-01-01",
  "value": [
    {
      "ListingKey": "a1",
      "BedroomsTotal": 5,
      "ListPrice": 100000.00,
      "StreetName": "Main",
      "ModificationTimestamp": "2020-04-02T02:02:02.02Z",
      "ListingContractDate": "2020-04-02",
      "StandardStatus": "ActiveUnderContract",
      "AccessibilityFeatures": ["AccessibleApproachWithRamp", "AccessibleEntrance", "Visitable"]
    }
  ]
}
```

<br />

**Get Properties Listed in a Given Year**
```
GET https://api.reso.org/Property?$filter=ListingContractDate ge 2020-01-01 and ListingContractDate lt 2021-01-01
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=ListingContractDate ge 2020-01-01 and ListingContractDate lt 2021-01-01",
  "value": [
    {
      "ListingKey": "b2",
      "BedroomsTotal": 3,
      "ListPrice": 200000.00,
      "StreetName": "2nd",
      "ModificationTimestamp": "2020-12-31T00:01:01.01.007Z",
      "ListingContractDate": "2020-05-25",
      "StandardStatus": "Closed",
      "AccessibilityFeatures": []
    }
  ]
}
```

<br />

**Get Active Members with First Name 'James' or 'Adam'**
```
GET https://api.reso.org/Member?$filter=MemberStatus eq 'Active' and (MemberFirstName eq 'James' or MemberFirstName eq 'Adam')
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Member?$filter=MemberStatus eq 'Active' and (MemberFirstName eq 'James' or MemberFirstName eq 'Adam')",
  "value": [
    {
      "MemberKey": "a1",
      "MemberStatus": "Active",
      "MemberFirstName": "James",
      "MemberLastName": "Smith",
      "ModificationTimestamp": "2021-08-21T00:01:01.01.007Z"
    }
     {
      "MemberKey": "b2",
      "MemberStatus": "Active",
      "MemberFirstName": "Adam",
      "MemberLastName": "Smith",
      "ModificationTimestamp": "2021-08-21T00:01:01.01.007Z"
    }
  ]
}
```

<br />

**Query on Boolean Field to Find Short Sales**
```
GET https://api.reso.org/Property?$filter=ShortSale eq true
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=ShortSale eq true",
  "value": [
    {
      "ListingKey": "a1",
      "BedroomsTotal": 5,
      "ListPrice": 100000.00,
      "StreetName": "Main",
      "ModificationTimestamp": "2020-04-02T02:02:02.02Z",
      "ListingContractDate": "2020-04-02",
      "ShortSale": true,
      "StandardStatus": "ActiveUnderContract",
      "AccessibilityFeatures": ["AccessibleApproachWithRamp", "AccessibleEntrance", "Visitable"]
    }
  ]
}
```

<br />

**Combine Multiple Criteria in a Listing Search**
```
GET https://api.reso.org/Property?$filter=ListPrice gt 250000 and ListPrice lt 500000
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=ListPrice gt 250000 and ListPrice lt 500000",
  "value": [
    {
      "ListingKey": "c3",
      "BedroomsTotal": 5,
      "ListPrice": 400000.00,
      "StreetName": "Main",
      "ModificationTimestamp": "2020-04-02T02:02:02.02Z",
      "ListingContractDate": "2020-04-02",
      "StandardStatus": "Active",
      "AccessibilityFeatures": ["Visitable"]
    }
  ]
}
```

<br />

**Get Properties with a Listing Price Greater Than $300K**
```
GET https://api.reso.org/Property?$filter=ListPrice gt 300000
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=ListPrice gt 300000",
  "value": [
    {
      "ListingKey": "c3",
      "BedroomsTotal": 5,
      "ListPrice": 400000.00,
      "StreetName": "Main",
      "ModificationTimestamp": "2020-04-02T02:02:02.02Z",
      "ListingContractDate": "2020-04-02",
      "StandardStatus": "Active",
      "AccessibilityFeatures": ["Visitable"]
    }
  ]
}
```

<br />

**Get Properties with a Listing Price of $300K**
```
GET https://api.reso.org/Property?$filter=ListPrice eq 300000
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=ListPrice eq 300000",
  "value": [
    {
      "ListingKey": "d4",
      "BedroomsTotal": 4,
      "ListPrice": 300000.00,
      "StreetName": "Oak",
      "ModificationTimestamp": "2021-08-15T00:01:01.01.007Z",
      "StandardStatus": "Active",
      "AccessibilityFeatures": []
    }
  ]
}
```

<br />

**Retrieve Records in a Specific Order**

```
GET https://api.reso.org/Property?$filter=ListPrice lt 500000&$orderby=ListPrice desc
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=ListPrice lt 500000&$orderby=ListPrice desc",
  "value": [
    {
      "ListingKey": "e5",
      "BedroomsTotal": 5,
      "ListPrice": 499999,
      "StreetName": "7th",
      "ModificationTimestamp": "2021-06-25T00:01:01.01.007Z",
      "StandardStatus": "Active",
      "AccessibilityFeatures": []
    }
    {
      "ListingKey": "f6",
      "BedroomsTotal": 5,
      "ListPrice": 489000,
      "StreetName": "Maple",
      "ModificationTimestamp": "2021-06-25T00:01:01.01.007Z",
      "StandardStatus": "Active",
      "AccessibilityFeatures": []
    }
  ]
}
```

<br />

**Get a Count of Property Records**
```
GET https://api.reso.org/Property?$select=ListingKey,ModificationTimestamp&$top=1&$count=true
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$select=ListingKey,ModificationTimestamp&$top=1&$count=true",
  "@odata.count": 2,
  "value": [
    {
      "ListingKey": "a1",
      "ModificationTimestamp": "2020-04-02T02:02:02.02Z" 
    }
  ]
}
```

<br />


**Get Top Five Residential Properties**
```
GET https://api.reso.org/Property?$filter=PropertyType eq 'Residential'&$top=5
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=PropertyType eq 'Residential'&$top=5",
  "value": [
    {
      "ListingKey": "a1",
      "BedroomsTotal": 5,
      "ListPrice": 100000.00,
      "StreetName": "Main",
      "ModificationTimestamp": "2020-04-02T02:02:02.02Z",
      "ListingContractDate": "2020-04-02",
      "StandardStatus": "ActiveUnderContract",
      "AccessibilityFeatures": ["AccessibleApproachWithRamp", "AccessibleEntrance", "Visitable"]
    }
    {
    "ListingKey": "b2",
    "BedroomsTotal": 5,
    "ListPrice": 400000.00,
    "StreetName": "Main",
    "ModificationTimestamp": "2020-04-02T02:02:02.02Z",
    "ListingContractDate": "2020-04-02",
    "StandardStatus": "Active",
    "AccessibilityFeatures": ["Visitable"]
    }
    {
    "ListingKey": "c3",
    "BedroomsTotal": 4,
    "ListPrice": 300000.00,
    "StreetName": "Oak",
    "ModificationTimestamp": "2021-08-15T00:01:01.01.007Z",
    "StandardStatus": "Active",
    "AccessibilityFeatures": []
    }
    {
    "ListingKey": "d4",
    "BedroomsTotal": 5,
    "ListPrice": 499999,
    "StreetName": "7th",
    "ModificationTimestamp": "2021-06-25T00:01:01.01.007Z",
    "StandardStatus": "Active",
    "AccessibilityFeatures": []
    }
    {
    "ListingKey": "e5",
    "BedroomsTotal": 5,
    "ListPrice": 489000,
    "StreetName": "Maple",
    "ModificationTimestamp": "2021-06-25T00:01:01.01.007Z",
    "StandardStatus": "Active",
    "AccessibilityFeatures": []
    }
  ]
}
  
```

<br />

**Get the First Five Members**
```
GET https://api.reso.org/Member?$top=5&$skip=0
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Member?$top=5&$skip=0",
  "value": [
       {
      "MemberKey": "a1",
      "MemberStatus": "Active",
      "MemberFirstName": "Angela",
      "MemberLastName": "Adams",
      "ModificationTimestamp": "2021-08-21T00:01:01.01.007Z"
    }
     {
      "MemberKey": "b2",
      "MemberStatus": "Active",
      "MemberFirstName": "Betty",
      "MemberLastName": "Adams",
      "ModificationTimestamp": "2021-08-21T00:01:01.01.007Z"
    }
    {
      "MemberKey": "c3",
      "MemberStatus": "Active",
      "MemberFirstName": "Henry",
      "MemberLastName": "Adams",
      "ModificationTimestamp": "2021-08-21T00:01:01.01.007Z"
    }
    {
      "MemberKey": "d4",
      "MemberStatus": "Active",
      "MemberFirstName": "Kevin",
      "MemberLastName": "Adams",
      "ModificationTimestamp": "2021-08-21T00:01:01.01.007Z"
    }
    {
      "MemberKey": "e5",
      "MemberStatus": "Active",
      "MemberFirstName": "Timothy",
      "MemberLastName": "Adams",
      "ModificationTimestamp": "2021-08-21T00:01:01.01.007Z"
    }
  ]
}
```

<br />

**Get the Next Five Members**
```
GET https://api.reso.org//Member?$top=5&$skip=5
HTTP/2 200 OK
```
```json
{
  "@odata.context": "GET https://api.reso.org//Member?$top=5&$skip=5",
  "value": [
       {
      "MemberKey": "f6",
      "MemberStatus": "Active",
      "MemberFirstName": "James",
      "MemberLastName": "Smith",
      "ModificationTimestamp": "2021-08-21T00:01:01.01.007Z"
    }
     {
      "MemberKey": "g7",
      "MemberStatus": "Active",
      "MemberFirstName": "Adam",
      "MemberLastName": "Smith",
      "ModificationTimestamp": "2021-08-21T00:01:01.01.007Z"
    }
    {
      "MemberKey": "h8",
      "MemberStatus": "Active",
      "MemberFirstName": "Jennifer",
      "MemberLastName": "Smith",
      "ModificationTimestamp": "2021-08-21T00:01:01.01.007Z"
    }
    {
      "MemberKey": "i9",
      "MemberStatus": "Active",
      "MemberFirstName": "Kevin",
      "MemberLastName": "Smith",
      "ModificationTimestamp": "2021-08-21T00:01:01.01.007Z"
    }
    {
      "MemberKey": "j10",
      "MemberStatus": "Active",
      "MemberFirstName": "Theresa",
      "MemberLastName": "Smith",
      "ModificationTimestamp": "2021-08-21T00:01:01.01.007Z"
    }
  ]
}
```
_**Note:** The implementation of $top and $orderby is defined by the server and may restrict what values may be used in either option. A compliant client SHOULD use the $orderby query to sustain consistency between requests, however a compliant server is not required to guarantee consistent results between requests._

<br />

**Get Properties with a Listing Price of Less than $300K**
```
GET https://api.reso.org/Property?$filter=ListPrice lt 300000
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=ListPrice lt 300000",
  "value": [
    {
      "ListingKey": "a1",
      "BedroomsTotal": 5,
      "ListPrice": 100000.00,
      "StreetName": "Main",
      "ModificationTimestamp": "2020-04-02T02:02:02.02Z",
      "ListingContractDate": "2020-04-02",
      "StandardStatus": "ActiveUnderContract",
      "AccessibilityFeatures": ["AccessibleApproachWithRamp", "AccessibleEntrance", "Visitable"]
    }
  ]
}
```

<br />

**Get Properties with a Price Range of $250k to $500k**
```
GET https://api.reso.org/Property?$filter=ListPrice gt 250000 and ListPrice lt 500000
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$filter=ListPrice gt 250000 and ListPrice lt 500000",
  "value": [
    {
      "ListingKey": "b2",
      "BedroomsTotal": 4,
      "ListPrice": 300000.00,
      "StreetName": "Oak",
      "ModificationTimestamp": "2021-08-15T00:01:01.01.007Z",
      "StandardStatus": "Active",
      "AccessibilityFeatures": []
    }
  ]
}
```

<br />

**Select Specific Field Values**
```
GET https://api.reso.org/Member?$select=MemberLastName,MemberFirstName,MemberMlsId
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Member?$select=MemberLastName,MemberFirstName,MemberMlsId",
  "value": [
    {
      "MemberLastName": "Smith",
      "MemberFirstName": "James",
      "MemberMlsId": "JSMITH"
    }
  ]
}
```
_Note: All names in the `$select` option are case-sensitive to match the names of elements provided by the resource._

<br />

**Get Most Recent ListingKey and ModificationTimestamp in Descending Order**
```
GET https://api.reso.org/Property?$select=ListingKey,ModificationTimestamp&$orderby=ModificationTimestamp desc
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property?$select=ListingKey,ModificationTimestamp&$orderby=ModificationTimestamp desc",
  "value": [
    {
      "ListingKey": "b2",
      "ModificationTimestamp": "2021-08-15T00:01:01.01.007Z"
    }
    {
      "ListingKey": "a1",
      "ModificationTimestamp": "2020-04-02T02:02:02.02Z"
    }
  ]
}
```

<br />

**Get a Single Property Record**
```
GET https://api.reso.org/Property('a3')
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Property('a3')",
  "ListingKey": "a3",
  "BedroomsTotal": 3,
  "ListPrice": 200000,
  "StreetName": "3rd",
  "ModificationTimestamp": "2021-09-12T00:01:01.01.007Z",
  "StandardStatus": "Active",
  "AccessibilityFeatures": []
}
```
_**Note**: this is referred to as a ["Singleton Property"](http://docs.oasis-open.org/odata/odata-csdl-xml/v4.01/odata-csdl-xml-v4.01.html#_Toc38530395) in the OData specification. Notice how the return value of the item is located at the top level rathre than in a `value` array._


<br />

**Filter by Multiple Field Values**
```
GET https://api.reso.org/Member?$filter=MemberFirstName eq 'Joe' and MemberLastName eq 'Smith'
HTTP/2 200 OK
```
```json
{
  "@odata.context": "https://api.reso.org/Member?$filter=MemberFirstName eq 'Joe' and MemberLastName eq 'Smith'",
  "value": [
    {
      "MemberKey": "a1",
      "MemberStatus": "Active",
      "MemberFirstName": "Joe",
      "MemberLastName": "Smith",
      "ModificationTimestamp": "2020-02-21T00:01:01.01.007Z"
    }
  ]
}
```
_**Note**: Query strings MUST be URL encoded where appropriate by a compliant client._

<br />

## 2.9 Security

Servers MUST implement one of the following [OAuth2](https://oauth.net/2/) authentication methods to be compliant with the RESO Web API specification:

* [Bearer Tokens](https://oauth.net/2/bearer-tokens/)
* [Client Credentials](https://oauth.net/2/grant-types/client-credentials/)

**Note**: _The [Open ID Connect](https://openid.net/connect/) layer was previously supported by the RESO Web API. As of Web API 1.0.2, RESO only supports Bearer tokens and Client Credentials during Certification._

<br /><br />

# Section 3: Certification

## 3.1 Purpose 
The goal of the Web API 2.0.0 Core specification is to provide a common, stable set of authentication protocols and API functionality to meet the needs of the real estate industry, with the intent that the Core specification will rarely change going forward.

Endorsements will be used to provide additional functionality to the Core specification in a modular manner and treated as separate specifications with their own dependencies, one of which may or may not be a dependency on Web API Core.

This section will focus exclusively on the Web API Core specification.

## 3.2 Background
The RESO Web API provides an open standard for a RESTful, JSON-based API that's centered around the RESO Data Dictionary, with the ability to support local extension. At its core, the RESO Web API standard is based on a subset of the OData specification from OASIS.

The OData specification consists of the following:
* Metadata format.
* Query format and URL structure to support it.
* Response format and type system. 

Each of these items MUST be valid with respect to OData for a Web API server to be considered compliant.

Additionally, there are RESO requirements beyond those of OData. For instance, Web API Servers MUST expose at least one Property, Member, Office, Media, or InternetTracking Data Dictionary resource in order to be certified. 

There are also authentication requirements which, at the time of writing, are servers that MUST support OAuth2 Auth Tokens OR Client Credentials.

The Web API Core testing rules ensure that server metadata are compliant, the data types provided by the RESO Data Dictionary support a minimum set of query operations valid for their types, that the query and response format are correct, and that the results logically match the query that was being used.

## 3.3 Testing Framework
RESO Web API Core certification is provided by the RESO Commander. 

The RESO Commander is an open source, cross-platform Java library created by RESO that uses established community libraries, such as the Apache Olingo OData Client, XML parsers, and JSON Schema Validators, to provide a testing API.

Web API tests are written in a high-level testing language (DSL) called Gherkin. This is part of a Behavior Driven Development (BDD) platform called Cucumber that allows for the expression of testing workflows using a natural language that is intended to be accessible to business analysts, QA testers, and programmers alike.

A command-line interface has been provided during the initial development phase as an entry point into the testing API. This provides the environment used for certification, self-assessment, or even a test automation server in a continuous integration and deployment platform such as GitHub CI, Jenkins, Travis, or CircleCI to help prevent regressions in a RESO-certified codebase.

A graphical user interface is also available through popular and free Integrated Development Environment (IDE) plugins for IntelliJ and Eclipse. IDEs provide a superior testing platform, as they provide better informational messages and are able to run and debug the entire test suite or a given individual test. The availability of plugins saves significant time in testing, development, and certification. The level of community support is one of the reasons Cucumber was chosen as a testing platform.

## 3.4 Testing Methodology
### Configuring the Test Client
Configuration of the RESO Commander for Web API Certification involves providing a service endpoint, authentication, resource, and field information in a template that will be used during the automated testing process.

A blank [Web API Core template](https://github.com/RESOStandards/web-api-commander/blob/main/sample-web-api-server.core.1.0.2.resoscript) may be found in the root of the RESO Commander project. 

There is also a [sample template](https://github.com/RESOStandards/web-api-commander/blob/main/src/test/resources/mock.web-api-server.core.1.0.2.resoscript) used internally for acceptance testing of the Web API testing tool. The sample template provides a useful reference when filling out RESOScript files. 

Items marked as REQUIRED in the configuration file MUST be completed, but things like sample field values have already been provided and should be sufficient for testing. If not, they also may be changed.

### Metadata Request Using RESO Standard Authentication
When testing begins, an HTTP request is made to an applicant's given service location with either OAuth2 [Bearer Tokens](https://oauth.net/2/bearer-tokens/) or [Client Credentials](https://oauth.net/2/grant-types/client-credentials/). 

Both of these authentication strategies allow for data consumption to be machine automated so that additional interaction from a user isn't necessary during the authentication process. As such, the RESO Data Dictionary Commander can be used for automated testing. 

The metadata request is expected to function according to the OData specification in terms of [request](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#_Toc31358863) and [response](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#_Toc31358882) headers and response formats. 

RESO specifically uses the [XML version of OData metadata](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752500), which contains an Entity Data Model (EDM) and model definitions, and is often referred to as EDMX.

### Metadata Validation
#### Syntax Checking
Metadata returned from a RESO Web API server are checked for XML validity as well as validated against [Entity Data Model (EDM) and EDMX definitions published by OASIS](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/schemas/), the creators of the OData specification. If metadata are invalid for any reason, Data Dictionary testing will halt. 

#### Semantic Checking
After metadata syntax has been validated, declared data models are checked for correctness. 

For example, if a given server declares they support the RESO Property resource, then the RESO Commander will look for an OData EntityType definition for Property. If the underlying data model is not found, metadata validation will fail with a diagnostic message to help users understand why a given error occurred. Once the model is found, its field and enumeration definitions will be checked for correctness as well.

Another aspect of semantic checking is ensuring that all models have keys so they can be indexed, meaning that a data request can be made to the server by key. This is a basic requirement for fetching data from a server.

### RESO Certification
Certification of Web API servers consists mainly of ensuring that a core set of required query operations are supported in a manner adhering to the RESO and OData specifications, and that servers send the appropriate response for each query. These fields are preconfigured in a file, as mentioned in the [section on Configuration](#configuring-the-test-client), rather than sampled from the RESO Data Dictionary. 

Data Dictionary resources, fields, and enumerations MUST be used in the configuration of the testing tool for RESO Certification.

In addition to comparison operators, such as greater and less than for things like Integers and Timestamps, OData query operators such as `$select`, which allows the consumer to specify a list of fields to be returned in the payload, or `$top` which allows the consumer to specify the size of the result set. These are outlined in the next section.

## 3.5 Web API Core Testing Queries
The following queries are used during Web API Core testing. 

Each item links to its relevant acceptance test in the RESO Commander repository.

Sample queries assume that `https://api.reso.org/` is being used as the OData service root.

<br />

### Request and Validate OData XML Metadata
| Item | Details |
| -- | -- |
| **Id**| metadata-validation |
| **Description** | Request and Validate Server Metadata |
| **Sample Query** | ```GET https://api.reso.org/$metadata?$format=application/xml``` |
| **Section** | [2.5.1](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#251-metadata-request) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L11-L23) |
| **Notes** <img width=200px /> | See: [Metadata Validation](#metadata-validation) <img width=1000px /> |

<br />

### Service Document Request
| Item | Details |
| -- | -- |
| **Id** | service-document |
| **Description** | Request and validate OData service document |
| **Sample Query** | ```GET https://api.reso.org/``` |
| **Section** | [2.5.2](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#252-service-document-request) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L25-L31) |
| **Notes** <img width=200px /> | See: [OData service document request](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part1-protocol/odata-v4.0-errata03-os-part1-protocol-complete.html#_Toc453752280) <img width=1000px /> |

<br />

### Fetch by Key
| Item | Details |
| -- | -- |
| **Id** | fetch-by-key |
| **Description** | Allows Records to be retrieved by primary key. |
| **Sample Query** | ```GET https://api.reso.org/Property('12345')?$select=ListingKey``` |
| **Section** | [2.5.3](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#253-fetch-by-key) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L33-L41) |
| **Notes** <img width=200px /> | Data Indexability by Key Requirement. <img width=1000px /> |

<br />

### `$select` Query Option
| Item | Details |
| -- | -- |
| **Id** | select |
| **Description** | `$select` allows fields to be requested on an individual basis as part of a query. |
| **Sample Query** | ```GET https://api.reso.org/Property?$select=ListingKey,BedroomsTotal``` |
| **Section** | [2.5.4](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#254-select-operator) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L43-L52) |
| **Notes** <img width=200px /> | The `$select` list determines the "data shape" of the response for a given query. <img width=1000px /> |

<br />

### `$top` Query Option
| Item | Details |
| -- | -- |
| **Id** | top |
| **Description** | `$top` allows the client to request a specific number of records in a query. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5``` |
| **Section** | [2.5.5](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#255-top-operator) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L54-L64) |
| **Notes** <img width=200px /> | _None_ <img width=1000px /> |

<br />

### `$count` Query Option
| Item | Details |
| -- | -- |
| **Id** | top |
| **Description** | The `$count` system query option with a value of `true` specifies that the total  |count of items within a collection matching the request be returned along with the result. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=0&$count=true``` |
| **Section** | [2.5.6](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#256-count-operator) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L85-L93) |
| **Notes** <img width=200px /> | _None_ <img width=1000px /> |

<br />

### `$skip` Query Option
| Item | Details |
| -- | -- |
| **Id** | top |
| **Description** | `$top` allows the client to request a specific number of records in a query. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$skip=5``` |
| **Section** | [2.5.7](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#257-skip-operator) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L66-L83) |
| **Notes**  | Use `$top` and `$skip` in conjunction to page. |

<br />

### `$orderby` ISO 8601 Timestamp Field Ascending
| Item | Details |
| -- | -- |
| **Id** | orderby-timestamp-asc |
| **Description** | `$orderby` allows results to be returned in a specified order. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=20&$select=ListingKey,BedroomsTotal,ModificationTimestamp&$orderby=ModificationTimestamp asc``` |
| **Section** | [2.5.8](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#258-orderby-operator) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L419-L429) |
| **Notes** <img width=200px /> | [More information](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752361) <img width=1000px /> |

<br />

### `$orderby` ISO 8601 Timestamp Field Descending
| Item | Details |
| -- | -- |
| **Id** | orderby-timestamp-desc |
| **Description** | `$orderby` allows results to be returned in a specified order. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=20&$select=ListingKey,BedroomsTotal,ModificationTimestamp&$orderby=ModificationTimestamp desc``` |
| **Section** | [2.5.8](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#258-orderby-operator) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L431-L441) |
| **Notes** <img width=200px /> | [More information](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752361) <img width=1000px /> |

<br />

### `$orderby` ISO 8601 Timestamp Field Ascending with Integer `gt` Filter
| Item | Details |
| -- | -- |
| **Id** | orderby-timestamp-asc-filter-int-gt |
| **Description** | `$orderby` allows results to be returned in a specified order. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=20&$select=ListingKey,BedroomsTotal,ModificationTimestamp&$orderby=ModificationTimestamp asc&$filter=BedroomsTotal gt 3``` |
| **Section** | [2.5.8](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#258-orderby-operator) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L448-L458)  |
| **Notes** <img width=200px /> | [More information](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752361) <img width=1000px /> |

<br />

### `$orderby` ISO 8601 Timestamp Field Descending with Integer `gt` Filter
| Item | Details |
| -- | -- |
| **Id** | orderby-timestamp-desc-filter-int-gt |
| **Description** | `$orderby` allows results to be returned in a specified order. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=20&$select=ListingKey,BedroomsTotal,ModificationTimestamp&$orderby=ModificationTimestamp desc&$filter=BedroomsTotal gt 3``` |
| **Section** | [2.5.8](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#258-orderby-operator) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L460-L470) |
| **Notes** <img width=200px /> | [More information](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752361) <img width=1000px /> |

<br />

### Filter Integer Field Using `and` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-int-and |
| **Description** | `$filter` with `and` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,BedroomsTotal&$filter=BedroomsTotal gt 3 and BedroomsTotal lt 10``` |
| **Section** | [2.5.9.1](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2591-odata-primitive-types) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L99-L109) |
| **Notes** <img width=200px /> | [See OData 5.1.1.1.7](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter Integer Field Using `or` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-int-or |
| **Description** | `$filter` with `or` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,BedroomsTotal&$filter=BedroomsTotal lt 10 or BedroomsTotal gt 3``` |
| **Section** | [2.5.9.1](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2591-odata-primitive-types) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L111-L121) |
| **Notes** <img width=200px /> | [See OData 5.1.1.1.8](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter Integer Field Using `not ()` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-int-not |
| **Description** | `$filter` with `not` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,BedroomsTotal&$filter=not (BedroomsTotal le -1)``` |
| **Section** | [2.5.9.1](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2591-odata-primitive-types) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L123-L133) |
| **Notes** <img width=200px /> | [See OData 5.1.1.1.9](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter Integer Field Using `eq` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-int-eq |
| **Description** | `$filter` with `eq` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,BedroomsTotal&$filter=BedroomsTotal eq 3``` |
| **Section** | [2.5.9.2](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2592-equals) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L135-L145) |
| **Notes** <img width=200px /> | [See OData 5.1.1.1.1](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter Integer Field Using `ne` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-int-ne |
| **Description** | `$filter` with `ne` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,BedroomsTotal&$filter=BedroomsTotal ne 3``` |
| **Section** | [2.5.9.3](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2593-not-equals) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L147-L157) |
| **Notes** <img width=200px /> | [See OData 5.1.1.1.2](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter Integer Field Using `gt` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-int-gt |
| **Description** | `$filter` with `gt` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,BedroomsTotal&$filter=BedroomsTotal gt 3``` |
| **Section** | [2.5.9.4](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2594-greater-than) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L159-L169) |
| **Notes** <img width=200px /> | [See OData 5.1.1.1.3](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter Integer Field Using `ge` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-int-ge |
| **Description** | `$filter` with `ge` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,BedroomsTotal&$filter=BedroomsTotal ge 3``` |
| **Section** | [2.5.9.5](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2595-greater-than-or-equal) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L171-L181) |
| **Notes** <img width=200px /> | [See OData 5.1.1.1.4](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter Integer Field Using `lt` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-int-lt |
| **Description** | `$filter` with `lt` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,BedroomsTotal&$filter=BedroomsTotal lt 3``` |
| **Section** | [2.5.9.6](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2596-less-than) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L183-L193) |
| **Notes** <img width=200px /> | [See OData 5.1.1.1.5](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter Integer Field Using `le` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-int-le |
| **Description** | `$filter` with `le` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,BedroomsTotal&$filter=BedroomsTotal le 3``` |
| **Section** | [2.5.9.7](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2597-less-than-or-equal) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L195-L205) |
| **Notes** <img width=200px /> | [See OData 5.1.1.1.6](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter Decimal Field Using `ne` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-decimal-ne |
| **Description** | `$filter` with `ne` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ListPrice&$filter=ListPrice ne 0.00``` |
| **Section** | [2.5.9.3](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2593-not-equals) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L212-L222) |
| **Notes** <img width=200px /> | [See OData 5.1.1.1.2](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter Decimal Field Using `gt` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-decimal-gt |
| **Description** | `$filter` with `gt` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ListPrice&$filter=ListPrice gt 0.00``` |
| **Section** | [2.5.9.4](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2594-greater-than) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L224-L234) |
| **Notes** <img width=200px /> | [See OData 5.1.1.1.4](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter Decimal Field Using `ge` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-decimal-ge |
| **Description** | `$filter` with `ge` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ListPrice&$filter=ListPrice ge 0.00``` |
| **Section** | [2.5.9.5](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2595-greater-than-or-equal) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L236-L246) |
| **Notes** <img width=200px /> | [See OData 5.1.1.1.5](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter Decimal Field Using `lt` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-decimal-lt |
| **Description** | `$filter` with `lt` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ListPrice&$filter=ListPrice lt 1234567.89``` |
| **Section** | [2.5.9.6](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2596-less-than) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L248-L258) |
| **Notes** <img width=200px /> | [See OData 5.1.1.1.3](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter Decimal Field Using `le` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-decimal-le |
| **Description** | `$filter` with `le` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ListPrice&$filter=ListPrice le 1234567.89``` |
| **Section** | [2.5.9.7](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2597-less-than-or-equal) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L260-L270) |
| **Notes** <img width=200px /> | [See OData 5.1.1.1.4](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter ISO 8601 Date Using `eq` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-date-eq |
| **Description** | `$filter` [ISO 8601 date](https://en.wikipedia.org/wiki/ISO_8601) in YYYY-MM-DD format with `eq` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ListingContractDate&$filter=ListingContractDate eq 2019-12-31``` |
| **Section** | [2.5.9.2](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2592-equals) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L277-L287) |
| **Notes** <img width=200px /> | [See OData 5.1.1.6.1](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter ISO 8601 Date Using `ne` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-date-ne |
| **Description** | `$filter` [ISO 8601 date](https://en.wikipedia.org/wiki/ISO_8601) in YYYY-MM-DD format with `ne` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ListingContractDate&$filter=ListingContractDate ne 2019-12-31``` |
| **Section** | [2.5.9.3](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2593-not-equals) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L289-L299) |
| **Notes** <img width=200px /> | [See OData 5.1.1.6.1](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter ISO 8601 Date Using `gt` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-date-gt |
| **Description** | `$filter` [ISO 8601 date](https://en.wikipedia.org/wiki/ISO_8601) in YYYY-MM-DD format with `gt` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ListingContractDate&$filter=ListingContractDate gt 2019-12-31``` |
| **Section** | [2.5.9.4](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2594-greater-than) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L301-L311) |
| **Notes** <img width=200px /> | [See OData 5.1.1.6.1](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter ISO 8601 Date Using `ge` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-date-ge |
| **Description** | `$filter` [ISO 8601 date](https://en.wikipedia.org/wiki/ISO_8601) in YYYY-MM-DD  format with `ge` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ListingContractDate&$filter=ListingContractDate ge 2019-12-31``` |
| **Section** | [2.5.9.5](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2595-greater-than-or-equal) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L313-L323) |
| **Notes** <img width=200px /> | [See OData 5.1.1.6.1](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter ISO 8601 Date Using `lt` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-date-lt |
| **Description** | `$filter` [ISO 8601 date](https://en.wikipedia.org/wiki/ISO_8601) in YYYY-MM-DD  format with `lt` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ListingContractDate&$filter=ListingContractDate lt 2019-12-31``` |
| **Section** | [2.5.9.6](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2596-less-than) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L325-L335) |
| **Notes** <img width=200px /> | [See OData 5.1.1.6.1](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter ISO 8601 Date Using `le` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-date-le |
| **Description** | `$filter` [ISO 8601 date](https://en.wikipedia.org/wiki/ISO_8601) in YYYY-MM-DD format with `le` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ListingContractDate&$filter=ListingContractDate le 2019-12-31``` |
| **Section** | [2.5.9.7](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2597-less-than-or-equal) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L337-L347) |
| **Notes** <img width=200px /> | [See OData 5.1.1.6.1](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter ISO 8601 Timestamp Using `ne` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-datetime-ne |
| **Description** | `$filter` [ISO 8601 timestamp](https://en.wikipedia.org/wiki/ISO_8601) in YYYY-MM-DD format with `ne` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ModificationTimestamp&$filter=ModificationTimestamp ne 2019-12-31T23:55:55-09:00``` |
| **Section** | [2.5.9.3](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2593-not-equals) |
| **Acceptance Test** | [Source]()  |
| **Notes** <img width=200px /> | [See OData 5.1.1.6.11](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter ISO 8601 Timestamp Using `gt` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-datetime-gt |
| **Description** | `$filter` [ISO 8601 timestamp](https://en.wikipedia.org/wiki/ISO_8601) in YYYY-MM-DD format with `gt` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ModificationTimestamp&$filter=ModificationTimestamp gt 2019-12-31T23:55:55-09:00``` |
| **Section** | [2.5.9.4](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2594-greater-than) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L354-L364) |
| **Notes** <img width=200px /> | [See OData 5.1.1.6.11](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter ISO 8601 Timestamp Using `ge` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-datetime-ge |
| **Description** | `$filter` [ISO 8601 timestamp](https://en.wikipedia.org/wiki/ISO_8601) in YYYY-MM-DD format with `ge` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ModificationTimestamp&$filter=ModificationTimestamp ge 2019-12-31T23:55:55-09:00``` |
| **Section** | [2.5.9.5](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2595-greater-than-or-equal) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L366-L376) |
| **Notes** <img width=200px /> | [See OData 5.1.1.6.11](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter ISO 8601 Timestamp Using `lt` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-datetime-lt |
| **Description** | `$filter` [ISO 8601 timestamp](https://en.wikipedia.org/wiki/ISO_8601) in YYYY-MM-DD format with `lt` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ModificationTimestamp&$filter=ModificationTimestamp lt 2020-12-31T23:55:55-09:00``` |
| **Section** | [2.5.9.6](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2596-less-than) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L378-L388) |
| **Notes** <img width=200px /> | [See OData 5.1.1.6.11](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter ISO 8601 Timestamp Using `le` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-datetime-le |
| **Description** | `$filter` [ISO 8601 timestamp](https://en.wikipedia.org/wiki/ISO_8601) in YYYY-MM-DD format with `le` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ModificationTimestamp&$filter=ModificationTimestamp le 2020-12-31T23:55:55-09:00``` |
| **Section** | [2.5.9.7](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2597-less-than-or-equal) |
| **Acceptance Test** | [Source]() |
| **Notes** <img width=200px /> | [See OData 5.1.1.6.11](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter ISO 8601 Timestamp Using `ne` Logical Operator and `now()` 
| Item | Details |
| -- | -- |
| **Id** | filter-datetime-ne-now |
| **Description** | `$filter` [ISO 8601 timestamp](https://en.wikipedia.org/wiki/ISO_8601) in YYYY-MM-DD format with `ne` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ModificationTimestamp&$filter=ModificationTimestamp ne 2019-12-31T23:55:55-09:00``` |
| **Section** | [2.5.9.3](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2593-not-equals) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L402-L412) |
| **Notes** <img width=200px /> | [See OData 5.1.1.6.11](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />


### Filter ISO 8601 Timestamp Using `le` Logical Operator and `now()`
| Item | Details |
| -- | -- |
| **Id** | filter-datetime-lt-now |
| **Description** | `$filter` [ISO 8601 timestamp](https://en.wikipedia.org/wiki/ISO_8601) in YYYY-MM-DD format with `le` logical operator and the OData `now()` function. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ModificationTimestamp&$filter=ModificationTimestamp lt now()``` |
| **Section** | [2.5.9.6](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2596-less-than) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L378-L388) |
| **Notes** <img width=200px /> | [See OData 5.1.1.8.9](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_now) <img width=1000px /> |

<br />

### Filter ISO 8601 Timestamp Using LE Logical Operator and `now()`
| Item | Details |
| -- | -- |
| **Id** | filter-datetime-le-now |
| **Description** | `$filter` [ISO 8601 timestamp](https://en.wikipedia.org/wiki/ISO_8601) in YYYY-MM-DD format with `le` logical operator and the OData `now()` function. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,ModificationTimestamp&$filter=ModificationTimestamp lt now()``` |
| **Section** | [2.5.9.7](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#2597-less-than-or-equal) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L390-L400) |
| **Notes** <img width=200px /> | [See OData 5.1.1.8.9](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_now) <img width=1000px /> |

<br />

### Filter Single Enumeration Using `has` and OData `Edm.EnumType`
| Item | Details |
| -- | -- |
| **Id** | filter-enum-single-has |
| **Description**  | has` operator for `Edm.EnumType`. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,PropertyType&$filter=PropertyType has PropertyEnums.PropertyType'Residential'``` |
| **Section** | [2.5.9.8.1](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#25981-edmenumtype-enumerations) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L477-L487) |
| **Notes** <img width=200px /> | [More Information](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752566) <img width=1000px /> |

<br />

### Filter Single Enumeration Using `eq` and OData `Edm.EnumType`
| Item | Details |
| -- | -- |
| **Id** | filter-enum-single-eq |
| **Description**  | eq` operator for `Edm.EnumType`. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,PropertyType&$filter=PropertyType eq PropertyEnums.PropertyType'Residential'``` |
| **Section** | [2.5.9.8.1](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#25981-edmenumtype-enumerations) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L489-L497) |
| **Notes** <img width=200px /> | [More Information](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752566) <img width=1000px /> |

<br />

### Filter Single Enumeration Using `ne` and OData `Edm.EnumType`
| Item | Details |
| -- | -- |
| **Id** | filter-enum-single-ne |
| **Description**  | `ne` operator for `Edm.EnumType`. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,PropertyType&$filter=PropertyType ne PropertyEnums.PropertyType'Residential'``` |
| **Section** | [2.5.9.8.1](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#25981-edmenumtype-enumerations) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L499-L506) |
| **Notes** <img width=200px /> | [More Information](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752566) <img width=1000px /> |

<br />

### Filter Multiple Enumeration Using `has` and OData `Edm.EnumType`
| Item | Details |
| -- | -- |
| **Id** | filter-enum-multi-has |
| **Description**  | `has` operator for `Edm.EnumType` and `IsFlags=true`. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,Appliances&$filter=Appliances has PropertyEnums.Appliances'Refrigerator'``` |
| **Section** | [2.5.9.9.1](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#25991-odata-isflagstrue) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L513-L523) |
| **Notes** <img width=200px /> | [More Information](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752566) <img width=1000px /> |

<br />

### Filter Multiple Enumeration Using `has` and OData `Edm.EnumType` with `and` Logical Operator
| Item | Details |
| -- | -- |
| **Id** | filter-enum-multi-has-and |
| **Description**  | has` operator for `Edm.EnumType` and `IsFlags=true` with `and` logical operator. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,Appliances&$filter=Appliances has PropertyEnums.Appliances'Refrigerator' and Appliances has PropertyEnums.Appliances'Stacked'``` 
| **Section** | [2.5.9.9.1](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#25991-odata-isflagstrue) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L525-L536) |
| **Notes** <img width=200px /> | [More Information](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752566) <img width=1000px /> |

<br />

### Filter Multiple Enumeration Using `any` Lambda Operator and OData `Collection(Edm.EnumType)`
| Item | Details |
| -- | -- |
| **Id** | filter-coll-enum-any |
| **Description**  | `any` lambda for `Collection(Edm.EnumType)`. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,Appliances&$filter=Appliances/any(enum:enum eq PropertyEnums.Appliances'Refrigerator')``` |
| **Section** | [2.5.9.9.2](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#25992-collection-of-edmenumtype) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L543-L554) |
| **Notes** <img width=200px /> | [See OData 5.1.1.10.1](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### Filter Multiple Enumeration Using `all` Lambda Operator and OData `Collection(Edm.EnumType)`
| Item | Details |
| -- | -- |
| **Id** | filter-coll-enum-any |
| **Description**  | `any` lambda for `Collection(Edm.EnumType)`. |
| **Sample Query** | ```GET https://api.reso.org/Property?$top=5&$select=ListingKey,Appliances&$filter=Appliances/all(enum:enum eq PropertyEnums.Appliances'Refrigerator')` ``| 
| **Section** | [2.5.9.9.2](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#25992-collection-of-edmenumtype) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L556-L567) |
| **Notes** <img width=200px /> | [See OData 5.1.1.10.2](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/odata-v4.0-errata03-os-part2-url-conventions-complete.html#_Toc453752358) <img width=1000px /> |

<br />

### HTTP 400 Response Code Test
| Item | Details |
| -- | -- |
| **Id** | response-code-400 |
| **Description** | Issues query to trigger HTTP 400 response code. |
| **Sample Query** | ```GET https://api.reso.org/Property?$filter=BadField eq 'SoBad'``` |
| **Section** | [2.6.1](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#261-http-response-codes) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L575-L581) |
| **Notes** <img width=200px /> | _None_ <img width=1000px /> |

<br />

### HTTP 404 Response Code Test
| Item | Details |
| -- | -- |
| **Id** | response-code-404 |
| **Description** | Issues query to trigger HTTP 404 response code. |
| **Sample Query** | ```GET https://api.reso.org/ResourceNotFound``` |
| **Section** | [2.6.1](https://github.com/RESOStandards/reso-transport-specifications/blob/rcp-037-web-api-core-endorsement-2.0.0-major/WEB-API-CORE.md#261-http-response-codes) |
| **Acceptance Test** | [Source](https://github.com/RESOStandards/web-api-commander/blob/6ff35627926f6b25ce5a5ae737caa69967b3811d/src/main/java/org/reso/certification/features/web-api/web-api-server.core.feature#L583-L589) |
| **Notes** <img width=200px /> | _None_ <img width=1000px /> |

<br /><br />

# Section 4: Contributors

This document was written by [Joshua Darnell](mailto:josh@reso.org).

Thanks to the following contributors for their help with this project:

| Contributor | Company |
| --- | --- |
| Paul Stusiak | Falcon Technologies Corp. |
| Sergio Del Rio | Templates for Business, Inc. |
| Joshua Darnell | kurotek, LLC |
| Cody Gustafson | FBS Data Systems |
| Chris Lambrou | MetroMLS |
| Scott Petronis | Onboard Informatics |
| Matthew McGuire | CoreLogic |
| Fred Larsen | UtahRealEstate.com
| James McDaniel | UtahRealEstate.com |
| Robert Gottesman | RESO |
| Rob Larson | Larson Consulting, LLC |
| Paul Hethmon | Corelogic |
| Rick Trevino | MetroList |
| Pace Davis | Zillow Group |
| Michael Watt | Zillow Group |
| Geoff Rispin | Templates 4 Business, Inc. |
| Maria Dalarcao | MLSListings, Inc. |
| Jeremy Crawford | RESO |

Many thanks to those who contributed to the Web API Core specification, including volunteers from the Transport workgroup. 

<br /><br />

# Section 5: References
Please see the following references for more information regarding topics covered in this document.

| Description | Link |
| --- | --- |
| REST | [Representational State Transfer](https://en.wikipedia.org/wiki/Representational_state_transfer) |
| Open Data Protocol or "OData" | [OData - the Best Way to REST](https://www.odata.org/) |
| OData "4.0" | [Documentation · OData - the Best Way to REST](https://www.odata.org/documentation/) |
| OData "4.0" - Part 1 - Protocol | [OData Version 4.0. Part 1: Protocol Plus Errata 03](http://docs.oasis-open.org/odata/odata/v4.0/errata03/odata-v4.0-errata03-part1-protocol-complete.html) |
| OData "4.0" - Part 2 - URL Conventions | [OData Version 4.0. Part 2: URL Conventions Plus Errata 03](http://docs.oasis-open.org/odata/odata/v4.0/errata03/odata-v4.0-errata03-part2-url-conventions-complete.html) |
| OData "4.0" - Part 3 - Common Schema Definition Language | [OData Version 4.0. Part 3: Common Schema Definition Language (CSDL) Plus Errata 03](http://docs.oasis-open.org/odata/odata/v4.0/errata03/odata-v4.0-errata03-part3-csdl-complete.html) |
| Geospatial Support in OData | [Geospatial data support in OData · OData - the Best Way to REST](http://www.odata.org/blog/geospatial-data-support-in-odata/) |
| HTTP/1.1 Protocol | [Hypertext Transfer Protocol (HTTP/1.1): Message Syntax and Routing](https://tools.ietf.org/html/rfc7230) <br /> [Hypertext Transfer Protocol (HTTP/1.1): Semantics and Content](https://tools.ietf.org/html/rfc7231) <br /> [Hypertext Transfer Protocol (HTTP/1.1): Conditional Requests](https://tools.ietf.org/html/rfc7232) <br /> [Hypertext Transfer Protocol (HTTP/1.1): Range Requests](https://tools.ietf.org/html/rfc7233) <br /> [Hypertext Transfer Protocol (HTTP/1.1): Caching](https://tools.ietf.org/html/rfc7234) <br /> [Hypertext Transfer Protocol (HTTP/1.1): Authentication](https://tools.ietf.org/html/rfc7235) |
| HTTP/2.0 Protocol | [Hypertext Transfer Protocol Version 2 (HTTP/2)](https://tools.ietf.org/html/rfc7540) <br /> [HPACK: Header Compression for HTTP/2](https://tools.ietf.org/html/rfc7541)  |
| Transport Layer Security (TLS) (Encryption for HTTP support) | [The Transport Layer Security (TLS) Protocol Version 1.2](https://www.ietf.org/rfc/rfc5246.txt) <br /> [Recommendations fSecure Use of Transport Layer Security (TLS) and Datagram Transport Layer Security (DTLS)](https://tools.ietf.org/html/rfc7525) <br /> [OWASP TLS implementation guide](https://www.owasp.org/index.php/Transport_Layer_Protection_Cheat_Sheet) <br /> [SSL Labs TLS Deployment Best Practices](https://www.ssllabs.com/downloads/SSL_TLS_Deployment_Best_Practices.pdf) |

<br /><br />

# Section 6: Appendices

## Approved RCPs
The following RCPs are included in Web API Core 2.0.0:
* [RCP - WEBAPI-010 Add Update Functionality to Web API Specification](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2239399511)
* [RCP - WEBAPI-011 Child Order Action](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2239401081)
* [RCP - WEBAPI-013 Add Certification Rule Impact/Changes to Process](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2239399520)
* [RCP - WEBAPI-016 Depreciate Bit Map Enumerations and Utilize Collections for Enumerations](https://reso.atlassian.net/wiki/display/RESOWebAPIRCP/RCP+-+WEBAPI-016+Depreciate+Bit+Map+Enumerations+and+Utilize+Collections+for+Enumerations)
* [RCP - WEBAPI-017 Add The Internet Tracking Resource to RESO Web API v1.1 Specification](https://reso.atlassian.net/wiki/display/RESOWebAPIRCP/RCP+-+WEBAPI-017+Add+The+Internet+Tracking+Resource+to+RESO+Web+API+v1.1+Specification)
* [RCP - WEBAPI-018 User Federation Best Practices](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2250178745/RCP+-+WEBAPI-018+User+Federation+Best+Practices)
* [RCP - WebAPI-019 - Validation Expression in the WebAPI](https://reso.atlassian.net/wiki/display/RESOWebAPIRCP/RCP+WebAPI-019+-+Validation+Expression+in+the+WebAPI)
* [RCP - WEBAPI-026 Change Default Certification Testing to Bearer Token](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2259388362/RCP+-++WEBAPI-026+Change+Default+Certification+Testing+to+Bearer+Token)
* [RCP - WEBAPI-029 Revise Web API Certification Procedures](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2275148134/RCP+-++WEBAPI-029+Revise+Web+API+Certification+Procedures)
* [RCP - WEBAPI-031 Data Dictionary Representation in the Web API](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2275149854/RCP+-+WEBAPI-031+Data+Dictionary+Representation+in+the+Web+API)

<br /><br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO](mailto:info@reso.org) if you have any questions.