# RESO Web API Add/Edit Endorsement

| **RCP** | 10 |
| :--- | :--- |
| **Version** | 2.0.0 |
| **Authors** | Sergio Del Rio ([T4Bi](mailto:Sergio.Del.Rio@t4bi.com))<br />[Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org)) |
| **Status** | **RATIFIED** |
| **Date Approved** | December 2024 |
| **Dependencies** | [Data Dictionary 1.7+](./data-dictionary.md)<br />[Web API 2.0.0+](./web-api-core.md) |
| **Related Links** | [DD Wiki 1.7](https://ddwiki.reso.org/display/DDW17/RESO+Data+Dictionary+1.7)<br />[Data Dictionary Spreadsheet](https://docs.google.com/spreadsheets/d/1_59Iqr7AQ51rEFa7p0ND-YhJjEru8gY-D_HM1yy5c6w/edit?usp=sharing)<br /> |

The keywords "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and "OPTIONAL" in this document are to be interpreted as described in [RFC 2119](https://www.ietf.org/rfc/rfc2119.txt).


<br />

# Table of Contents
- [Introduction](#introduction)
- [Section 1: Purpose](#section-1-purpose)
- [Section 2: Specification](#section-2-specification)
- [Section 3: Certification](#section-3-certification)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: Appendix](#section-6-appendix)
- [Section 7: License](#section-7-license)

<br />

# Introduction
The Web API Add/Edit Endorsement defines how to create, update, or delete data in the RESO Web API. It also includes standard error message responses so interactive clients may inform users how to correct issues with business rules, for example.

This proposal does not provide formal business rule support. That is addressed in [Validation Expressions (RCP-019)](./web-api-validation-expression.md), which is compatible with this specification.


<br />

# Section 1: Purpose
Creating and editing data is an important part of the listing transaction and primarily done through a UI. In order to allow for interactive clients, _data producers_ need to be able to perform certain actions. 

There are also other cases, such as creating Open Houses, Showings, or maintaining Member and Office data, where the ability to support such clients is useful.

The goal of this proposal is to provide a standard mechanism for create, update, and delete actions in the RESO Web API.

Add/Edit providers may implement as many or as few actions as makes sense for their business case, with one or more standard or local fields. 

When Data Dictionary elements are used, they will be recognized by the RESO Certification System.

<br />

# Section 2: Specification
The RESO Web API Add/Edit Endorsement consists of three possible actions:
* Create
* Update
* Delete

These correspond to the HTTP verbs `POST`, `PATCH`, and `DELETE`, respectively.

This specification also defines standard error responses.

## Authorization
Servers MUST implement one of the following [OAuth2 authorization methods](https://oauth.net/2/) to be compliant with the RESO Web API specification:
* [Bearer Tokens](https://oauth.net/2/bearer-tokens/)
* [Client Credentials](https://oauth.net/2/grant-types/client-credentials/)

OAuth2 is not part of OData. As such, servers and clients will need to implement support for it.

## Server Metadata
RESO Web API servers MUST advertise metadata in XML format using a `/$metadata` path relative to their service root.

The [OData Service Root](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#_Toc31360927) used in this document is assumed to be `https://api.reso.org`. 

As such, the XML metadata would be available at `https://api.reso.org/$metadata`. This will vary between providers and systems.

The examples in this document assume the following OData XML Metadata:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<edmx:Edmx Version="4.0" xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx">
  <edmx:DataServices>
    <Schema Namespace="org.reso.metadata" xmlns="http://docs.oasis-open.org/odata/ns/edm">
      <EntityType Name="Property">
        <Key>
          <PropertyRef Name="ListingKey"/>
        </Key>
        <Property Name="ListingKey" Type="Edm.String" MaxLength="255"/>
        <Property Name="ListPrice" Type="Edm.Decimal" Precision="14" Scale="2"/>
        <Property Name="BedroomsTotal" Type="Edm.Int64"/>
        <Property Name="BathroomsTotalInteger" Type="Edm.Int64"/>
        <Property Name="StandardStatus" Type="Edm.String">
          <Annotation Term="RESO.OData.Metadata.LookupName" String="StandardStatus"/>
        </Property>
        <Property Name="AccessibilityFeatures" Type="Collection(Edm.String)">
          <Annotation Term="RESO.OData.Metadata.LookupName" String="AccessibilityFeatures"/>
        </Property>
        <Property Name="ModificationTimestamp" Precision="27" Type="Edm.DateTimeOffset"/>
      </EntityType>
      <EntityType Name="Lookup">
        <Key>
          <PropertyRef Name="LookupKey"/>
        </Key>
        <Property Name="LookupKey" Type="Edm.String" Nullable="false"/>
        <Property Name="LookupName" Type="Edm.String" Nullable="false"/>
        <Property Name="LookupValue" Type="Edm.String" Nullable="false"/>
        <Property Name="StandardLookupValue" Type="Edm.String"/>
        <Property Name="LegacyODataValue" Type="Edm.String"/>
        <Property Name="ModificationTimestamp" Precision="27" Type="Edm.DateTimeOffset" Nullable="false"/>
      </EntityType>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>
```

Note that the _Lookup Resource_ is used in the examples above, so single- and multi-valued enumerations have types `Edm.String` and `Collection(Edm.String)`.

## Responses and Representations
More than one response format is allowed for create (POST) and update (PATCH) actions. 
* Servers MAY support returning the representation of the updated record with the response, minimal responses with only headers and no payload, or both.
* Clients MAY request either response type with the use of the `Prefer` header. Note that the server may not support the given `Prefer` header, so clients should expect either `representation` or `minimal` responses.
* If the server supports returning the representation, then it MUST indicate this in the `Preference-Applied` header by showing that the preference was applied. 

_From the [OData "4.01" Specification](http://docs.oasis-open.org/odata/odata/v4.0/os/part1-protocol/odata-v4.0-os-part1-protocol.html#_Toc372793631)_:

> 8.2.8.7 Preference return=representation and return=minimal
> The return=representation and return=minimal preferences are defined in [HTTP-Prefer],
> 
>In OData, return=representation or return=minimal is defined for use with a POST, PUT, or PATCH Data Modification Request other than to a stream property, or to an Action Request. Specifying a preference of return=representation or return=minimal in a GET or DELETE request, or any request to a stream property, SHOULD return a 4xx Client Error.
>
>A preference of return=representation or return=minimal is allowed on an individual Data Modification Request or Action Request within a batch, subject to the same restrictions, but SHOULD return a 4xx Client Error if specified on the batch request itself.
>
>A preference of return=minimal requests that the service invoke the request but does not return content in the response. The service MAY apply this preference by returning 204 No Content in which case it MAY include a Preference-Applied response header containing the return=minimal preference.
>
>A preference of return=representation requests that the service invokes the request and returns the modified entity. The service MAY apply this preference by returning the successfully modified resource in the body of the response, formatted according to the rules specified for the requested format. In this case the service MAY include a Preference-Applied response header containing the return=representation preference.

## Error Responses
The [OData "4.01" specification](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#_Toc31358908) outlines the JSON format of the error response body as follows:

> The representation of an error response body is format-specific. It consists at least of the following information:
>
>·         code: required non-null, non-empty, language-independent string. Its value is a service-defined error code. This code serves as a sub-status for the HTTP error code specified in the response.
>
>·         message: required non-null, non-empty, language-dependent, human-readable string describing the error. The Content-Language header MUST contain the language code from [RFC5646] corresponding to the language in which the value for message is written.
>
>·         target: optional nullable, potentially empty string indicating the target of the error, for example, the name of the property in error.
>
>·         details: optional, potentially empty collection of structured instances with code, message, and target following the rules above.
>
>·         innererror: optional structured instance with service-defined content.
> 
> Service implementations SHOULD carefully consider which information to include in production environments to guard against potential security concerns around information disclosure.

**Note**: RESO Certification requires the `details` array to be present in the error response, though optional in the OData specification above, in order to provide usability for interactive clients which need to know the affected fields and their corresponding error messages. The `target` and `message` fields are therefore also required in `details`.

Examples of the error response format are shown in the following sections.

## Create Action
This section outlines requests and responses for creating records.

### Create Action Succeeds
There are two successful request and response formats depending on whether the server supports returning the created record in the response body.

#### Create Action with `return=representation`
A client can request that the current representation of a record be returned upon creation. 

This is done by using the `Prefer: return=representation` request header.

**REQUEST**

```
POST https://api.reso.org/Property
  OData-Version: 4.01
  Content-Type: application/json
  Accept: application/json
  Prefer: return=representation
```

```json
{
 "ListPrice": 123456.00,
 "BedroomsTotal": 3,
 "BathroomsTotalInteger": 3,
 "AccessibilityFeatures": [
  "Accessible Approach with Ramp", "Accessible Entrance", "Visitable"
 ] 
}
```

**RESPONSE**

```
HTTP/2 201 Created
  OData-Version: 4.01
  EntityId: "12345"
  Location: https://api.reso.org/Property('12345')
  Content-Length: 566
  Content-Type: application/json
  Preference-Applied: return=representation
```

```json
{
  "@odata.context":"https://api.reso.org/$metadata#Property/$entity",
  "@odata.id":"https://api.reso.org/Property('12345')",
  "@odata.editLink":"https://api.reso.org/Property('12345')",
  "@odata.etag": "W/\"MjAxOC0wMS0yM1QwODo1Njo0NS4yMi0wODowMA==\"",
  "ListingKey": "12345",
  "ListPrice": 123456.00,
  "BedroomsTotal": 3,
  "BathroomsTotalInteger": 3,
  "AccessibilityFeatures": [
    "Accessible Approach with Ramp", "Accessible Entrance", "Visitable"
  ],
  "StandardStatus": "Coming Soon",
  "ModificationTimestamp": "2022-12-05T18:33:20Z"
}
```

#### Create Action with `return=minimal`
There may be cases where the representation isn't needed by the client or isn't supported by the server. 

**REQUEST**

```
POST https://api.reso.org/Property
  OData-Version: 4.01
  Content-Type: application/json
  Accept: application/json
  Prefer: return=minimal
```

```json
{
 "ListPrice": 123456.00,
 "BedroomsTotal": 3,
 "BathroomsTotalInteger": 3,
 "AccessibilityFeatures": [
  "Accessible Approach with Ramp", "Accessible Entrance", "Visitable"
 ] 
}
```

**RESPONSE**

```
HTTP/2 204 No Content
  OData-Version: 4.01
  EntityId: "12345"
  Location: https://api.reso.org/Property('12345')
  Content-Length: N
  Content-Type: application/json
  Preference-Applied: return=minimal
```

The response body is empty in this case.

<br />

### Create Action Fails
There is a single error response, regardless of representation. 

**REQUEST**

```
POST https://api.reso.org/Property
  OData-Version: 4.01
  Content-Type: application/json
  Accept: application/json
  Prefer: return=representation
```

```json
{
 "ListPrice": -123456.00,
 "BedroomsTotal": 3,
 "BathroomsTotalInteger": 3,
 "AccessibilityFeatures": [
  "Accessible Approach with Ramp", "Accessible Entrance", "Visitable"
 ] 
}
```

**RESPONSE**

```
HTTP/2 400 Bad Request
```

```json
{
  "error": {
    "code": "20100",
    "message": "Errors",
    "target": "Create",
    "details": [
      {
        "code": "30212",
        "target": "ListPrice",
        "message": "List Price must be greater than 0"
      }
    ]
  }
}
```

Note how the `target` field in the `details` array references the `ListPrice` in the Property Resource. It's important that providers follow this format in their error responses. Each item will be validated against the server metadata in order to make sure the item exists.

When the error is with an item in a collection, for example the second media object in a nested payload, it would be referenced by `Media[1].Category` (assuming the error was with the Category field). The RESO Data Dictionary defines standard relationships so that [Property has a collection of Media items](https://ddwiki.reso.org/display/DDW17/Media+Field) associated with it.

The `message` MUST be provided and SHOULD contain a user-friendly response that could be displayed in an application. 

RESO Certification will verify that there is a non-null, non-empty message, but will not validate the content.

## Update Action
This section shows requests and responses for updating records.

### Update Action Succeeds
There are two possible request and response formats when updating data, depending on whether returning the representation is supported on a given system.

#### Update Action with `return=representation`
A client can request that the current representation of a record be returned upon update. 

This is done with a request header value of `Prefer: return=representation`.


**REQUEST**

```
PATCH https://api.reso.org/Property('12345')
  OData-Version: 4.01
  Content-Type: application/json
  Accept: application/json
  If-Match: W/\"MjAxOC0wMS0yM1QwODo1Njo0NS4yMi0wODowMA==\"
  Prefer: return=representation
```

```json
{
  "ListPrice": 133456.00
}
```

**RESPONSE**

```
HTTP/2 200 OK
  EntityId: "12345"
  Location: https://api.reso.org/Property('12345')
  OData-Version: 4.01
  Content-Length: 566
  Content-Type: application/json;
  Preference-Applied: return=representation
```

```json
{
  "@odata.context":"https://api.reso.org/$metadata#Property/$entity",
  "@odata.id":"https://api.reso.org/Property('12345')",
  "@odata.editLink":"https://api.reso.org/Property('12345')",
  "@odata.etag": "W/\"ABCxOC0wMS0yM1QwODo1Njo0NS4yMi0wODowMA==\"",
  "ListingKey": "12345",
  "ListPrice": 133456.00,
  "BedroomsTotal": 3,
  "BathroomsTotalInteger": 3,
  "AccessibilityFeatures": [
    "Accessible Approach with Ramp", "Accessible Entrance", "Visitable"
  ],
  "StandardStatus": "Coming Soon",
  "ModificationTimestamp": "2022-12-05T18:33:20Z"
}
```

#### Update Action with `return=minimal`
Similar to create actions, there may be cases where the representation isn't needed by the client or isn't supported by the server.

The server MAY return a minimal response in these cases.

**REQUEST**

```
PATCH https://api.reso.org/Property('12345')
  OData-Version: 4.01
  Content-Type: application/json
  Accept: application/json
  If-Match: W/\"MjAxOC0wMS0yM1QwODo1Njo0NS4yMi0wODowMA==\"
  Prefer: return=minimal
```

```json
{
 "ListPrice": 133456.00,
}
```

**RESPONSE**

```
HTTP/2 204 No Content
  OData-Version: 4.01
  EntityId: "12345"
  Preference-Applied: return=minimal
```

There is no response body in this case, only the response headers shown above.

<br />

### Update Action Fails
There is a single error response regardless of the representation. 

**REQUEST**

```
PATCH https://api.reso.org/Property('12345')
  OData-Version: 4.01
  Content-Type: application/json
  Accept: application/json
  If-Match: W/\"MjAxOC0wMS0yM1QwODo1Njo0NS4yMi0wODowMA==\"
  Prefer: return=representation
```

```json
{
 "ListPrice": -133456.00,
}
```

**RESPONSE**

```
HTTP/2 400 Bad Request
```

```json
{
  "error": {
    "code": "20100",
    "message": "Errors",
    "target": "Update",
    "details": [
      {
        "code": "30212",
        "target": "ListPrice",
        "message": "List Price must be greater than 0"
      }
    ]
  }
}
```

Since the `target` of the first item in the `details` array is `ListPrice`, and the request was made to the Property Resource, this means the ListPrice field in the Property Resource had the error shown in `message`.


## Delete Action
The delete action allows for the deletion of records. Unlike the create and update actions, it does not support different representations.

### Delete Action Succeeds

**REQUEST**

```
DELETE https://api.reso.org/Property('12345')
  OData-Version: 4.01
  If-Match: W/\"MjAxOC0wMS0yM1QwODo1Njo0NS4yMi0wODowMA==\"
```

**RESPONSE**
```
HTTP/2 204 No Content
 OData-Version: 4.01
```

There is no response body in this case.

### Delete Action Fails

**REQUEST**

Let's assume the client tries to delete a record that doesn't exist.

```
DELETE https://api.reso.org/Property('12346')
  OData-Version: 4.01
  If-Match: W/\"MjAxOC0wMS0yM1QwODo1Njo0NS4yMi0wODowMA==\"
```

**RESPONSE**

The response would be as follows:

```
HTTP/2 404 Not Found
```

There are other possible error scenarios for `DELETE` actions as well. For example, the server does not support delete. 

The server MUST respond with the appropriate HTTP `4XX` status code when errors are encountered, and MAY decide whether to return a response body with the appropriate error message in these cases.

<br /><br />

# Section 3: Certification

For each create, update, or delete action there is a success and failure scenario.

Those being certified will provide known good and bad payloads that can trigger each of these responses. They will also indicate whether they support `return=minimal` or `return=representation` (or both) for each action being tested.

Since Add/Edit potentially changes data, it is not required that providers be tested on production servers. The expectation is that they will make these services available, however, and that they match what they're certified with.

Certification reports will show which actions and data elements were tested. Items matching the RESO Data Dictionary will be classified as such, but are not required. 

## Create Action Succeeds with `return=representation`

When the `Prefer: return=representation` request header is present, the testing rules are as follows:

```
POST https://api.reso.org/Property
  OData-Version: 4.01
  Content-Type: application/json
  Accept: application/json
  Prefer: return=representation
```

```gherkin
@create @create-succeeds @add-edit-endorsement @rcp-010 @2.1.0 @return-representation
Scenario: Create operation succeeds using a given payload
  Given valid metadata have been retrieved
  And request data has been provided in "create-succeeds.json"
  And request data in "create-succeeds.json" is valid JSON
  And schema in "create-succeeds.json" matches the metadata
  And the request header "OData-Version" "equals" one of the following values
    | 4.0 | 4.01 |
  And the request header "Content-Type" "contains" "application/json"
  And the request header "Accept" "contains" "application/json"
  And the `Prefer` request header is `return=representation`
  When a "POST" request is made to the "resource-endpoint" URL with data in "create-succeeds.json"
  Then the server responds with one of the following status codes
    | 201 | 204 |
  And the response header "OData-Version" "equals" one of the following values
    | 4.0 | 4.01 |
  And the response header "EntityId" "MUST" "be present" if the response code was 204
  And the response header "Location" "MUST" "be present"
  And the response header "Location" "is a valid URL"
  And the response header "Location" "MUST" reference the resource being edited
  And the `Preference-Applied` response header is `return=representation`
  And the response is valid JSON
  And the JSON response "MAY" contain "@odata.context"
  And the JSON response value "@odata.context" "is a valid URL"
  And the JSON response "MAY" contain "@odata.id"
  And the JSON response value "@odata.id" "is a valid URL"
  And the JSON response "MUST" contain "@odata.editLink"
  And the JSON response value "@odata.editLink" "is a valid URL"
  And the JSON response "MUST" contain all JSON data in "create-succeeds.json"
  When a "GET" request is made to the URL in response header "Location"
  Then the server responds with a status code of "200"
  And the response has header "OData-Version" with one of the following values
    | 4.0 | 4.01 |
  And the JSON response contains the data in "create-succeeds.json"
  And the JSON response matches the format advertised in the metadata
```

## Create Action Succeeds with `return=minimal`

The testing rules for the minimal response are as follows:

```
POST https://api.reso.org/Property
  OData-Version: 4.01
  Content-Type: application/json
  Accept: application/json
  Prefer: return=minimal
```

```gherkin
@create @create-succeeds @add-edit-endorsement @rcp-010 @2.1.0 @return-minimal
Scenario: Create operation succeeds using a given payload with a minimal response
  Given valid metadata have been retrieved
  And request data has been provided in "create-succeeds.json"
  And request data in "create-succeeds.json" is valid JSON
  And schema in "create-succeeds.json" matches the metadata
  And the request header "OData-Version" "equals" one of the following values
    | 4.0 | 4.01 |
  And the request header "Content-Type" "contains" "application/json"
  And the request header "Accept" "contains" "application/json"
  And the `Prefer` request header is `return=minimal`
  When a "POST" request is made to the "resource-endpoint" URL with data in "create-succeeds.json"
  Then the server responds with one of the following status codes
    | 201 | 204 |
  And the response header "OData-Version" "equals" one of the following values
    | 4.0 | 4.01 |
  And the response header "EntityId" "MUST" "be present" if the response code was 204
  And the response header "Location" "MUST" "be present"
  And the response header "Location" "is a valid URL"
  And the response header "Location" "MUST" reference the resource being edited
  And the `Preference-Applied` response header is `return=minimal`
  When a "GET" request is made to the URL in response header "Location"
  Then the server responds with a status code of "200"
  And the response has header "OData-Version" with one of the following values
    | 4.0 | 4.01 |
  And the JSON response contains the data in "create-succeeds.json"
  And the JSON response matches the format advertised in the metadata
```


## Create Action Fails

The error response is the same for the create action regardless if the client requested minimal or representation.

```
POST https://api.reso.org/Property
  OData-Version: 4.01
  Content-Type: application/json
  Accept: application/json
  Prefer: return=representation
```

```gherkin
@create @create-fails @add-edit-endorsement @rcp-010 @1.0.2
Scenario: Create operation fails using a given payload
  Given valid metadata have been retrieved
  And request data has been provided in "create-fails.json"
  And request data in "create-fails.json" is valid JSON
  And schema in "create-fails.json" matches the metadata
  And the request header "OData-Version" "equals" one of the following values
    | 4.0 | 4.01 |
  And the request header "Content-Type" "MUST" "be present"
  And the request header "Content-Type" "equals" "application/json"
  And the request header "Accept" "MUST" "be present"
  And the request header "Accept" "contains" "application/json"
  When a "POST" request is made to the "resource-endpoint" URL with data in "create-fails.json"
  Then the server responds with one of the following error codes
    | 400 |
  And the response has header "OData-Version" with one of the following values
    | 4.0 | 4.01 |
  And the error response is in a valid format
  And the values in the "target" field in the JSON payload "error.details" path are contained within the metadata
  And the values in the "message" field in the JSON payload "error.details" path have non-zero length
```


## Update Action Succeeds with `return=representation`

Similar to the create action, update supports a `Prefer` header of `return=minimal` or `return=representation`.

When the `Prefer: return=representation` request header is present, the testing rules are as follows:

```
PATCH https://api.reso.org/Property('12345')
  OData-Version: 4.01
  Content-Type: application/json
  Accept: application/json
  If-Match: W/\"MjAxOC0wMS0yM1QwODo1Njo0NS4yMi0wODowMA==\"
  Prefer: return=representation
```

```gherkin
@update @update-succeeds @add-edit-endorsement @rcp-010 @2.0.0 @return-representation
Scenario: Update operation succeeds using a given payload
  Given valid metadata have been retrieved
  And request data has been provided in "update-succeeds.json"
  And request data in "update-succeeds.json" is valid JSON
  And schema in "update-succeeds.json" matches the metadata
  And the request header "OData-Version" "equals" one of the following values
    | 4.0 | 4.01 |
  And the request header "Content-Type" "contains" "application/json"
  And the request header "Accept" "contains" "application/json"
  And the request header "Prefer" "contains" "return=representation"
  When a "PATCH" request is made to the "resource-endpoint" URL with data in "update-succeeds.json"
  Then the server responds with one of the following status codes
    | 200 | 204 |
  And the response header "OData-Version" "equals" one of the following values
    | 4.0 | 4.01 |
  And the response header "EntityId" "MUST" "be present" if the response code was 204
  And the response header "Location" "MUST" "be present"
  And the response header "Location" "is a valid URL"
  And the `Preference-Applied` response header is `return=representation`
  And the response header "Location" "MUST" reference the resource being edited
  And the response is valid JSON if the response code was 200
  And the JSON response "MUST" contain "@odata.etag"
  And the JSON response value "@odata.etag" "starts with" "W/"
  And the JSON response "MAY" contain "@odata.context"
  And the JSON response value "@odata.context" "is a valid URL"
  And the JSON response "MAY" contain "@odata.id"
  And the JSON response value "@odata.id" "is a valid URL"
  And the JSON response "MUST" contain "@odata.editLink"
  And the JSON response value "@odata.editLink" "is a valid URL"
  And the JSON response "MUST" contain all JSON data in "update-succeeds.json"
  When a "GET" request is made to the URL in response header "Location"
  Then the server responds with a status code of "200"
  And the response has header "OData-Version" with one of the following values
    | 4.0 | 4.01 |
  And the JSON response contains the data in "update-succeeds.json"
  And the JSON response matches the format advertised in the metadata

```

## Update Action Succeeds with `return=minimal`

When the `Prefer: return=minimal` request header is present, the testing rules are as follows:


```
PATCH https://api.reso.org/Property('12345')
  OData-Version: 4.01
  Content-Type: application/json
  Accept: application/json
  If-Match: W/\"MjAxOC0wMS0yM1QwODo1Njo0NS4yMi0wODowMA==\"
  Prefer: return=minimal
```

```gherkin
@update @update-succeeds @add-edit-endorsement @rcp-010 @2.0.0 @return-minimal
Scenario: Update operation succeeds using a given payload with a minimal response
  Given valid metadata have been retrieved
  And request data has been provided in "update-succeeds.json"
  And request data in "update-succeeds.json" is valid JSON
  And schema in "update-succeeds.json" matches the metadata
  And the request header "OData-Version" "equals" one of the following values
    | 4.0 | 4.01 |
  And the request header "Content-Type" "contains" "application/json"
  And the request header "Accept" "contains" "application/json"
  And the request header "Prefer" "contains" "return=minimal"
  When a "PATCH" request is made to the "resource-endpoint" URL with data in "update-succeeds.json"
  Then the server responds with one of the following status codes
    | 200 | 204 |
  And the response header "OData-Version" "equals" one of the following values
    | 4.0 | 4.01 |
  And the response header "EntityId" "MUST" "be present" if the response code was 204
  And the response header "Location" "MUST" "be present"
  And the response header "Location" "is a valid URL"
  And the response header "Location" "MUST" reference the resource being edited
  When a "GET" request is made to the URL in response header "Location"
  Then the server responds with a status code of "200"
  And the response has header "OData-Version" with one of the following values
    | 4.0 | 4.01 |
  And the JSON response contains the data in "update-succeeds.json"
  And the JSON response matches the format advertised in the metadata

```

## Update Action Fails

```
PATCH https://api.reso.org/Property('12345')
  OData-Version: 4.01
  Content-Type: application/json
  Accept: application/json
  If-Match: W/\"MjAxOC0wMS0yM1QwODo1Njo0NS4yMi0wODowMA==\"
  Prefer: return=representation
```

```gherkin
@update @update-fails @add-edit-endorsement @rcp-010 @1.0.2
Scenario: Update operation fails using a given payload
  Given valid metadata have been retrieved
  And request data has been provided in "update-fails.json"
  And request data in "update-fails.json" is valid JSON
  And schema in "update-fails.json" matches the metadata
  And the request header "OData-Version" "equals" one of the following values
    | 4.0 | 4.01 |
  And the request header "Content-Type" "MUST" "be present"
  And the request header "Content-Type" "equals" "application/json"
  And the request header "Accept" "MUST" "be present"
  And the request header "Accept" "contains" "application/json"
  When a "PATCH" request is made to the "resource-endpoint" URL with data in "update-fails.json"
  Then the server responds with one of the following error codes
    | 400 |
  And the response has header "OData-Version" with one of the following values
    | 4.0 | 4.01 |
  And the error response is in a valid format
  And the values in the "target" field in the JSON payload "error.details" path are contained within the metadata
  And the values in the "message" field in the JSON payload "error.details" path have non-zero length
```

## Delete Action Succeeds

Unlike the create and update actions, the delete action only supports one response format.

```
DELETE https://api.reso.org/Property('12345')
  OData-Version: 4.01
  If-Match: W/\"MjAxOC0wMS0yM1QwODo1Njo0NS4yMi0wODowMA==\"
```

```gherkin
@delete @update-succeeds @add-edit-endorsement @rcp-010 @2.0.0
Scenario: Delete operation succeeds using a given Url
  Given valid metadata have been retrieved
  And the request header "OData-Version" "equals" one of the following values
    | 4.0 | 4.01 |
  When a "DELETE" request is made to the "resource-endpoint" URL with data in "delete-succeeds.json"
  Then the server responds with one of the following status codes
    | 204 |
  And the response header "OData-Version" "equals" one of the following values
    | 4.0 | 4.01 |
  And the response is valid JSON
  And the JSON response "MUST" be empty
  When a "GET" request is made to the URL in response header "Location"
  Then the server responds with a status code of 404
  And the response has header "OData-Version" with one of the following values
    | 4.0 | 4.01 |
```

## Delete Action Fails

```
DELETE https://api.reso.org/Property('12346')
  OData-Version: 4.01
  If-Match: W/\"MjAxOC0wMS0yM1QwODo1Njo0NS4yMi0wODowMA==\"
```

```gherkin
@delete @delete-fails @add-edit-endorsement @rcp-010 @2.0.0
Scenario: Delete operation fails using a given Url
  Given valid metadata have been retrieved
  And the request header "OData-Version" "equals" one of the following values
    | 4.0 | 4.01 |
  When a "DELETE" request is made to the "resource-endpoint" URL with data in "delete-fails.json"
  Then the server responds with a status code between 400 and 499
  And the response header "OData-Version" "equals" one of the following values
    | 4.0 | 4.01 |
```

<br /><br />

# Section 4. Contributors
This document was written by [Sergio Del Rio](mailto:sergio.del.rio@t4bi.com) and [Joshua Darnell](mailto:josh@reso.org).

Thanks to the Transport and Certification volunteers who contributed to this proposal.

<br /><br />

# Section 5: References

Please see the following references for more information regarding topics covered in this document:
* [OData 4.01 Specifications](https://docs.oasis-open.org/odata/odata/v4.01/)
* [RESO Data Dictionary](https://ddwiki.reso.org)
* [Transport GitHub Discussions](https://github.com/RESOStandards/transport/discussions/41)

This specification is based on the [original RCP-010 proposal](https://github.com/RESOStandards/reso-transport-specifications/files/8412594/RESOWebAPIRCP-RCP-WEBAPI-010AddFunctionalitytoWebAPISpecification-040422-1917.pdf) by Sergio Del Rio, which was previously adopted into DRAFT status by the Transport Workgroup for an earlier version of the RESO Web API.

Some requirements have been removed from the original proposal, specifically batch, child order action, and async operations. Providers MAY implement these items but they will not be tested for version 2.0.0 of this endorsement. 

<br /><br />

# Section 6: Appendix
* [HTTP Error Responses](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status#client_error_responses)

<br /><br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

Please [contact RESO Transport](mailto:transport@reso.org) if you have any questions.
