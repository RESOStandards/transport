# RESO Web API Add/Edit

| **RCP** | 10 |
| :--- | :--- |
| **Authors** | Sergio Del Rio ([T4Bi](Sergio.Del.Rio@t4bi.com))<br />[Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org)) |
| **Status** | **DRAFT** |
| **Date Approved** | April 2017 (original)<br />March 2020 (testing rules) |
| **Dependencies** | [Data Dictionary 1.7+](./data-dictionary.md)<br />[Web API 2.0.0+](./web-api-core.md) |
| **Related Links** | [Draft Testing Rules](https://github.com/RESOStandards/web-api-commander/discussions/64)<br />[DD Wiki 1.7](https://ddwiki.reso.org/display/DDW17/RESO+Data+Dictionary+1.7)<br />[Data Dictionary Spreadsheet](https://docs.google.com/spreadsheets/d/1_59Iqr7AQ51rEFa7p0ND-YhJjEru8gY-D_HM1yy5c6w/edit?usp=sharing)<br /> |

The Web API Add/Edit endorsement defines how to Create, Update, and Delete data in the RESO Web API. 

This endorsement is a work in progress for 2022. It is based on the [original RCP-010 proposal](https://github.com/RESOStandards/reso-transport-specifications/files/8412594/RESOWebAPIRCP-RCP-WEBAPI-010AddFunctionalitytoWebAPISpecification-040422-1917.pdf)
by Sergio Del Rio and was previously adopted into DRAFT status by the RESO Transport Workgroup for Web API 1.1.0.

Note that some requirements have been removed from the original proposal, specifically child order action and async operations. 

Providers MAY implement these items but they will not be tested for version 2.1.0 of this endorsement. 

<br />

# Approved Testing Rules

## Feature and Background

### Discuss ...
[Selected Conformance Rules for Updatable Services](  https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_OData40MinimalConformanceLevel
)

To be considered an Updatable OData 4.0 Service, the service additionally:

18.  MUST include edit links (explicitly or implicitly) for all updatable or deletable resources according to Data-JSON]
19.  MUST support POST of new entities to insertable entity sets (section 11.4.1.5 and 11.4.2.1)
20.  MUST support POST of new related entities to updatable navigation properties (section 11.4.2)
21.  MUST support POST to $ref to add an existing entity to an updatable related collection (section 11.4.6.1)
22.  MUST support PUT to $ref to set an existing single updatable related entity (section 11.4.6.3)
23.  MUST support PATCH to all edit URLs for updatable resources (section 11.4.3)
24.  MUST support DELETE to all edit URLs for deletable resources (section 11.4.5)
25.  MUST support DELETE to $ref to remove a reference to an entity from an updatable navigation property ection 11.4.6.2)
26.  MUST support If-Match header in update/delete of any resources returned with an ETag (section 11.4.1.1)
27.  MUST return a Location header with the edit URL or read URL of a created resource (section 11.4.2)
28.  MUST include the OData-EntityId header in response to any create or upsert operation that returns 204 No ntent (section 8.3.4)
29.  MUST support Upserts (section 11.4.4)
30.  SHOULD support PUT and PATCH to an individual primitive (section 11.4.9.1) or complex (section 11.4.9.3) operty (respectively)
31.  SHOULD support DELETE to set an individual property to null (section 11.4.9.2)
32.  SHOULD support deep inserts (section 11.4.2.2)
33.  MAY support set-based updates (section 11.4.13) or deletes (section 11.4.14) to members of a collection

In addition, to be considered an [Updatable OData 4.01 Service](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_OData401MinimalConformanceLevel), the service:
  
18.  MUST conform to the OData 4.0 Minimal Conformance Level for an Updatable service.
19.  MUST support DELETE to the reference of a collection member to be removed, identified by key (section 11.4.6.
20.  SHOULD support PUT against single entity with nested content
21.  SHOULD support deep updates (section 11.4.3.1) and deep inserts (section 11.4.2.2)
22.  SHOULD support PUT or DELETE to $ref of a collection-valued nav prop
23.  MAY support POST to collections of complex/primitive types
24.  MAY support PATCH and DELETE to a collection
25.  MAY support POST, PATCH and DELETE to a collection URL terminating in a type cast segment
26.  MAY support PATCH to entity sets using the 4.01 delta payload format
27.  MAY support $select and $expand on data modification requests

<br />

### Testing Rules

```Cucumber
Feature: Web API Server Add/Edit Endorsement
  The Add/Edit Endorsement contains tests for Create, Update, and Delete actions,
  which are MAY requirements at this time.
  # SEE: https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2239399511/RCP+-+WEBAPI-010+Add+Functionality+to+Web+API+Specification

  Background:
    Given a RESOScript file was provided
    And Client Settings and Parameters were read from the file
    And a test container was successfully created from the given RESOScript
    And the test container uses an authorization_code or client_credentials for authentication
```

## Create Record

To create an entity in a collection, the client sends a POST request to that collection's URL.

The POST body MUST contain a single valid entity representation.
  
The entity representation MAY include references to existing entities as well as content for new related entities, but MUST NOT contain content for existing related entities.
  
The result of the operation is the entity with relationships to all included references to existing entities
as well as all related entities created inline.

### What Should we test?
If the key properties for an entity include key properties of a directly related entity, those related entities
MUST be included either as references to existing entities or as content for new related entities.

### Not tested here
An entity may also be created as the result of an Upsert operation.

### Should we support this here? This would mean that we'd check the fields against the metadata and then expect those items to be present in the re-query even if they weren't defined in the metadata...
To create an open entity (an instance of an open type), additional property values beyond those specified in
the metadata MAY be sent in the request body. The service MUST treat these as dynamic properties and add them
to the created instance.

### This was the initial assumption for testing?
If the entity being created is not an open entity, additional property values beyond those specified in the
metadata SHOULD NOT be sent in the request body. The service MUST fail if unable to persist all property
values specified in the request.

### Should we test that defined default values were honored for items not passed in the POST request?
Properties with a defined default value, nullable properties, and collection-valued properties omitted from the
request are set to the default value, null, or an empty collection, respectively.

### Covered below
Upon successful completion, the response MUST contain a Location header that contains the edit URL or read URL
of the created entity.

### Are we testing the 204 and supporting the return=minimal header? If so, that seems like a different test?
Upon successful completion the service MUST respond with either 201 Created and a representation of the created
entity, or 204 No Content if the request included a Prefer header with a value of return=minimal and did not
include the system query options $select and $expand.

### Discuss - do we support links in this version? Not in original RCP-010 spec.
Links to existing Entities:
https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.tml#sec_LinktoRelatedEntitiesWhenCreatinganE

### Discuss - Do we support/test for deep insert?
Deep Insert: https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.tml#sec_CreateRelatedEntitiesWhenCreatinganE


### Response Codes
A Create Entity, Create Media Entity, or Invoke Action request that successfully creates a resource returns 201
Created. In this case, the response body MUST contain the resource created.

SEE:
* https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2239399511/RCP+-+WEBAPI-010+Add+Functionality+toWeb+API+Specification#Example-1---Request
* https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_CreateanEntity
* https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#_Toc31358894

### Testing Rules

#### Success Scenario

```Cucumber

  #
  # POST serviceRoot/Property
  #      OData-Version: 4.01
  #      Content-Type: application/json;odata.metadata=minimal
  #      Accept: application/json

  @create @create-succeeds @add-edit-endorsement @rcp-010 @1.0.2
  Scenario: Create operation succeeds using a given payload
    Given valid metadata have been retrieved
    And request data has been provided in "create-succeeds.json"
    And request data in "create-succeeds.json" is valid JSON
    And schema in "create-succeeds.json" matches the metadata
    And the request header "OData-Version" "equals" one of the following values
      | 4.0 | 4.01 |
    And the request header "Content-Type" "contains" "application/json"
    And the request header "Accept" "contains" "application/json"
    When a "POST" request is made to the "resource-endpoint" URL with data in "create-succeeds.json"
    Then the server responds with one of the following status codes
      | 201 | 204 |
    And the response header "OData-Version" "equals" one of the following values
      | 4.0 | 4.01 |
    And the response header "EntityId" "MUST" "be present" if the response code was 204
    And the response header "Location" "MUST" "be present"
    And the response header "Location" "is a valid URL"
    And the response header "Location" "MUST" reference the resource being edited
    And the response is valid JSON
    And the JSON response "MAY" contain "@odata.context"
    And the JSON response value "@odata.context" "is a valid URL"
    And the JSON response "MAY" contain "@odata.id"
    And the JSON response value "@odata.id" "is a valid URL"
    And the JSON response "MUST" contain "@odata.editLink"
    And the JSON response value "@odata.editLink" "is a valid URL"
    And the JSON response "MUST" contain all JSON data in "create-succeeds.json"
    When a "GET" request is made to the URL in response header "Location"
    Then the server responds with a status code of 200
    And the response has header "OData-Version" with one of the following values
      | 4.0 | 4.01 |
    And the JSON response contains the data in "create-succeeds.json"
    And the JSON response matches the format advertised in the metadata
```

#### Failure Scenario

  Create Record Fails
  
  SEE:
  * https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2239399511/RCP+-+WEBAPI-010+Add+Functionality+toeb+API+Specification#Error-Message-Example
  
```Cucumber
  # POST serviceRoot/Property
  #      OData-Version: 4.01
  #      Content-Type: application/json
  #      Accept: application/json
  #
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
```


## Update Record

### Discuss, do we support both PUT and PATCH? 

OData recommends not using PUT due to data loss...

To update an individual entity, the client makes a PATCH or PUT request to a URL that identifies the entity.
Services MAY restrict updates only to requests addressing the edit URL of the entity.

Services SHOULD support PATCH as the preferred means of updating an entity. PATCH provides more resiliency between
clients and services by directly modifying only those values specified by the client.

The semantics of PATCH, as defined in [RFC5789], is to merge the content in the request payload with the
[entity’s] current state, applying the update only to those components specified in the request body.

Collection properties and primitive properties provided in the payload corresponding to updatable properties
MUST replace the value of the corresponding property in the entity or complex type. Missing properties of the
containing entity or complex property, including dynamic properties, MUST NOT be directly altered unless as a
side effect of changes resulting from the provided properties.

Services MAY additionally support PUT but should be aware of the potential for data-loss in round-tripping
properties that the client may not know about in advance, such as open or added properties, or properties not
specified in metadata. Services that support PUT MUST replace all values of structural properties with those
specified in the request body. Missing non-key, updatable structural properties not defined as dependent
properties within a referential constraint MUST be set to their default values. Omitting a non-nullable property
with no service-generated or default value from a PUT request results in a 400 Bad Request error. Missing dynamic
structural properties MUST be removed or set to null.

For requests with an OData-Version header with a value of 4.01 or greater, the media stream of a media entity can
be updated by specifying the base64url-encoded representation of the media stream as a virtual property $value.

Updating a dependent property that is tied to a key property of the principal entity through a referential
constraint updates the relationship to point to the entity with the specified key value.
If there is no such entity, the update fails.

Updating a principle property that is tied to a dependent entity through a referential constraint on the dependent
entity updates the dependent property.

Key and other properties marked as read-only in metadata (including computed properties), as well as dependent
properties that are not tied to key properties of the principal entity, can be omitted from the request.
If the request contains a value for one of these properties, the service MUST ignore that value when applying
the update. Services MUST return an error if an insert or update contains a new value for a property marked as
updatable that cannot currently be changed by the user (i.e., given the state of the object or permissions
of the user). The service MAY return success in this case if the specified value matches the value of the
property. Clients SHOULD use PATCH and specify only those properties intended to be changed.

Entity id and entity type cannot be changed when updating an entity. However, format-specific rules might in some
cases require providing entity id and entity type values in the payload when applying the update.

For requests with an OData-Version header with a value of 4.01 or greater, if the entity representation in the
request body includes an ETag value, the update MUST NOT be performed and SHOULD return 412 Precondition Failed
if the supplied ETag value is not * and does not match the current ETag value for the entity. ETag values in
request bodies MUST be ignored for requests containing an OData-Version header with a value of 4.0.

If an update specifies both a binding to a single-valued navigation property and a dependent property that is
tied to a key property of the principal entity according to the same navigation property, then the dependent
property is ignored, and the relationship is updated according to the value specified in the binding.

If the entity being updated is open, then additional values for properties beyond those specified in the metadata or returned in a previous request MAY be sent in the request body. The service MUST treat these as dynamic properties.

If the entity being updated is not open, then additional values for properties beyond those specified in the
metadata or returned in a previous request SHOULD NOT be sent in the request body. The service MUST fail if it is
unable to persist all updatable property values specified in the request.

Upon successful completion the service responds with either 200 OK and a representation of the updated entity,
or 204 No Content.

The client may request that the response SHOULD include a body by specifying a Prefer header with a value of
return=representation, or by specifying the system query options $select or $expand.

If the service uses ETags for optimistic concurrency control, the entities in the response MUST include ETags.

SEE:
* https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_UpdateanEntity
* https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2239399511/RCP+-+WEBAPI-010+Add+Functionality+to+WebAPI+Specification#5.3-Examples-for-Updating-Data


### Testing Rules


#### Success Scenario

```Cucumber
  # PATCH serviceRoot/Property('12345')
  #   OData-Version: 4.01
  #   Content-Type: application/json;odata.metadata=minimal
  #   Accept: application/json
  #   If-Match: W/\"MjAxOC0wMS0yM1QwODo1Njo0NS4yMi0wODowMA==\"
  #   Prefer: return=representation
  #
  @update @update-succeeds @add-edit-endorsement @rcp-010 @1.0.2
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
    Then the server responds with a status code of 200
    And the response has header "OData-Version" with one of the following values
      | 4.0 | 4.01 |
    And the JSON response contains the data in "update-succeeds.json"
    And the JSON response matches the format advertised in the metadata
```

#### Failure Scenario
SEE:
* https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2239399511/RCP+-+WEBAPI-010+Add+Functionality+to+Web+API+Specification#Error-Message-Example


```Cucumber
  #
  # PATCH serviceRoot/Property('12345')
  #   OData-Version: 4.01
  #   Content-Type: application/json;odata.metadata=minimal
  #   Accept: application/json
  #   If-Match: W/\"MjAxOC0wMS0yM1QwODo1Njo0NS4yMi0wODowMA==\"
  #   Prefer: return=representation
  #
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
```


## Delete Record

A successful DELETE request to an entity's edit URL deletes the entity. The request body SHOULD be empty.
Singleton entities cannot be deleted.

On successful completion of the delete, the response MUST be 204 No Content and contain an empty body.

Services MUST implicitly remove relations to and from an entity when deleting it; clients need not delete the elations explicitly.

SEE:
* http://docs.oasis-open.org/odata/odata/v4.01/csprd04/part1-protocol/odata-v4.01-csprd04-part1-protocol.tml#sec_DeleteanEntity
* https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2239399511/RCP+-+WEBAPI-010+Add+Functionality+to+WebAPI+Specification#5.3-Examples-for-Deleting-Data

### Testing Rules

#### Success Scenario

```Cucumber
  #
  # DELETE serviceRoot/Property('12345')
  #   OData-Version: 4.01
  #   If-Match: W/\"MjAxOC0wMS0yM1QwODo1Njo0NS4yMi0wODowMA==\"
  #
  @delete @delete-succeeds @add-edit-endorsement @rcp-010 @1.0.2
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

#### Failure Scenario

SEE:
* https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2239399511/RCP+-+WEBAPI-010+Add+Functionality+to+Web+API+Specification#Error-Message-Example


```Cucumber
  #
  # DELETE serviceRoot/Property('12345')
  #   OData-Version: 4.01
  #   If-Match: W/\"MjAxOC0wMS0yM1QwODo1Njo0NS4yMi0wODowMA==\"
  #
  @delete @delete-fails @add-edit-endorsement @rcp-010 @1.0.2
  Scenario: Delete operation fails using a given Url
    Given valid metadata have been retrieved
    And the request header "OData-Version" "equals" one of the following values
      | 4.0 | 4.01 |
    When a "DELETE" request is made to the "resource-endpoint" URL with data in "delete-fails.json"
    Then the server responds with a status code between 400 and 499
    And the response header "OData-Version" "equals" one of the following values
      | 4.0 | 4.01 |

```