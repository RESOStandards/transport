# Add/Edit with Media

| **RCP** | 48 |
| :--- | :--- |
| **Version** | **2.0.0** |
| **Authors** | [Geoff Rispin (T4bi)](grispin@t4bi.com)<br /> [Cody Gustafson (FBS)](cody.gustafson@fbs.com)|
| **Status** | IN PROGRESS |
| **Date Ratified** | |
| **Dependencies** | [Data Dictionary 2.0+](./data-dictionary.md)<br />[Web API 2.0.0+](./web-api-core.md) |
| **Related Links** | [Open Data Protocol (OData)](https://www.odata.org/documentation/) <br /> [OData streams](https://docs.oasis-open.org/odata/odata-json-format/v4.02/odata-json-format-v4.02.html)<br /> |

The keywords "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and "OPTIONAL" in this document are to be interpreted as described in [RFC 2119](https://www.ietf.org/rfc/rfc2119.txt).

<br />

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

<br /><br />

# Table of Contents
- [Add/Edit with Media](#addedit-with-media)
- [RESO End User License Agreement (EULA)](#reso-end-user-license-agreement-eula)
- [Table of Contents](#table-of-contents)
- [Section 1: Introduction](#section-1-introduction)
- [Summary of Changes](#summary-of-changes)
- [Section 2: Specification](#section-2-specification)
  - [Data Representation](#data-representation)
    - [Data Structure](#data-structure)
    - [Status of the Media](#status-of-the-media)
  - [Media Add/Edit Process](#media-addedit-process)
  - [Media Post Processing](#media-post-processing)
  - [Media Add/Edit Examples](#media-addedit-examples)
    - [Create Initial Media Resource](#create-initial-media-resource)
    - [Upload the Media Byte Array](#upload-the-media-byte-array)
      - [Example with non-WebApi Endpoint](#example-with-non-webapi-endpoint)
      - [Example with WebApi endpoint](#example-with-webapi-endpoint)
    - [Successful Media Upload](#successful-media-upload)
      - [Example with External Media Endpoint](#example-with-external-media-endpoint)
      - [Example with WebApi Endpoint](#example-with-webapi-endpoint-1)
    - [Media Record Created but no Byte Stream Provided](#media-record-created-but-no-byte-stream-provided)
    - [Error Uploading Media](#error-uploading-media)
    - [Media Uploaded but Byte Stream Rejected](#media-uploaded-but-byte-stream-rejected)
    - [Attempting to Update a Write-Once Data Provider](#attempting-to-update-a-write-once-data-provider)
- [Section 3: Certification](#section-3-certification)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: Appendices](#section-6-appendices)
- [Section 7: License](#section-7-license)

<br /><br />


# Section 1: Introduction
Solutions using the RESO Web API need a way to add or edit media data such as photos, documents and videos so that Web API clients can provide the media binary data consistently to the servers regardless of the Web API provider.  This proposal outlines a solution that leverages the existing Add/Edit process and the OData binary data specifications.

<br /><br />

# Summary of Changes

* Introduces a specification for the addition and update of Media binary byte stream data to the transport specification
* Changes `MediaStatus` into a "Open With Enumerations" field
* Adds a `MediaStatusDescription` field to the `Media` resource
* Add a `hasStreamYN` field to the `Model` resource

<br /><br />

# Section 2: Specification

This specification relies heavily on the [OData streams](https://docs.oasis-open.org/odata/odata-json-format/v4.02/odata-json-format-v4.02.html) specification for the transport interaction with the server.

While OData streams works for the communication of the data, there are some media operations that cannot be preformed in real 
time and are deferred post-processing that could prevent the successful publishing of the media.  This could include format
validation, transcoding (resizing, reformatting) for different displays/devices, compliance validation, copyright validation and/or content delivery distribution to name a few identified scenarios.  
As a result of this need to allow for background operations to occur after the media has been received, a method to communicate the progression of the media is required.  The existing [field `MediaStatus`](https://ddwiki.reso.org/display/DDW20/MediaStatus+Field) will be used to comminucate the status and `MediaStatusDescription` will be introduced to provide contextual information when available.  This will assist clients in know why media has been rejected without having to contact the server provider outside of the transport.

## Data Representation

### Data Structure
These are the required fields to represent the upload process for media.  The change request for the data dictionary workgroup can be found [here](https://reso.atlassian.net/wiki/spaces/DDGDF/pages/9525100546/Web+API+Add+Edit+with+Media):

```xml
<EntityType Name="Media" hasStream="True">
  ...
  <Property Name="MediaStatus" Type="Edm.String" />
  <Property Name="MediaStatusDescription" Type="Edm.String" />
  ...
</EntityType>
```

The `Media` resource/model will be annotated to have an OData stream attached.  

### Status of the Media

The status of the media will be tracked on the `Media` resource.  The `MediaStatus` field will show the status of the media.

* `Incomplete` - the `Media` record that does not have a complete record 
* `Processing` - the `Media` record is syntactically complete and is currently being processed by the backend
* `Complete` - the `Media` record is validated and ready for consumption
* `Deleted` - the `Media` record is no longer published 
* `Rejected` - the `Media` has failed post processing for some reason.  Details of the problem will be presented in the `MediaStatusDescription` field. (e.g. "Media Payload does not match the specified media type", "Unable to decode Media Payload")

Some examples of `Rejected` status reason could be:
* Media stream does not match `MediaType`  (Sent a video for a photo Media record)
* The internal image checksum failed.
* Image failed copyright ID check
* Content of the media does not meet compliance rules

## Media Add/Edit Process

The creation of the Media Resource will be done using as a standard RESO Web API 2.0.0 Add/Edit process. The post SHOULD include all known resource record fields for the media with the exception of only the media byte stream.

The response object MAY contain `mediaReadLink` and `mediaEditLink` annotations which the client MUST respect as per the [OData specification](https://docs.oasis-open.org/odata/odata-json-format/v4.02/odata-json-format-v4.02.html) when communicating the byte stream to the server. If fields are not provided the implicit URLs per the [OData specification](https://docs.oasis-open.org/odata/odata-json-format/v4.02/odata-json-format-v4.02.html) MUST then be used.  

Byte stream endpoints MUST only expect previously established security credentials if they are in the same HTTP security domain as the original request.  Byte stream endpoints outside the original OData security domain cannot expect any security credentials to be provided outside of the contents of the respective annotation being used.

The Byte stream endpoint will provide HTTP response code values related to the success of the byte stream POST.  All HTTP 2xx response codes are successful.  All HTTP 4xx error codes except for HTTP 409 (Conflict) SHOULD have the client query the OData resource to validate/refresh the annotation URL and retry the POST using the URL.

The HTTP 409 (Confict) response designates that the byte stream cannot be replaced.  This can happen if the implementation is using a write-once model for media to retain history. Providers MUST return a HTTP 409 (Conflict) status when they are using a write-once model.  When a client recieves a HTTP 409 status, the client SHOULD add a new media record with the updated byte stream and then delete the original Media record. 

## Media Post Processing

The Media record will transition to `Processing` status while the server prepares the media for distribution downstream. The implementation will do all the work required for distribution before changing the status to `Complete`.

If any post-upoload processing fails, the media record will be have a status of `Rejected` and the reasoning for the Media submitter will be populated in the `MediaStatusDescription` field for actions to be taken.

Rules MAY be written against the `MediaStatus` field to handle the order of operations required to publish a listing.  Example:  Media must be complete before it can be attached to a listing or Media attached to a listing must all be Complete before a listing can be on-market.

## Media Add/Edit Examples

### Create Initial Media Resource
Create the initial media record using the OData standard. This could be an expanded record on another resource as a secondary approach but the example will be with Media as a tier 1 Resource/Model.

**REQUEST**
```http
POST https://api.my-webapi.io/Media
OData-Version: 4.01
Content-Type: application/json
Accept: application/json
Prefer: return=representation
```
```json
{
 "ShortDescription": "Ipsum Lorum",
 "Order": 1,
 "MediaType": "image/jpeg",
 
}
```

**RESPONSE - External Media storage**
```http
HTTP/2 201 Created
OData-Version: 4.01
EntityId: "12345"
Location: https://api.my-webapi.io/Media('12345')
Content-Length: 200
Content-Type: application/json
Preference-Applied: return=representation
```
```json
{
  "@odata.context": "https://api.my-webapi.io/$metadata#Media/$entity",
  "@odata.id": "Media('12345')",
  "@odata.editLink": "https://api.my-webapi.io/Media('12345')",
  "@odata.mediaReadLink": "https://api-my-webapi.io/image/not_available.jpg",
  "@odata.mediaEditLink": "https://storage.my-webapi.io/media/12345.jpg?authentication_token=my-one-time-use-auth-token-12345-zyxwut",
  "@odata.etag": "W/\"aBcDeFgHiJkLmNoPqRsTuVwXyz\"",
  "MediaObjectID": "12345",
  "ShortDescription": "Ipsum Lorum",
  "Order": 1,
  "MediaType": "image/jpeg",
  
  "MediaStatus": "Incomplete",
  "MediaStatusDescription": "Awaiting Byte Stream",
}
```

**RESPONSE - Media through Web API**

When the `MediaEditLink` annotation is not provided, there is an implicit URL for the editMediaLink `https://api.my-webapi.io/Media('12345')/$value` provided by the OData specification

```http
HTTP/2 201 Created
OData-Version: 4.01
EntityId: "12345"
Location: https://api.my-webapi.io/Media('12345')
Content-Length: 200
Content-Type: application/json
Preference-Applied: return=representation
```
```json
{
  "@odata.context":"https://api.my-webapi.io/$metadata#Media/$entity",
  "@odata.id":"Media('12345')",
  "@odata.editLink":"https://api.my-webapi.io/Media('12345')",
  "@odata.mediaReadLink": "https://api-my-webapi.io/image/not_available.jpg",
  "@odata.etag": "W/\"aBcDeFgHiJkLmNoPqRsTuVwXyz\"",
  "MediaObjectID": "12345",
  "ShortDescription": "Ipsum Lorum",
  "Order": 1,
  "MediaType": "image/jpeg",
  
  "MediaStatus": "Incomplete",
  "MediaStatusDescription": "Awaiting Byte Stream",
}
```

The return from the POST will return the Media record with the object in an `Incomplete` status.  The next step in the process is to provide the byte stream for the media object to the server at the provided endpoint.  The client MUST use the `@odata.mediaEditLink` if provided and use the implicit URL only if one is not provided as per the OData specification.

### Upload the Media Byte Array

The byte stream upload is a simple HTTP POST transaction to the provided to the endpoint.  If it is an request outside the Web API domain, then all required data (authentication, etc.) MUST be provided in the `@odata.mediaEditLink` URL.  If `@odata.mediaEditLink` is not in WebApi domain, then the established authentication (cookies, authentication headers, etc.) MUST continue to be provided.

#### Example with non-WebApi Endpoint

**REQUEST - External Target**
```http
POST https://storage.my-webapi.io/media/12345.jpg?authentication_token=my-one-time-use-auth-token=12345-zyxwut
Content-Type: image/jpeg

<<Byte stream of media object>>
```

**RESPONSE**
```http
HTTP/2 200 OK
```

#### Example with WebApi endpoint

**REQUEST - WebApi Target**
```http
POST https://api.my-webapi.io/Media('12345')/$value
Content-Type: image/jpeg

<<Byte stream of media object>>
```

**RESPONSE**
```http
HTTP/2 200 OK
```


### Successful Media Upload 
The media record can be queried to confirm the media has successfully been processed.

#### Example with External Media Endpoint

**REQUEST**
```http
GET https://api.my-webapi.io/Media('12345')
OData-Version: 4.01
Content-Type: application/json
Accept: application/json
Prefer: return=representation
```

**RESPONSE**
```http
HTTP/2 200
OData-Version: 4.01
EntityId: "12345"
Location: https://api.my-webapi.io/Media('12345')
Content-Length: 200
Content-Type: application/json
Preference-Applied: return=representation
```
```json
{
  "@odata.context": "https://api.my-webapi.io/$metadata#Media/$entity",
  "@odata.id": "Media('12345')",
  "@odata.editLink": "https://api.webapi.io/Media('12345')",
  "@odata.mediaReadLink": "https://storage.my-webapi.io/media/12345.jpg",
  "@odata.mediaEditLink": "https://storage.my-webapi.io/media/12345.jpg?authentication_token=my-one-time-use-auth-token-12345-zyxwut",
  "@odata.etag": "W/\"aBcDeFgHiJkLmNoPqRsTuVwXyz\"",
  "MediaObjectID": "12345",
  "ShortDescription": "Ipsum Lorum",
  "Order": 1,
  "MediaType": "image/jpeg",
  
  "MediaStatus": "Complete",
  "MediaStatusDescription": "Processing Complete",
}
```

#### Example with WebApi Endpoint

**RESPONSE**
```http
HTTP/2 200
OData-Version: 4.01
EntityId: "12345"
Location: https://api.my-webapi.io/Media('12345')
Content-Length: 200
Content-Type: application/json
Preference-Applied: return=representation
```
```json
{
  "@odata.context": "https://api.my-webapi.io/$metadata#Media/$entity",
  "@odata.id": "Media('12345')",
  "@odata.editLink": "https://api.webapi.io/Media('12345')",
  "@odata.mediaReadLink": "https://api.webapi.io/Media('12345')/$value",
  "@odata.etag": "W/\"aBcDeFgHiJkLmNoPqRsTuVwXyz\"",
  "MediaObjectID": "12345",
  "ShortDescription": "Ipsum Lorum",
  "Order": 1,
  "MediaType": "image/jpeg",
  
  "MediaStatus": "Complete",
  "MediaStatusDescription": "Processing Complete",
}
```

### Media Record Created but no Byte Stream Provided

This will leave the media record in a Incomplete state and the vendor can do culling/clean up as required. The client can requery the Media record for the URLs to attempt to re-use the Media record.  Rules can be used to prevent the transition of the listing until valid media is provided if required.

**REQUEST**
```http
GET https://api.my-webapi.io/Media('12345')
OData-Version: 4.01
Content-Type: application/json
Accept: application/json
Prefer: return=representation
```

**RESPONSE**
```http
HTTP/2 200
OData-Version: 4.01
EntityId: "12345"
Location: https://api.my-webapi.io/Media('12345')
Content-Length: 200
Content-Type: application/json
Preference-Applied: return=representation
```
```json
{
  "@odata.context": "https://api.my-webapi.io/$metadata#Media/$entity",
  "@odata.id": "Media('12345')",
  "@odata.editLink": "https://api.webapi.io/Media('12345')",
  "@odata.mediaReadLink": "https://api-my-webapi.io/image/not_available.jpg",
  "@odata.mediaEditLink": "https://storage.my-webapi.io/media/12345.jpg?authentication_token=my-one-time-use-auth-token-12345-zyxwut",
  "@odata.etag": "W/\"aBcDeFgHiJkLmNoPqRsTuVwXyz\"",
  "MediaObjectID": "12345",
  "ShortDescription": "Ipsum Lorum",
  "Order": 1,
  "MediaType": "image/jpeg",
  
  "MediaStatus": "Incomplete",
  "MediaStatusDescription": "Awaiting Byte Stream",
}
```

**REQUEST**
```http
POST https://storage.my-webapi.io/media/12345.jpg?authentication_token=my-one-time-use-auth-token=12345-zyxwut
Content-Type: image/jpeg

<<Byte stream of media object>>
```

**RESPONSE**
```http
HTTP/2 200 OK
```

###  Error Uploading Media

If a client gets a HTTP 4xx error (except for 409 see Write-Once behaviour below) uploading media they validate their `mediaEditLink` URL and re-attempt the upload.  After a couple of retries, they SHOULD consider the upload a failure and contact the server provider.  

This can happen in normal operation if the `mediaEditLink` is a pre-signed URL for a storage provider and the authentication credentials have timed out.  Requiring the Media record would get a updated URL with a fresh set of credentials.

If the server has timed out the Media record due to lack of completion (a byte stream was not uploaded within 30 days for example), the server will transition the `MediaStatus` to the `Rejected` state.

<ins>**Refreshed Case**</ins>

In this case, the client will get a new `mediaEditLink` URL as the authentication token was updated.

**REQUEST**
```http
POST https://api.my-webapi.io/Media('12345')
OData-Version: 4.01
Content-Type: application/json
Accept: application/json
Prefer: return=representation
```

**RESPONSE**
```http
HTTP/2 200
OData-Version: 4.01
EntityId: "12345"
Location: https://api.my-webapi.io/Media('12345')
Content-Length: 200
Content-Type: application/json
Preference-Applied: return=representation
```
```json
{
  "@odata.context": "https://api.my-webapi.io/$metadata#Media/$entity",
  "@odata.id": "Media('12345')",
  "@odata.editLink": "https://api.webapi.io/Media('12345')",
  "@odata.mediaReadLink": "https://api-my-webapi.io/image/not_available.jpg",
  "@odata.mediaEditLink": "https://storage.my-webapi.io/media/12345.jpg?authentication_token=my-one-time-use-auth-token-12345-take2",
  "@odata.etag": "W/\"aBcDeFgHiJkLmNoPqRsTuVwXyz\"",
  "MediaObjectID": "12345",
  "ShortDescription": "Ipsum Lorum",
  "Order": 1,
  "MediaType": "image/jpeg",
  
  "MediaStatus": "Incomplete",
  "MediaStatusDescription": "Awaiting Byte Stream",
}
```

**REQUEST**
```http
POST https://storage.my-webapi.io/media/12345.jpg?authentication_token=my-one-time-use-auth-token=12345-take2
Content-Type: image/jpeg

<<Byte stream of media object>>
```

**RESPONSE**
```http
HTTP/2 200 OK
```


<ins>**Rejected Case**</ins>

In the case of a `Rejected` media record, the client SHOULD replace the media record by adding a new record and deleting the old one.  

**REQUEST**
```http
GET https://api.my-webapi.io/Media('12345')
OData-Version: 4.01
Content-Type: application/json
Accept: application/json
Prefer: return=representation
```

**RESPONSE**
```http
HTTP/2 200
OData-Version: 4.01
EntityId: "12345"
Location: https://api.my-webapi.io/Media('12345')
Content-Length: 200
Content-Type: application/json
Preference-Applied: return=representation
```
```json
{
  "@odata.context": "https://api.my-webapi.io/$metadata#Media/$entity",
  "@odata.id": "Media('12345')",
  "@odata.editLink": "https://api.webapi.io/Media('12345')",
  "@odata.etag": "W/\"aBcDeFgHiJkLmNoPqRsTuVwXyz\"",
  "MediaObjectID": "12345",
  "ShortDescription": "Ipsum Lorum",
  "Order": 1,
  "MediaType": "image/jpeg",
  
  "MediaStatus": "Rejected",
  "MediaStatusDescription": "Media record has timed out",
}
```
```http
POST https://api.my-webapi.io/Media('12345')/$value
Content-Type: image/jpeg

<<Byte stream of media object>>
```

**RESPONSE**
```http
HTTP/2 403 Forbidden
```


### Media Uploaded but Byte Stream Rejected

The media provided is not of the correct type or failed some other validation process after upload but before distribution occurred.

<ins>**Resubmit Case**</ins>

This is the case where the server allows the client to replace the byte stream of a media record.  

**REQUEST**
```http
GET https://api.my-webapi.io/Media('12345')
OData-Version: 4.01
Content-Type: application/json
Accept: application/json
Prefer: return=representation
```

**RESPONSE**
```http
HTTP/2 200
OData-Version: 4.01
EntityId: "12345"
Location: https://api.my-webapi.io/Media('12345')
Content-Length: 200
Content-Type: application/json
Preference-Applied: return=representation
```
```json
{
  "@odata.context": "https://api.my-webapi.io/$metadata#Media/$entity",
  "@odata.id": "Media('12345')",
  "@odata.editLink": "https://api.webapi.io/Media('12345')",
  "@odata.mediaReadLink": "https://api-my-webapi.io/image/not_available.jpg",
  "@odata.mediaEditLink": "https://storage.my-webapi.io/media/12345.jpg?authentication_token=my-one-time-use-auth-token-12345-take2",
  "@odata.etag": "W/\"aBcDeFgHiJkLmNoPqRsTuVwXyz\"",
  "MediaObjectID": "12345",
  "ShortDescription": "Ipsum Lorum",
  "Order": 1,
  "MediaType": "image/jpeg",
  
  "MediaStatus": "Rejected",
  "MediaStatusDescription": "Byte stream is not of type image/jpeg",
}
```

**REQUEST**
```http
POST https://storage.my-webapi.io/media/12345.jpg?authentication_token=my-one-time-use-auth-token=12345-take2
Content-Type: image/jpeg

<<Byte stream of media object>>
```

**RESPONSE**
```http
HTTP/2 200 OK
```

### Attempting to Update a Write-Once Data Provider 

The client will try the resubmit behaviour above but will receive the HTTP 409 response when attempting to send the byte stream at which point the client switch to the write-once behaviour, creating a new media record with byte stream and then deleting the old record.  This approach is because the client does not know ahead of time if the server is write-once or not. 

**REQUEST**
```http
GET https://api.my-webapi.io/Media('12345')
OData-Version: 4.01
Content-Type: application/json
Accept: application/json
Prefer: return=representation
```

**RESPONSE**
```http
HTTP/2 200
OData-Version: 4.01
EntityId: "12345"
Location: https://api.my-webapi.io/Media('12345')
Content-Length: 200
Content-Type: application/json
Preference-Applied: return=representation
```
```json
{
  "@odata.context":"https://api.my-webapi.io/$metadata#Media/$entity",
  "@odata.id":"Media('12345')",
  "@odata.editLink":"https://api.webapi.io/Media('12345')",
  "@odata.etag": "W/\"aBcDeFgHiJkLmNoPqRsTuVwXyz\"",
  "MediaObjectID": "12345",
  "ShortDescription": "Ipsum Lorum",
  "Order": 1,
  "MediaType": "image/jpeg",
  
  "MediaStatus": "Rejected",
  "MediaStatusDescription": "Byte stream is not of type image/jpeg",
}
```

**REQUEST**
```http
POST https://api.my-webapi.io/Media('12345')/$value
Content-Type: image/jpeg

<<Byte stream of media object>>
```

**RESPONSE**
```http
HTTP/2 409 Conflict

```

The resolution to this behaviour is as follows.  
* Creation new record is done using the above examples
* Deletion of the old record uses the standard Add/Edit behaviour on the media resource

# Section 3: Certification

For solutions that want the add/edit with media endorsement, the follow operations should all be successful.

* Metadata Level
  * Confirm that the OData EDMX exposes the Media resource has the `hasStream` attribute set to true
  * Confirm the `Model` resource exposes the `hasStreamYN` field for the Media resource
  * Confirm the `MediaStatusDescription` field is advertised
* Resource Level
  * Confirm a new media record including the byte stream can be added
  * Confirm a media byte stream can be updated or the response follows the write-once behaviour
  * Confirm a media record that has been added with no byte stream attached shows up in as `Incomplete` in the `MediaStatus` field
  * Confirm a complete and successful media uploaded record shows as `Complete` in the `MediaStatus` field

# Section 4: Contributors
* [Geoff Rispin (T4bi)](mailto:grispin@t4bi.com)
* [Cody Gustafson (FBS)](mailto:cody.gustafson@fbs.com)
* [Joshua Darnell (RESO)](mailto:josh@reso.org)

# Section 5: References
* [Open Data Protocol (OData)](https://www.odata.org/documentation/)
* [OData streams](https://docs.oasis-open.org/odata/odata-json-format/v4.0/os/odata-json-format-v4.0-os.html)

# Section 6: Appendices

# Section 7: License

This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO](mailto:info@reso.org) if you have any questions.