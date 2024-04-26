# RESO Add/Edit Media

| **RCP** | 48 |
| :--- | :--- |
| **Version** | **2.1.0** |
| **Authors** | [Geoff Rispin (T4bi)](grispin@t4bi.com)<br /> [Cody Gustafson (FBS)](cody.gustafson@fbs.com)|
| **Status** | IN PROGRESS |
| **Date Ratified** | |
| **Dependencies** | [Data Dictionary 2.0+](./data-dictionary.md)<br />[Web API 2.1.0+](./web-api-core.md) |
| **Related Links** | [Open Data Protocol (OData)](https://www.odata.org/documentation/) <br /> [OData streams](https://docs.oasis-open.org/odata/odata-json-format/v4.0/os/odata-json-format-v4.0-os.html)<br /> |


<br /><br />

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

<br /><br />

# Table of Contents
- [RESO Add/Edit Media](#reso-addedit-media)
- [RESO End User License Agreement (EULA)](#reso-end-user-license-agreement-eula)
- [Table of Contents](#table-of-contents)
- [Summary of Changes](#summary-of-changes)
- [Introduction](#introduction)
- [Section 1: Purpose](#section-1-purpose)
- [Section 2: Specification](#section-2-specification)
  - [Data Representation](#data-representation)
    - [Data Structure](#data-structure)
    - [Status of the Media](#status-of-the-media)
  - [Media Upload Process](#media-upload-process)
  - [Media post processing](#media-post-processing)
- [Section 3: Certification](#section-3-certification)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: Appendices](#section-6-appendices)
- [Section 7: License](#section-7-license)

<br /><br />

# Summary of Changes

* Introducing a specification for the addition and update of Media binary byte stream data to the transport specification

<br /><br />

# Introduction
A method is needed to preform add and edit operations on the binary data stored on the Media resource.  Currently this data is read-only in the RESO WebApi specification and this functionality is needed for feature parity with RETS.

<br /><br />

# Section 1: Purpose
The goal of this proposal is to provide a method for media submission to the server.

<br /><br />

# Section 2: Specification

This specification relies heavily on the [OData streams](https://docs.oasis-open.org/odata/odata-json-format/v4.0/os/odata-json-format-v4.0-os.html) specification for the transport interaction with the server.

While OData streams works for the communication of the data, there are some media operations that cannot be preformed in real 
time and are deferred post-processing that could prevent the successful publishing of the media.  This could include format
validation, transcoding (resizing, reformating) for different displays/devices, compliance validation, copyright validation and/or content delivery distribution to name a few identified scenarios.  
As a result of this need to allow for background operations to occur after the media has been received, a method to communicate the progression of the media is required.  The existing field `MediaStatus` will be used to comminucate the status and `MediaStatusDescription` will be introduced to provide contextual information when available.  This will assist clients in know why media has been rejected without having to contact the server provider outside of the transport.

## Data Representation

### Data Structure
These are the required fields to represent the upload process for media

```xml
<EntityType Name="Media">
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
* `Rejected` - the `Media` has failed post processing for some reason.  Details of the problem will be presented in the `MediaStatusDescription` field.

Some examples of `Rejected` status reason could be:
* Media stream does not match `MediaType`  (Sent a video for a photo Media record)
* The internal image checksum failed.
* Image failed copyright ID check

## Media Upload Process

The creation of the media resource will be done using as a standard RESO WebApi 2.1.0 Add/Edit process. The post should include all known resource record fields for the media with the exception of only the media byte stream.

The response object may contain `mediaReadLink` and `mediaEditLink` annotations which the client MUST respect as per the OData specification when communicating the byte stream to the server. If records are not provided the implicit URLs per the OData specification can then be used.  

Byte stream endpoints must only expect previously established security credentials if they are in the same HTTP security domain as the original request.  Byte stream endpoints outside the original OData security domain cannot expect any security credentials to be provided outside of the contents of the respective annotation being used.

The Byte stream endpoint will provide HTTP response code values related to the success of the byte stream POST.  All HTTP 2xx response codes are successful.  All HTTP 4xx error codes except for HTTP 409 (Conflict) should have the client query the OData resource to validate/refresh the annotation URL and retry the POST using the URL.

The HTTP 409 (Confict) response designates that the byte stream cannot be replaced.  This can happen if the implementation is using a write-once model for media to retain history.  When a 409 is received, the client should add a new media record with the updated byte stream and then delete the original Media record.

## Media post processing

The Media record will transition to `Processing` status while the server prepares the media for distribution downstream. The implementation will do all the work required for distribution before changing the status to `Complete`

If any post-upoload processing fails, the media record will be have a status of `Rejected` and the reasoning for the Media submitter will be populated in the `MediaStatusDescription` field for actions to be taken.

Rules may be written against the `MediaStatus` field to handle the order of operations required to publish a listing.  Example:  Media must be complete before it can be attached to a listing or Media attached to a listing must all be Complete before a listing can be on-market.

# Section 3: Certification
TBD

# Section 4: Contributors
* [Geoff Rispin (T4bi)](grispin@t4bi.com)
* [Cody Gustafson (FBS)](cody.gustafson@fbs.com)
* [Joshua Darnell (RESO)](mailto:josh@reso.org)

# Section 5: References
* [Open Data Protocol (OData)](https://www.odata.org/documentation/)
* [OData streams](https://docs.oasis-open.org/odata/odata-json-format/v4.0/os/odata-json-format-v4.0-os.html)

# Section 6: Appendices

# Section 7: License

This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO](mailto:info@reso.org) if you have any questions.