# RESO Data Dictionary JSON Payloads

| **RCP** | 25 |
| :--- | :--- |
| **Version** | **1.7** |
| **Authors** | Alexander Likholyot ([Planitar](alex@planitar.com))<br />Joshua Darnell ([RESO](mailto:josh@reso.org))<br />Paul Stusiak ([Falcon Technologies](pstusiak@falcontechnologies.com)) | |
| **Status** | **IN PROGRESS** |
| **Date Submitted** | February 2019 |
| **Date Approved** | April 2019 |
| **Dependencies** | [Data Dictionary 1.7](https://ddwiki.reso.org/display/DDW17/RESO+Data+Dictionary+1.7) |
| **Related Links** | [RCP-022](https://github.com/RESOStandards/transport/blob/43-migrate-rcp-022-from-confluence/autofill-service.md) |

Data Dictionary JSON Payloads are a normative standard format generated from the Data Dictionary specification that can be used with any JSON Web API.  

# Synopsis
This RCP outlines the a generalized Data Dictionary JSON Schema to accompany the Lightweight Autofill proposal (RCP-022)
and for use in any JSON Web API.

# Rationale
When working with RESO Data Dictionary data, it's convenient to be able to pick that data up from any JSON Web API URL.

In doing so, there is a need to have a standard shape that all RESO Web API servers can understand as well. 

This proposal doesn't introduce any new Data Dictionary items. Rather, it presents a normative reference for the data shape 
expected in transports that don't use the RESO Web API directly, such as [RCP-022](https://github.com/RESOStandards/transport/blob/43-migrate-rcp-022-from-confluence/autofill-service.md).

# Proposal
In the Lightweight Autofill Service proposal ([RCP-022](https://github.com/RESOStandards/transport/blob/43-migrate-rcp-022-from-confluence/autofill-service.md)), 
a RESO Web API server is expected to retrieve data from an external source and attach it to a Property record, either existing or in-progress, in system other than the one it originated in.

While these data could be pushed into that Web API server by means of Add/Edit ([RCP-010](./web-api-add-edit.md)) using an update client, it's much easier for 
professionals working with real estate data (such as Photographers), to provide a url containing a lightweight payload that can be 
loaded into a listing instead. 

The main requirement in the preceding example is having a standard Data Dictionary JSON format to provide the record in. The URL and protocol used to
retrieve the payload are a separate concern. 

This proposal outlines the canonical format all Data Dictionary JSON payloads should follow.

## Case 1: Record Does Not Include Keys for Existing Records
In this case, using the example of the Autofill Service, no keys have been provided 
and records are expected to be created for all resource data contained in the payload. 

The Autofill data provider would submit a URI to the Web API host, which would resolve to a payload similar to the following:

```
{
  "value": [
    {
      "Country": "CA",
      "StateOrProvince": "YT",
      "City": "Dawson City",
      "PostalCode": "Y0B 1G0",
      "StreetName": "Bayshore Rd",
      "StreetNumber": "1803",
      "Media": [
        {
          "ResourceName": "Property",
          "MediaCategory": "Branded Virtual Tour",
          "MediaType": "mov",
          "MediaURL": "https://example.com/vJVDL415WZ7GE1/",
          "ShortDescription": "Example"
        },
        {
          "ResourceName": "Property",
          "MediaCategory": "Floor Plan",
          "MediaType": "pdf",
          "MediaURL": "https://example.com/vJVDL415WZ7GE1/doc/floorplan_imperial.pdf",
          "ShortDescription": "imperial"
        }
      ],
      "Rooms": [
        {
          "RoomType": "Dining",
          "RoomName": "Breakfast",
          "RoomWidth": 4.409,
          "RoomLength": 2.977,
          "RoomLengthWidthUnits": "Meters",
          "RoomLengthWidthSource": "LocalProvider"
        },
        {
          "RoomType": "Dining",
          "RoomName": "Dining",
          "RoomWidth": 4.3,
          "RoomLength": 5.998,
          "RoomLengthWidthUnits": "Meters",
          "RoomLengthWidthSource": "LocalProvider"
        }
      ]
    }
  ]
}

```

Note the nested Media and Rooms arrays in the payload. The RESO Web API uses expanded data sets when related information accompanies a another record. 

In this case, the Property record has Media and Rooms information as well. This allows RESO Web API servers to use the same payload for autofill 
as they do with upsert provided through Add/Edit ([RCP-010](./web-api-add-edit.md)).

## Case 2: Data Includes Keys for Existing Records
Assume listing data already exists in the Web API host's system and the goal is to make a call to an autofill provider 
to update existing data, such as the StreetNumber of the Property record and dimensions of one of the existing rooms. 

In this case, the user would provide an autofill URI that provides response similar to that used in Case 1, 
except now the resulting payload would contain keys for any data that was meant to be updated, as follows:

```
{
  "value": [
    {
      "StreetNumber": "1804",
      "Rooms": [
        {
          "RoomKey": "A34535F235",
          "RoomWidth": 5.139,
          "RoomLength": 3.882
        }
      ]
    }
  ]
}
```
Note: in the case of upserts, the autofill provider MUST include the keys for the records to be updated. 

Autofill providers are expected to return a standard `HTTP 200` status code when autofill objects are successfully
transferred from the provider to the autofill consumer, i.e. the Web API host. 

Consumers of external JSON Web API services should be prepared to render the appropriate error message in cases where the autofill object fails to transfer. 

This proposal doesn't specify any additional status codes that might be used in such a scenario and instead relies on 
existing HTTP protocol conventions for status codes. 

For example, a 500 should be thrown when there's an internal server error of some kind on the autofill provider.

# Impact
Those wanting to implement the Autofill proposal described in [RCP-022](https://github.com/RESOStandards/transport/blob/43-migrate-rcp-022-from-confluence/autofill-service.md) and [RCP-025](https://github.com/RESOStandards/transport/new/44-migrate-rcp-025-from-confluence/dd-json-payloads.md) 
will need to create a mechanism for retrieving autofill objects and attaching them to an existing resource in their system 
(or use them to create new resources). 

This proposal doesn't add additional requirements to the Web API specification, as its purpose is to create a data contract 
that can be used to process external resource data into a Web API host system.

# Compatibility
RESO Data Dictionary 1.7+

# Certification Impact
RESO Certification is not required at this time. This may be revised given sufficient interest in having these payloads certified. 

# Original Proposal
[**Download PDF**](https://github.com/RESOStandards/transport/files/9862314/RESOWebAPIRCP-RCP.-.WEBAPI-025.Lightweight.Autofill.Schema-251022-164452.pdf)

