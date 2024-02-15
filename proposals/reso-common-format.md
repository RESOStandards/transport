# RESO Common Format

| **RCP** | 25 |
| :--- | :--- |
| **Version** | **1.7** |
| **Authors** | Alexander Likholyot ([Planitar](mailto:alex@planitar.com))<br />Joshua Darnell ([RESO](mailto:josh@reso.org))<br />Paul Stusiak ([Falcon Technologies](mailto:pstusiak@falcontechnologies.com)) |
| **Status** | **RATIFIED** |
| **Date Submitted** | February 2019 |
| **Date Ratified** | November 2023 |
| **Dependencies** | [Data Dictionary 1.7+](https://ddwiki.reso.org/display/DDW17/RESO+Data+Dictionary+1.7) |
| **Related Links** | [RESO Common Schema Open API Format](https://github.com/RESOStandards/web-api-commander/blob/main/src/main/resources/RESODataDictionary-1.7.openapi3.json) |

# Synopsis
This proposal establishes a normative JSON data format that corresponds with the RESO Data Dictionary and is compatible with existing Web API implementations for retrieving or creating data.

Data Dictionary JSON format is not limited to Web APIs and may be used with any data source, for instance an S3 bucket or web hooks event. Another use is as a standard format for off-chain data in distributed ledgers. 

Both single- and multi-valued payloads are supported, as well as local data elements.

# Rationale
The RESO Web API is used primarily by MLS data providers, and it's important they provide a common interface for metadata, query support, and payload formats throughout the MLS landscape due to its decentralized nature. The RESO Web API provides a standard API Gateway for the MLS domain.

There are other participants in the real estate transaction and ecosystem who benefit from having a standard payload format to provide interoperability between various systems. For example, tour, assessment, and showing providers or photographers.

API services might be structured differently than the RESO Web API that MLSs use, but the data is in the same format and therefore interoperable between providers.


# Proposal
The RESO Data Dictionary is a transport-independent representation of data models within the RESO domain.

The RESO Web API uses a versioned, JSON-based representation of the Data Dictionary which is generated from its specification. 

The goal of this proposal is to generalize that format.

## Context
When using the RESO Web API clients can infer the Data Dictionary models they're representing, as there's a canonical URI structure that corresponds to advertised data elements.

When working with Data Dictionary JSON Payloads, the context of the data isn't clear. 

In order to address this, a special context variable will be added to Data Dictionary JSON payloads to indicate which model and version is being represented in the payload. 

RESO has a [Uniform Resource Name (URN) namespace](https://www.iana.org/assignments/urn-formal/reso) registered with IANA. 

Section 3.4.1 (a) of this proposal shows a representation of a versioned Data Dictionary resource: 

```
3.4.1 RESO Data Dictionary Metadata Elements
  (a) Versioned Metadata Resource Element

      Format:
        urn:reso:metadata:{version}:resource:{resource-name}

      where "version" MAY have values such as "1.6" or "1.7", and
      "resource-name¨ is one of "Property", "Member", "Office", or 
      other resource definitions RESO provides (see references).

      Example: 
        urn:reso:metadata:1.7:resource:property
```

[**READ MORE about URNs**](https://en.wikipedia.org/wiki/Uniform_Resource_Name)

**Example**

RESO context variable for a Data Dictionary 1.7 Property Resource:

```
{
  "@reso.context": "urn:reso:metadata:1.7:resource:property",
  ...
}
```

Those working with the Data Dictionary JSON format should be prepared to extract standard data elements, such as fields and lookups, using versioned RESO reference metadata, as there may be local data in the payload whose schema is unknown ahead of time. 

There are metadata resources and services available to assist in doing this. Please contact dev@reso.org for more information. 

Since the Data Dictionary also defines related objects or collections for each resource, such as ListAgent as a Member record or Media as a collection of Media items in a Property Resource, the versioned resource in the URN provides enough information to interpret these items as well.

Additional parameters may be added to the URN to indicate such things as a particular action that goes along with the payload.

## Representations
There are two possible representations for Data Dictionary JSON Payloads:
* **Single-valued payloads** represent a value for a single record at the top level.
* **Multi-valued payloads** represent a collection of zero or more records, where zero items means the empty list `[]`.

These are outlined in the following sections.

### Single-Valued Payloads
It's often convenient to be able to represent a single, top-level JSON Data Dictionary item:

```json
{
  "@reso.context": "urn:reso:metadata:1.7:resource:property",
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
```

It's also important to support this format to maintain interoperability with existing Web API clients and servers, which use it for Add/Edit and "Singleton" objects.

### Multi-Valued Payloads

Multi-valued payloads are also supported, in which case the response contains an array property called `value` which is a collection of zero or more items corresponding to the Data Dictionary model annotated in `@reso.context`.

Using the payload in the previous example:

```json
{
  "@reso.context": "urn:reso:metadata:1.7:resource:property",
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

Those using Data Dictionary JSON Payloads should be prepared to check for the `value` property to determine whether to interpret the payload as single- or multi-valued.

# Impact
This proposal doesn't introduce any additional requirements for current RESO Web API providers. Data Dictionary JSON Payloads are designed to be interoperable with their current formats. 

# Compatibility
RESO Data Dictionary 1.7+.

# Certification
Since this specification isn't tied to a specific transport, testing is a bit different than endorsements that use the OData Web API.

## Payload Samples
Providers will submit payload samples matching the format described in this proposal, which will be validated using JSON schema artifacts generated from the Data Dictionary specification being tested. Ideally, this would consist of up to 100,000 records per resource starting with the newest records first. Samples can be submitted as a single file per resource or collection of them. Local data elements will not be tested, but will show up on the Certification Report along with their inferred data type.

## Schema Validation
RESO will test that each standard field matches its Data Dictionary definition in both name and type and that no synonyms are present for fields. For enumerations, RESO will check for either the machine-friendly OData values or human-friendly values. There may be reasons for those using RESO Common Format to also support machine-friendly OData values if they're doing an integration with a Web API server.

## Variations Report
If the provider supports Data Dictionary 2.0 or greater, a Variations Report will be produced similar to Data Dictionary 2.0 testing. This file will contain corrections for resource, field, and lookup mappings where they don't match the Data Dictionary as well as suggested mappings if additional mappings were found. Providers will need to resolve the items flagged in the Variations Report before an endorsement can be issued. 

## Certification Process and RESO Analytics
Once a provider has submitted samples, testing has been performed, variations have been addressed, and testing has produced a "Passed" result, providers will be able to approve and publish their results in RESO Analytics. Results will appear on both https://certification.reso.org and https://reso.org/certification.

To get started with testing, please contact [dev@reso.org](mailto:dev@reso.org). 


# Original Proposal
[**Download PDF**](https://github.com/RESOStandards/transport/files/9862314/RESOWebAPIRCP-RCP.-.WEBAPI-025.Lightweight.Autofill.Schema-251022-164452.pdf)
