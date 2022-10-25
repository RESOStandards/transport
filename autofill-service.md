# RESO Autofill Service

| **RCP** | 22 |
| :--- | :--- |
| **Version** | **1.0.0** |
| **Authors** | Alexander Likholyot ([Planitar](mailto:alex@planitar.com))<br />Paul Stusiak ([Falcon Technologies](mailto:pstusiak@falcontechnologies.com))<br />Joshua Darnell ([RESO](mailto:josh@reso.org)) |
| **Status** | **IN PROGRESS** |
| **Date Submitted** | August 2018 |
| **Date Approved** | April 2019 |
| **Protocol** | HTTP |
| **Dependencies** | [RCP-025](https://github.com/RESOStandards/transport/blob/44-migrate-rcp-025-from-confluence/dd-json-payloads.md)<br />[RESO Data Dictionary 1.7](https://ddwiki.reso.org/display/DDW17/RESO+Data+Dictionary+1.7) |
| **Related Links** | [OASIS OData TC](https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=odata) <br /> |

The RESO Autofill Service makes it easy to consume RESO standard data sets in Data Dictionary JSON format.

<br />

# Synopsis
A lightweight autofill object would be helpful for 3rd parties who provide autofill or auto-population of RESO Data Dictionary resource data to be used during the MLS listing input process.

# Rationale
When an agent gets a listing, the first thing they most often do is calling their 3rd party service provider (photographer, surveying or marketing company) to go out and get property data. Often an agent creates a listing only after that 3rd party property data is available and the manual data entry into MLS listing form is tedious and error-prone. This is especially true with room dimensions and floor areas where errors expose agents to certain liability. It makes sense to automate the data entry process as much as possible.

To automate this process, a URI that can be used to retrieve a Lightweight Autofill Object would be provided to the MLS input process by a member, who receives it from a 3rd party service provider. An implementation could provide Javascript in the MLS web-based listing input form that would parse the JSON data and automatically fill out the relevant form fields.

While there are other methods to accomplish this, such as to require an agent to obtain some pointer to a new MLS listing (such as MLS number and MLS server address), then go back to their 3rd party service provider with that info and ask them to manually update the listing with data or to use the Odata update transaction or to modify processes to get the MLS to query a 3rd party server for data on a particular address which would be prone to property mismatch errors.

The Lightweight Autofill Object offers a way to minimize the burden on all parties involved, especially agents and 3rd party service providers (e.g. photographers) who are much less technically savvy compared to MLS's, thus lowering barriers and increasing probability of wide adoption.

A few new definitions will need to be added to the Data Dictionary to support JSON document structuring of buildings, floors, and rooms. (Note that additional Data Dictionary resources have been tabled by the time being, see RCP-024 for further information).

An implementation of this API has been created and is in use. Planitar has implemented the Lightweight Autofill Object via an autofill endpoint for iGUIDE service providers (they deliver to agents photos, floor plans, room dimensions and floor areas). We are working with Stratus Data Systems to roll it out for the Toronto Real Estate Board. Rob Larson has expressed interest in supporting the autofill functionality at CRMLS if it is part of RESO standards.


# Proposal
The proposed protocol is called "Lightweight Autofill Object." It assumes that an endpoint serving a JSON object will be provided, which returns structured data in Data Dictionary format. The expectation is that an MLS listing input module would collect the URL of the autofill provider, retrieve the Lightweight Autofill Object, and then use it to pre-populate various fields in the listing form as well as other operations such as fetching property photos. The core of the proposed standard is the structure and naming of fields in the JSON object. See RCP-025 for specifics regarding the schema of the Lightweight Autofill Object.

# Lightweight Autofill Object Workflow
Service providers provide agents with a URL which can return Lightweight Autofill Object data.

* An agent pastes the autofill URL into a MLS listing input form.
* The MLS vendor (autofill consumer) fetches the Lightweight Autofill Object from the given URL.
* The autofill consumer processes the payload and populates the desired form fields with the given data.
* The agent saves the record, updating the listing with the data provided by the Lightweight Autofill Object.
* Optional Implementation Details
  * The listing form may ask an agent to check off data that the agent wishes to import instead of importing everything

The Autofill URL can be used not only during initial listing creation, but also during a listing update and the form may 
ask the agent to resolve any merge conflicts

# Get RESO Property Object
The Lightweight Autofill Provider produces a link where data in Data Dictionary JSON format may be retrieved. 

The consumer then picks up the data and ingests it using the RESO standard format.

**Request**
```
GET https://autofill.site.com
```

**Response**
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
      "PropertyRooms": [
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

Returns information about property with a tour as a RESO Property object:
* Property address
* Property structure and measurement details:
  * property structures (buildings), floors, rooms
  * dimensions, areas, wall perimeter lengths
  * etc.
* Media:
  * Branded and unbranded iGUIDE URLs
  * PDF floor plan URLs (metric and imperial)
  * gallery images URLs
  * video URL
  * etc.

See [RCP-025](https://github.com/RESOStandards/transport/blob/44-migrate-rcp-025-from-confluence/dd-json-payloads.md) for more information about the Data Dictionary JSON format.

# Impact
Does not require any new Data Dictionary or Web API standards. Uses existing Data Dictionary JSON format.

# Compatibility
JSON Web APIs, including the RESO Web API.

# Certification Impact
None at this time.

# Original Proposal
[**Download PDF**](https://github.com/RESOStandards/transport/files/9861970/RESOWebAPIRCP-RCP.-.WEBAPI-022.Lightweight.Autofill.Object-251022-155712.pdf)
