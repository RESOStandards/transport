# RESO Payloads Endorsement

| **RCP** | 41 |
| :-- | :-- |
| **Version** | **2.0** |
| **Authors** | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org)) |
| **Specification** | [**LINK TO TESTING RULES**](https://github.com/RESOStandards/reso-transport-specifications/issues/23) |
| **Status** | **IN PROGRESS** |
| **Date Started** | April 2022 |
| **Protocol** | HTTP |
| **Dependencies** | [Data Dictionary 1.7](./data-dictionary.md)<br />[Web API Core 2.0.0+](./web-api-core.md) |

Payloads 2.0 defines general data validation and availability testing rules, as well as the IDX Payload Endorsement.

**New in version 2.0**
* Support for OData `$expand`
* Sampling using server-driven paging
* Strict checking of what's advertised in the metadata vs. what was found in the payload

<br />

# Approved Testing Rules
See [GitHub Issue](https://github.com/RESOStandards/transport/issues/23).

## Support for Expanded Data Elements

In order to support OData Expand, a new Payloads specification will be needed to perform data validation and measure availability.

Data availability testing will be extended beyond top level resources, as outlined in the 1.7 Payloads spec, to support related data elements:

* When data is available at the top level, it will be sampled at the top level. For example, `GET /Property`.
* Records will be sampled using `@odata.nextLink` without any filters. For example, `GET /Property`. Paging will be done until a predetermined number of records have been fetched. 
* Sampling will then be done using a filter, most likely one year back. Example: `GET /Property?$filter=ModificationTimestamp gt 2021-06-24T21:06:29Z`
* IF the queries return a partial result, the `@odata.nextLink` header MUST be present. This will be verified by checking the response size against the advertised count on the resource. 
* When a navigation property is found in the server metadata, the testing tools will attempt to expand each of the items for that resource and score the sampled item separately from the Property resource. For example, `GET /Property?$expand=Media`
* IF an expansion and navigation property can be followed for a resource that’s not required at the top level, such as Media, a provider will pass testing.
* Providers MAY expose the resource at the top level as well, in which case they will both be tested.
* Providers MUST offer the ability to retrieve data using at least one of these methods for all advertised data models (resources or expanded elements). If no data can be retrieved, then the provider won’t pass testing.
* Certain resources MUST be available at the top level IF the provider supports them in their data set. These items also MAY be expanded when standard relationships exist for them. These items are: 
  * Property
  * Member
  * Office
  * Field
  * Lookup

This will also impact how data availability sampling is done in that RESO will ensure that next link works reliably over large data sets with various query options:
* The full set will be consumed from each resource without any query parameters using server driven paging.
* Since record sets can change over the course of sampling, a count will be taken before sampling starts and afterwards and some amount of discrepancy will be allowed, TBD.
* A subset of those records will be consumed using a filter of one year back at the time of testing.
* The `odata.maxpagesize` header will be tested ([link](https://docs.oasis-open.org/odata/odata/v4.0/os/part1-protocol/odata-v4.0-os-part1-protocol.html#_Toc372793629)) and will be set to a value that ideally works across all servers responses can be compared directly. A value of 1,000 would be a good place to start. 
* The server MUST provide a `Preference-Applied` header ([link](https://docs.oasis-open.org/odata/odata/v4.0/os/part1-protocol/odata-v4.0-os-part1-protocol.html#_Toc372793629)) for `odata.maxpagesize` tests. If the preference was applied then the page size MUST be the same as that requested. 

<br />

[Download Payloads 1.7 PDF](https://github.com/RESOStandards/reso-transport-specifications/files/8412711/RESO.Payloads.Testing.Specification.-.GitHub.pdf)


---

<br />
