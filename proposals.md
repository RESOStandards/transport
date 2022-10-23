# RESO Change Proposals (RCPs)
RESO Change Proposals are created through a [community review process](./reso-rcp-process.md).

See [new change proposals](./reso-rcp-process.md#new-change-proposals) 


<br /><br />


# RCPs and Statuses
* [**RATIFIED STANDARDS**](./proposals#ratified-standards)
* [**DRAFT SPECIFICATIONS**](./proposals#draft-specifications)
* [**IN PROGRESS PROPOSALS**](./proposals#in-progress-proposals)

<br /><br />

# RATIFIED STANDARDS
Ratified standards have been adopted by the Transport Workgroup and RESO as an organization.

Proposals that require certification must have adopted a specification, testing rules, and production-ready testing tools before being ratified.

<br />

## Data Dictionary 1.7

| **RCP** | RCP-036 |
| :--- | :--- |
| **Authors** | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org)) |
| **Specification** | [**LINK TO RCP**](./data-dictionary.md) |
| **Status** | **RATIFIED** |
| **Date Ratified** | January 2021 |
| **Dependencies** | [Web API Core 2.0.0+](./web-api-core.md) |
| **Related Links** | [DD Wiki 1.7](https://ddwiki.reso.org/display/DDW17/RESO+Data+Dictionary+1.7)<br />[Data Dictionary 1.7 Spreadsheet](https://docs.google.com/spreadsheets/d/1_59Iqr7AQ51rEFa7p0ND-YhJjEru8gY-D_HM1yy5c6w/edit?usp=sharing) |

The Data Dictionary endorsement defines models for use in the RESO domain. These include Resources, Fields, Lookups, and Relationships between Resources. The Data Dictionary may use the RESO Web API or RESO's Common Schema JSON format for transport.

<br /><br />

## Web API Core 2.0.0

| **RCP** | RCP-037 |
| :--- | :--- |
| **Authors** | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org))  |
| **Specification** | [**LINK TO RCP**](./web-api-core.md) |
| **Status** | **RATIFIED** |
| **Date Ratified** | January 2021 |
| **Protocol** | HTTP |
| **Dependencies** | OData 4.0 or 4.01<br />TLS 1.2+<br />OAuth 2 (Auth Token or Client Credentials) |
| **Related Links** | [OASIS OData TC](https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=odata) <br /> |

The Web API Core endorsement defines the primary functionality RESO Web API servers are expected to support in order to provide both replication and live query support.

<br /><br />

## Payloads 1.7

| **RCP** | RCP-038 |
| :-- | :-- |
| **Authors** | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org)) |
| **Specification** | [**LINK TO RCP**](./payloads.md) |
| **Status** | **RATIFIED** |
| **Date Ratified** | August 2021 |
| **Protocol** | HTTP |
| **Dependencies** | [Data Dictionary 1.7](data-dictionary.md)<br />[Web API Core 2.0.0+](./web-api-core.md) |

Defines general payloads data validation and availability testing rules, as well as the IDX Payload Endorsement.

<br /><br />

---

<br /><br />

# DRAFT SPECIFICATIONS
Draft specifications have been approved by the Certification Subgroup and are awaiting implementations and community review prior to Transport approval.

<br />

## Web API Add/Edit

| **RCP** | RCP-010 |
| :--- | :--- |
| **Authors** | Sergio Del Rio ([T4Bi](Sergio.Del.Rio@t4bi.com))<br />[Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org)) |
| **Specification** | [**LINK TO RCP**](./web-api-add-edit.md) |
| **Status** | **DRAFT** |
| **Date Approved** | April 2017 (original) |
| **Dependencies** | [Data Dictionary 1.7+](./data-dictionary.md)<br />[Web API 2.0.0+](./web-api-core.md) |
| **Related Links** | [Draft Testing Rules](https://github.com/RESOStandards/web-api-commander/discussions/64)<br />[DD Wiki 1.7](https://ddwiki.reso.org/display/DDW17/RESO+Data+Dictionary+1.7)<br />[Data Dictionary Spreadsheet](https://docs.google.com/spreadsheets/d/1_59Iqr7AQ51rEFa7p0ND-YhJjEru8gY-D_HM1yy5c6w/edit?usp=sharing)<br /> |

The Web API Add/Edit endorsement defines how to Create, Update, and Delete data in the RESO Web API. 

<br /><br />

## Web API Validation Expressions

| **RCP** | RCP-019 |
| :--- | :--- |
| **Authors** | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org))<br />Paul Stusiak ([Falcon Technologies](mailto:pstusiak@falcontechnologies.com)) |
| **Specification** | [LINK TO RCP](./web-api-validation-expression.md) |
| **Status** | **DRAFT** |
| **Date Approved** | April 2018 |
| **Dependencies** | [Validation Expression grammar](https://github.com/darnjo/rcp019) <br /> [Data Dictionary 1.7+](./data-dictionary.md)<br />[Web API 2.0.0+](./web-api-core.md)<br /> |
| **Related Links** | [DD Wiki 1.7](https://ddwiki.reso.org/display/DDW17/RESO+Data+Dictionary+1.7)<br />[Data Dictionary Spreadsheet](https://docs.google.com/spreadsheets/d/1_59Iqr7AQ51rEFa7p0ND-YhJjEru8gY-D_HM1yy5c6w/edit?usp=sharing)<br /> |

Web API Validation Expressions allow for the transport of machine-executable business rules using the [RETS 019 Validation Expression grammar](https://github.com/darnjo/rcp019).

<br /><br />

## Lookup Resource

| **RCP** | RCP-032 |
| :--- | :--- |
| **Authors** | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org))<br />Ryan Yates ([Rapattoni Corporation](mailto:ryates@rapattoni.com))<br />Sergio Del Rio ([T4Bi](Sergio.Del.Rio@t4bi.com))<br />Rob Larson ([Larson Consulting](rob@larson.cc))<br />Paul Stusiak ([Falcon Technologies](mailto:pstusiak@falcontechnologies.com)) |
| **Specification** | [**LINK TO RCP**](https://github.com/RESOStandards/transport/blob/c94fcd4d511a70f500a462cd10b8e48e10d0c113/data-dictionary.md) |
| **Status** | **DRAFT** |
| **Status Date** | December 2021 |
| **Dependencies** | [Data Dictionary 1.7+](./data-dictionary.md)<br />[Web API Core 2.0.0+](./web-api-core.md) |
| **Related Links** | [DD Wiki 1.7](https://ddwiki.reso.org/display/DDW17/RESO+Data+Dictionary+1.7)<br />[Data Dictionary Spreadsheet](https://docs.google.com/spreadsheets/d/1_59Iqr7AQ51rEFa7p0ND-YhJjEru8gY-D_HM1yy5c6w/edit?usp=sharing) |

The Lookup Resource provides a framework to advertise human-friendly display names and use them in payloads. This reduces the amount of interpretation data consumers need to do when using enumerations, and they can display what they get in the payload directly rather than inferring the values from annotations. It also better supports providers who have a large amount of lookup metadata, allowing it to be consumed and updated through incremental updates rather than having to update all metadata each time something changes.

<br /><br />

## Data Dictionary 2.0

| **RCP** | RCP-040 |
| :--- | :--- |
| **Authors** | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org)) |
| **Specification** | [**LINK TO RCP**](https://github.com/RESOStandards/transport/blob/c1baab163567a98c5457137d65ad09e3a22ac46e/data-dictionary.md) |
| **Status** | **DRAFT** |
| **Date Approved** | Sept 2022 |
| **Dependencies** | [Web API Core 2.0.0+](./web-api-core.md) |
| **Related Links** | [DD Wiki 2.0](https://ddwiki.reso.org/pages/viewpage.action?pageId=1123655)<br />[Data Dictionary 2.0 Spreadsheet](https://docs.google.com/spreadsheets/d/1_59Iqr7AQ51rEFa7p0ND-YhJjEru8gY-D_HM1yy5c6w/edit?usp=sharing) |

The Data Dictionary endorsement defines models for use in the RESO domain. These include Resources, Fields, Lookups, and Relationships between Resources.

**New in version 2.0**
* Added new resources, fields, and enumerations
* Additional validation of resources, fields, and enumerations using a number of heuristics (substring, edit distance, and data-driven matching)
* Data validation against server metadata - if items are found in the payload that are not advertised, providers will ont pass testing
* Updated Data Dictionary reference sheet structure

<br /><br />

---

<br /><br />

# IN PROGRESS PROPOSALS
In progress proposals are ones that are in review by Transport and Certification. See the [RESO RCP Process](./reso-rcp-process.md) and section on [new change proposals](./reso-rcp-process.md#new-change-proposals).

<br />

## Web API Core 2.1.0

| **RCP** | RCP-039 |
| :--- | :--- |
| **Authors** | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org)) |
| **Specification** | [**LINK TO TESTING RULES**](https://github.com/RESOStandards/transport/issues/22) |
| **Status** | **IN PROGRESS** |
| **Date Proposed** | April 2022 |
| **Protocol** | HTTP |
| **Dependencies** | OData 4.0 or 4.01<br />TLS 1.2+<br />OAuth 2 (Auth Token or Client Credentials) |

The Web API Core 2.1.0 endorsement defines the primary functionality RESO Web API servers are expected to have in order to provide both replication and live query support. 

**New in version 2.1.0**
* Support for OData `$expand`
* Server-Driven Paging (`@odata.nextLink`) required for Certification
* Support for the Lookup Resource, including string comparison tests for enumerations

<br /><br />

## Payloads 2.0

| **RCP** | RCP-041 |
| :-- | :-- |
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

Please contact dev@reso.org if you have any questions.

---

<br />

[<- HOME](./README.md)
