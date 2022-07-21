# RESO Endorsements
Endorsements are modular specifications grouped by their transport procotol, for example, the _RESO Web API_.

Specifications use [semantic versioning](https://semver.org/).

Please visit our [discussion forum](https://github.com/RESOStandards/reso-transport-specifications/discussions) if you have any questions or would like to start a proposal.

See [issues](https://github.com/RESOStandards/reso-transport-specifications/issues) for new specifications.

Proposals MUST have two verified implementations from separate vendors before they can be adopted.

<br />

| RCP | Name | Version | Status | Status Date |
| :-- | :-- | :-- | :-- | :-- |
| [**RCP-036**](#data-dictionary-17) | Data Dictionary | 1.7 | [**RATIFIED**](https://github.com/RESOStandards/reso-transport-specifications/edit/main/README.md#ratified) | Jan 2021 |
| [**RCP-037**](#web-api-core-200) | Web API Core | 2.0 | [**RATIFIED**](https://github.com/RESOStandards/reso-transport-specifications/edit/main/README.md#ratified) | Jan 2021|
| [**RCP-038**](#payloads-17) | Payloads (with IDX Payload) | 1.7 | [**RATIFIED**](https://github.com/RESOStandards/reso-transport-specifications/edit/main/README.md#ratified) | Aug 2021 |
| | 
| [**RCP-010**](#web-api-addedit) | Web API Add/Edit | 2.0.0 | [**DRAFT**](https://github.com/RESOStandards/reso-transport-specifications/edit/main/README.md#draft) | Apr 2017 |
| [**RCP-019**](#web-api-validation-expressions) | Web API Validation Expressions | 2.0.0 | [**DRAFT**](https://github.com/RESOStandards/reso-transport-specifications/edit/main/README.md#draft) | Apr 2018 |
| [**RCP-032**](#lookup-resource) | Lookup Resource | 1.7 | [**DRAFT**](https://github.com/RESOStandards/reso-transport-specifications/edit/main/README.md#draft) | Dec 2021 |
| [**RCP-040**](#data-dictionary-20) | Data Dictionary | 2.0 | [**DRAFT**](https://github.com/RESOStandards/reso-transport-specifications/edit/main/README.md#draft) | Dec 2021 |
| | 
| [**RCP-039**](#web-api-core-210) | Web API Core | 2.1.0 | [**IN PROGRESS**](https://github.com/RESOStandards/reso-transport-specifications/edit/main/README.md#draft) | Apr 2021 |
| [**RCP-041**](#payloads-20) | Payloads | 2.0 | [**IN PROGRESS**](https://github.com/RESOStandards/reso-transport-specifications/edit/main/README.md#draft) | Apr 2021 |

<br />

---

<br />

# RATIFIED 
Ratified proposals have been adopted by the workgroups and RESO as an organization. Proposals that require certification testing must have adopted a specification, testing rules, and production-ready testing tools for ratification.

## [Data Dictionary 1.7](./data-dictionary.md)
| **RCP** | RCP-036 |
| :--- | :--- |
| **Authors** | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org)) |
| **Status** | **RATIFIED** |
| **Date Ratified** | January 2021 |
| **Dependencies** | [Web API Core 2.0.0+](./web-api-core.md) |
| **Related Links** | [Specification](./data-dictionary.md)<br />[DD Wiki 1.7](https://ddwiki.reso.org/display/DDW17/RESO+Data+Dictionary+1.7)<br />[Data Dictionary Spreadsheet](https://docs.google.com/spreadsheets/d/1_59Iqr7AQ51rEFa7p0ND-YhJjEru8gY-D_HM1yy5c6w/edit?usp=sharing) |

The Data Dictionary endorsement defines models for use in the RESO domain. These include Resources, Fields, Lookups, and Relationships between Resources.

<br />

## [Web API Core 2.0.0](./web-api-core.md)
| **RCP** | RCP-037 |
| :--- | :--- |
| **Authors** | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org)) |
| **Status** | **RATIFIED** |
| **Date Ratified** | January 2021 |
| **Protocol** | HTTP |
| **Dependencies** | OData 4.0 or 4.01<br />TLS 1.2+<br />OAuth 2 (Auth Token or Client Credentials) |
| **Related Links** | [Specification](./web-api-core.md)<br /> |

The Web API Core endorsement defines the primary functionality RESO Web API servers are expected to support in order to provide both replication and live query support.

<br />

## [Payloads 1.7](./payloads.md)
| **RCP** | RCP-038 |
| :-- | :-- |
| **Authors** | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org)) |
| **Status** | **RATIFIED** |
| **Date Ratified** | August 2021 |
| **Protocol** | HTTP |
| **Dependencies** | [Data Dictionary 1.7](data-dictionary.md)<br />[Web API Core 2.0.0+](./web-api-core.md) |
| **Related Links** | [Specification](./payloads.md)<br /> |

Defines general payloads data validation and availability testing rules, as well as the IDX Payload Endorsement.

<br /><br />

# DRAFT
Draft proposals have been approved by the workgroups and organization and are awaiting implementations and community review.

## [Web API Add/Edit](./web-api-add-edit.md)

| **RCP** | RCP-010 |
| :--- | :--- |
| **Authors** | Sergio Del Rio ([T4Bi](Sergio.Del.Rio@t4bi.com))<br />[Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org)) |
| **Status** | **DRAFT** |
| **Date Approved** | April 2017 (original) |
| **Dependencies** | [Data Dictionary 1.7+](./data-dictionary.md)<br />[Web API 2.0.0+](./web-api-core.md)<br />[Validation Expression grammar](https://github.com/darnjo/rcp019) |
| **Related Links** | [Specification (Draft PR)](https://github.com/RESOStandards/reso-transport-specifications/blob/c94fcd4d511a70f500a462cd10b8e48e10d0c113/data-dictionary.md)<br />[DD Wiki 1.7](https://ddwiki.reso.org/display/DDW17/RESO+Data+Dictionary+1.7)<br />[Data Dictionary Spreadsheet](https://docs.google.com/spreadsheets/d/1_59Iqr7AQ51rEFa7p0ND-YhJjEru8gY-D_HM1yy5c6w/edit?usp=sharing)<br /> |

The Web API Add/Edit endorsement defines how to Create, Update, and Delete data in the RESO Web API. 

<br />

## [Web API Validation Expressions](./web-api-validation-expression.md)

| **RCP** | RCP-019 |
| :--- | :--- |
| **Authors** | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org))<br />Paul Stusiak ([Falcon Technologies](mailto:pstusiak@falcontechnologies.com)) |
| **Status** | **DRAFT** |
| **Date Approved** | April 2018 |
| **Dependencies** | [Data Dictionary 1.7+](./data-dictionary.md)<br />[Web API 2.0.0+](./web-api-core.md)<br />[Validation Expression grammar](https://github.com/darnjo/rcp019) |
| **Related Links** | [Specification (Draft PR)](https://github.com/RESOStandards/reso-transport-specifications/blob/c94fcd4d511a70f500a462cd10b8e48e10d0c113/data-dictionary.md)<br />[DD Wiki 1.7](https://ddwiki.reso.org/display/DDW17/RESO+Data+Dictionary+1.7)<br />[Data Dictionary Spreadsheet](https://docs.google.com/spreadsheets/d/1_59Iqr7AQ51rEFa7p0ND-YhJjEru8gY-D_HM1yy5c6w/edit?usp=sharing)<br /> |

Web API Validation Expressions allow for the transport of machine-executable business rules using the [RETS 019 Validation Expression grammar](https://github.com/darnjo/rcp019).

<br />

## [Lookup Resource](https://github.com/RESOStandards/reso-transport-specifications/blob/c94fcd4d511a70f500a462cd10b8e48e10d0c113/data-dictionary.md)

| **RCP** | RCP-032 |
| :--- | :--- |
| **Authors** | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org))<br />Ryan Yates ([Rapattoni Corporation](mailto:ryates@rapattoni.com))<br />Sergio Del Rio ([T4Bi](Sergio.Del.Rio@t4bi.com))<br />Rob Larson ([Larson Consulting](rob@larson.cc))<br />Paul Stusiak ([Falcon Technologies](mailto:pstusiak@falcontechnologies.com)) |
| **Status** | **DRAFT** |
| **Status Date** | December 2021 |
| **Dependencies** | [Web API Core 2.0.0+](./web-api-core.md) |
| **Related Links** | [Specification (Draft PR)](https://github.com/RESOStandards/reso-transport-specifications/blob/c94fcd4d511a70f500a462cd10b8e48e10d0c113/data-dictionary.md)<br />[DD Wiki 1.7](https://ddwiki.reso.org/display/DDW17/RESO+Data+Dictionary+1.7)<br />[Data Dictionary Spreadsheet](https://docs.google.com/spreadsheets/d/1_59Iqr7AQ51rEFa7p0ND-YhJjEru8gY-D_HM1yy5c6w/edit?usp=sharing) |

The Lookup Resource provides a framework to advertise human-friendly display names and use them in payloads. This reduces the amount of interpretation data consumers need to do when using enumerations, and they can display what they get in the payload directly rather than inferring the values from annotations. It also better supports providers who have a large amount of lookup metadata, allowing it to be consumed and updated through incremental updates rather than having to update all metadata each time something changes.

<br />

## [Data Dictionary 2.0](https://github.com/RESOStandards/reso-transport-specifications/blob/c1baab163567a98c5457137d65ad09e3a22ac46e/data-dictionary.md)

| **RCP** | RCP-040 |
| :--- | :--- |
| **Authors** | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org)) |
| **Status** | **DRAFT** |
| **Dependencies** | [Web API Core 2.0.0+](./web-api-core.md) |
| **Related Links** | [Specification (Draft PR)](https://github.com/RESOStandards/reso-transport-specifications/blob/c1baab163567a98c5457137d65ad09e3a22ac46e/data-dictionary.md)<br />[DD Wiki 1.7](https://ddwiki.reso.org/display/DDW17/RESO+Data+Dictionary+1.7)<br />[Data Dictionary Spreadsheet](https://docs.google.com/spreadsheets/d/1_59Iqr7AQ51rEFa7p0ND-YhJjEru8gY-D_HM1yy5c6w/edit?usp=sharing) |

The Data Dictionary endorsement defines models for use in the RESO domain. These include Resources, Fields, Lookups, and Relationships between Resources.

**New in version 2.0**
* Added new resources, fields, and enumerations
* Additional validation of resources, fields, and enumerations using a number of heuristics (substring, edit distance, and data-driven matching)
* Data validation against server metadata - if items are found in the payload that are not advertised, providers will ont pass testing
* Updated Data Dictionary reference sheet structure

<br /><br />

# IN PROGRESS
Proposals that are in progress are ones that have been reviewed by the workgroups and are actively being worked on prior to draft status.

## [Web API Core 2.1.0](https://github.com/RESOStandards/reso-transport-specifications/issues/22)
| **RCP** | RCP-039 |
| :--- | :--- |
| **Authors** | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org)) |
| **Status** | **IN PROGRESS** |
| **Date Started** | April 2022 |
| **Protocol** | HTTP |
| **Dependencies** | OData 4.0 or 4.01<br />TLS 1.2+<br />OAuth 2 (Auth Token or Client Credentials) |
| **Related Links** | [GitHub Issue](https://github.com/RESOStandards/reso-transport-specifications/issues/22)<br /> |

The Web API Core 2.1.0 endorsement defines the primary functionality RESO Web API servers are expected to have in order to provide both replication and live query support. 

**New in version 2.1.0**
* Support for OData `$expand`
* Server-Driven Paging (`@odata.nextLink`) required for Certification
* Support for the Lookup Resource, including string comparison tests for enumerations

<br />

## [Payloads 2.0](https://github.com/RESOStandards/reso-transport-specifications/issues/23)
| **RCP** | RCP-041 |
| :-- | :-- |
| **Authors** | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org)) |
| **Status** | **IN PROGRESS** |
| **Date Started** | April 2022 |
| **Protocol** | HTTP |
| **Dependencies** | [Data Dictionary 1.7](./data-dictionary.md)<br />[Web API Core 2.0.0+](./web-api-core.md) |
| **Related Links** | [GitHub Issue](https://github.com/RESOStandards/reso-transport-specifications/issues/23)<br /> |

Payloads 2.0 defines general data validation and availability testing rules, as well as the IDX Payload Endorsement.

**New in version 2.0**
* Support for OData `$expand`
* Sampling using server-driven paging
* Strict checking of what's advertised in the metadata vs. what was found in the payload

<br /><br />


# Related Documents
The following documents may also be helpful:
* [RESO DDWiki](https://ddwiki.reso.org/display/DDW17/)
* [RESO Data Dictionary 1.7 Reference Spreadsheet](https://docs.google.com/spreadsheets/d/1_59Iqr7AQ51rEFa7p0ND-YhJjEru8gY-D_HM1yy5c6w/edit?usp=sharing)
* [RESO Commander](https://github.com/RESOStandards/web-api-commander)

<br />

# New Change Proposals

## RCP Template
If you would like to suggest a new change proposal, please use the [**RCP Template**](./rcp-template.md) in this repository and fill in each section accordingly. 

## Submission Process

## 1. Proposal Phase
* Post on GitHub Discussions. 
* Request that the topic be added to the appropriate Transport and/or Certification agendas. Ideally, there would be some activity in the GitHub discussion before and after the workgroup meeting. 
* Once there’s consensus in the group to move a given item forward, an issue should be created and work on a specification can begin. 
* A preliminary specification and set of testing rules should be produced that others could (unambiguously) build software to. An impact assessment should also be included, when applicable.
* After work on the specification is complete, it should be posted for community review at least two weeks before the Transport or Certification meeting it’s to be discussed in.

## 2. Draft Phase
* Once approved into draft status, a draft PR should be made.
* The proposal will stay in draft status until it has at least two implementations from two separate vendors, at which point there will be a workgroup discussion, including revisiting which existing specifications and versions to add it to, if applicable. 
* The implementations should be verified against the proposed testing rules by RESO staff, even if on a development server. 

## 3. Approval / Adoption Phase
* The PR will be taken out of draft status at this point, awaiting final approval.
* There may also be a motion to create Certification tools, and potentially new reports or metrics, at that time. If so, these items need to be added to RESO’s backlog. 
* The Transport and Certifiaction groups will decide which specification(s) it should be added to and which version(s) to target; those writing the proposals can make requests in this regard. 
* Once the new specification is ready for ratification, related PRs will be merged into their corresponding specifications.


Please contact [RESO Development](mailto:dev@reso.org) if you have any questions.
