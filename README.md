# Transport Specifications
Proposals are organized into sections based on their underlying protocol, for example the _RESO Web API_.

Specifications are divided into modular endorsements, each of which are offered separately through RESO Certification. 

RESO Endorsements use [Semantic Versioning](https://semver.org/).

# RESO Web API
The following proposals use the RESO Web API as a transport mechanism.

## [Data Dictionary Endorsement](./data-dictionary.md)
**Status**: Adopted, **Current Version**: 1.7

The Data Dictionary endorsement defines models for use in the RESO domain. These include Resources, Fields, Lookups, and Relationships between Resources.

## [Payloads Endorsement](./payloads.md)
**Status**: Adopted, **Current Version**: 1.7

Defines general payloads data availability testing rules, as well as the IDX Payload Endorsement.

## [Web API Core Endorsement](./web-api-core.md)

**Status**: Adopted, **Current Version**: 2.0.0

The Web API Core endorsement defines the primary functionality RESO Web API servers are expected to have in order to provide both replication and live query support.

## [Web API Add/Edit Endorsement](./web-api-add-edit.md)
**Status**: In Review by the RESO Certification Subgroup

The Web API Add/Edit endorsement defines how to Create, Update, and Delete data in the RESO Web API. 

## [Web API Validation Expression Endorsement](./web-api-validation-expression.md)
**Status**: In Transport Backlog

Web API Validation Expressions allow for the transport of machine-executable business rules using the [RETS 019 Validation Expression grammar](https://github.com/darnjo/rcp019).


<br />

# Related Documents
The following documents may also be helpful:
* [Web API Core Testing Specification](https://docs.google.com/document/d/1btCduOpWWzeadeMcSviA8M9dclIz23P-bPUGKwcD0NY/edit#heading=h.tsujzsa8zmlt)
* [Data Dictionary 1.7 Testing Specification](https://docs.google.com/document/d/15DFf9kDX_mlGCJVOch2fztl8W5h-yd18N0_03Sb4HwM/edit?usp=sharing)
* [Payloads 1.7 Testing Specification](https://docs.google.com/document/d/1hNMqmDdK0C31tKrfdZnHIk1WmJPbAuluV_eJbErddCo/edit?usp=sharing)
* [RESO Commander](https://github.com/RESOStandards/web-api-commander)

<br />

# New Change Proposals

## RCP Template
If you would like to suggest a new change proposal, please use the [**RCP Template**](./RCP-TEMPLATE.md) in this repository and fill in each section accordingly. 

## Submission Process
The process for submitting a new proposal is as follows:
* Create a new branch for the proposal by choosing the next available RCP number. 
  - The branch name should be created from the RCP number and use a short and meaningful name so others understand what the proposal does.
  - It should also include the current version of the proposal affected. 
  - If the change proposal affects an existing version, a tag should be included specifying whether the proposed changes result in a major, minor, or patch level change to the existing spec.
  - Example: `rcp-040-fancy-new-web-api-feature-2.0.0-major`.
* Use the RCP Template to create a change proposal.
* When the proposal is ready for review, create a Draft PR so that it may be reviewed by the community and chairs.
* If adopted, the proposal will be merged into the main branch.

Please contact [RESO Development](mailto:dev@reso.org) if you have any questions.
