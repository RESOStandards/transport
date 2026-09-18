# RESO (Endorsement Name) Endorsement

| **RCP** | XXX |
| :--- | :--- |
| **Version** | **x.y.z** |
| **Authors** | [Author 1](#)<br />[Author 2](#) |
| **Specification** | [**LINK TO RCP**](#) |
| **Status** | IN PROGRESS |
| **Date Ratified** | Month YYYY |
| **Dependencies** | [Link 1](#)<br />[Link 2](#) |
| **Related Links** | [Link 1](#)<br />[Link 2](#) |


<br /><br />

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

<br /><br />

# Table of Contents
- [Summary of Changes](#summary-of-changes)
- [Introduction](#introduction)
- [Section 1: Purpose](#section-1-purpose)
- [Section 2: Specification](#section-2-specification)
  - [Section 2.1: (Subsection Name)](#section-21-subsection-name)
- [Section 3: Certification](#section-3-certification)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: Appendices](#section-6-appendices)
- [Section 7: License](#section-7-license)

<br /><br />

# Summary of Changes

* Provide a brief summary of changes contained within this RCP. 
* List significant changes or impact to existing standards.

<br /><br />

# Introduction
Introductory information about your proposal, including any significant background or industry insight to help reviewers understand the context. 

<br /><br />

# Section 1: Purpose
Outline specific business problems, goals, and practices that the specification addresses. 

<br /><br />

# Section 2: Specification
Provide specifics about the proposal in this section. 

The following should be included:
* Information about Authorization or Authentication, when relevant. 
* Sample request and response payloads.
* Examples of algorithms written in pseudo code.
* Links to existing specifications used in the standard, with examples of how they're used.
* Definitions of the Data Dictionary elements the proposal adds or changes, as tables (see Section 2.1).

## Section 2.1: (Subsection Name)
Number the subsections of Section 2 as "Section 2.x: Name" and list them in the Table of Contents.

Define each Data Dictionary element where the proposal introduces it, in a table with one row per field: **Field**, **Type** (the Data Dictionary simple data type), **Nullable** and **Definition** (one sentence in the Data Dictionary definition style), plus **Max length** or **Lookup** where they apply. Rules that do not fit a cell follow the table as short paragraphs. The standard values of a lookup are a two-column table beside the field that uses them.

| Field | Type | Nullable | Max length | Lookup | Definition |
| :--- | :--- | :--- | :--- | :--- | :--- |
| ExampleName | String | Yes | 50 | | The human-friendly name of the example. |
| ExampleStatus | String List, Single | No | | ExampleStatus | The status of the example. |
| ExampleYN | Boolean | Yes | | | Indicates whether or not the example applies. |

**ExampleStatus.** The lookup is open with enumerations; its standard values are:

| Lookup Value | Definition |
| :--- | :--- |
| Active | The example is in use. |
| Inactive | The example is no longer in use. |

<br /><br />

# Section 3: Certification

List testing rules required for the specification here. 

<br /><br />

# Section 4: Contributors
This document was written by [Your Name](mailto:you@yourcompany.org), [Author 1](mailto:author1@company.org), and [Author 2](mailto:author2@company.org).

Thanks to the following contributors for their help with this project:

| Contributor | Company |
| --- | --- |
| Contributor 1 | Company 1 |
| Contributor 2 | Company 2 |

<br /><br />

# Section 5: References

Please see the following references for more information regarding topics covered in this document:
* [Reference 1](https://reso.org)
* [Reference 2](https://reso.org)
* ...

<br /><br />

# Section 6: Appendices

## Proposed Data Dictionary elements
Point to the parts of Section 2 that define the elements rather than repeating them, and list anything the proposal deprecates. For example:

The elements this proposal defines, including the ExampleStatus lookup and its values, are in [Section 2.1](#section-21-subsection-name). The deprecated elements are:

| Resource | Deprecated Field | Replaced by | Note |
| :--- | :--- | :--- | :--- |
| Example | OldField | NewField | MAY be served for backward compatibility. |

Related proposals belong in the Related Links row of the header table. Other appendices, such as design rationale or worked examples, MAY follow this one.

<br /><br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO](mailto:info@reso.org) if you have any questions.
