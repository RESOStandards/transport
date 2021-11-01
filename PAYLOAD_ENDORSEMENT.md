# RESO Payload Endorsement

| Item | Value |
|---|---|
| Data Dictionary Version | 1.7.0+ |
| Web API Version | Core 2.0.0+ |
| Written | September 2020 |
| Ratified | August 31, 2021 |

[Link to Original Document](https://docs.google.com/document/d/1hNMqmDdK0C31tKrfdZnHIk1WmJPbAuluV_eJbErddCo/edit)

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.


# Table of Contents
- [Summary of Proposal](#summary-of-proposal)
- [Purpose](#purpose)
- [Testing Methodology](#testing-methodology)
- [Reporting](#reporting)
- [Certification Workflow](#certification-workflow)
- [Testing Framework](#testing-framework)
- [Feature Requests](#feature-requests)


# Summary of Proposal



## Initial Release: RESO Web API IDX Payloads (Q1 2021)

1. [**Required Fields**](https://docs.google.com/document/d/1hNMqmDdK0C31tKrfdZnHIk1WmJPbAuluV_eJbErddCo/edit#heading=h.hvjhgayb4mb9): All RESO Data Dictionary resources sampled MUST have a Key and ModificationTimestamp or the tests will fail.
   * The String representation of the key defined in the Data Dictionary for RESO standard resources and fields (*ListingKey, MemberKey, OfficeKey, etc.*) MUST be present. These keys will be used for sampling and MUST support key-based queries. 
   * Vendors MAY reject queries other than those which fetch by key, and should return an HTTP 501 response in this case, as outlined in [section 13.1.1.7 in the OData 4.0 Errata 03 Conformance Rules](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part1-protocol/odata-v4.0-errata03-os-part1-protocol-complete.html#_Toc453752323).
   * Applicants MAY provide the Numeric version of each key as well, and it will be counted if present (e.g., ListingKeyNumeric), but is not required. Numeric keys will not be validated against string keys. Non-Data Dictionary resources MUST have a key field by the OData specification, but they will not be checked and providers MAY use their own keys in those cases.

2. [**Sampling**](https://docs.google.com/document/d/1hNMqmDdK0C31tKrfdZnHIk1WmJPbAuluV_eJbErddCo/edit#heading=h.l3oqsdcr9j2t): After required metadata and field checks have passed, up to 10,000 records will be randomly sampled with the credentials of a) the full-access RESO user, and b) the given Payload user (e.g. IDX) for whom results are to be recorded and published. No payload data will be retained during this process aside from keys and modification timestamps of each record and the names of resources and fields discovered in the payload; hashes will be created for the rest of the data and used to compare results from (a) and (b).
3. [**Scoring**](https://docs.google.com/document/d/1hNMqmDdK0C31tKrfdZnHIk1WmJPbAuluV_eJbErddCo/edit#heading=h.8m9ohllnfmeu): Data collected will be scored and reports published showing the frequency with which data were found in each field, both for the RESO user and the Payloads specific user, as well as if data were found for one set of credentials but not the other. 

There will be a 1-2 month beta period before the tool is released during which time data providers will be able to run the Payloads testing tool on their servers before applying for certification, with the idea that they will help provide feedback during this time. 

Once the Payloads testing specification has been ratified, there are compliance rules for some data providers. MLSs have specific certification requirements due to their role in data distribution. They MUST be certified with an IDX Payload Endorsement even if they had received a Data Dictionary 1.7 certification prior to the IDX Payload testing tool being released. Those who have not yet been certified for Data Dictionary 1.7 will be required to pass before their IDX Payloads Endorsement is approved.

Web API Core 1.0.2 certification is also required for MLSs prior to Data Dictionary or Payloads certification being approved.

## Related: Data Dictionary Payloads Endorsement (Q2 2021)

* Will extend Payloads testing support to non-OData Web API data sources via [RCP-022](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2254275316/RCP+-+WEBAPI-022+Lightweight+Autofill+Object) and [RCP-025](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2259388340/RCP+-+WEBAPI-025+Lightweight+Autofill+Schema). 
* Currently in the Specification and Prototyping Phase. Seeking volunteers.



# Purpose

The purpose of the RESO Payloads testing tool is to deliver a flexible and automated framework that provides ease of use and configurable requirements for a set of Data Dictionary resources and fields identified in a given payload.

The goal of Payloads testing, in general, is to ensure that data is being delivered for fields in a given Data Dictionary payload.

To address this, random sampling is used both to ensure that a) all mandatory requirements are met, such as the fact that keys and timestamps MUST have data in them, and b) to report statistics for the amount of data availability found for a representative user of a given payload compared to a user with full access.

One of the primary goals of this project is that data providers can run the testing tools themselves on their own servers to see how they perform before applying for Payloads endorsements.



# Testing Methodology

There are several phases of RESO Payloads testing.

## Configuring the Test Client

The starting point is for applicants to create a configuration file in RESOScript (XML) format which contains credentials and a server's RESO Web API endpoint. A sample RESOScript file and instructions for how to use it will be provided with the initial release of the testing tool.

## Metadata Request Using RESO Standard Authentication

When testing begins, an HTTP request is made to an applicant’s given service location with either OAuth2 [Bearer Tokens](https://oauth.net/2/bearer-tokens/) or [Client Credentials](https://oauth.net/2/grant-types/client-credentials/). Both of these authentication strategies allow for data consumption to be machine automated so that additional interaction from a user isn’t necessary during the authentication process. As such, the RESO Data Dictionary Commander can be used for automated testing.

The metadata request is used when RESO Web API servers are the data source, in which case they are expected to function according to the OData specification in terms of [request](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#_Toc31358863) and [response](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#_Toc31358882) headers and response formats. RESO specifically uses an [XML version of OData metadata](http://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752500), which contains an Entity Data Model (EDM) and model definitions and is often referred to as EDMX.

## Metadata and Data Dictionary Validation

Applicants using the RESO Web API will have to pass Data Dictionary 1.7 testing, [as outlined in the specification](https://docs.google.com/document/d/15DFf9kDX_mlGCJVOch2fztl8W5h-yd18N0_03Sb4HwM/edit#heading=h.kzicg1dclst4). 

## Payloads Testing

At a high level, the Payloads testing procedure consists of the following steps:

* Metadata and Data Dictionary testing
* Sampling
* Scoring

### Sampling

Why perform Random Sampling on the server? The reason is that having RESO standard fields in the metadata alone isn’t enough since they could always be null. 

The testing tool will generate queries that can produce thousands of records spread over a representative sample. This will help determine which fields have data in them for both the full-access and representative payload user.

A set of sample queries might look like the following:

```
GET /Property?$filter=ModificationTimestamp ge 2018-01-01 and ModificationTimestamp lt 2018-02-01

GET /Property?$filter=ModificationTimestamp ge 2018-02-01 and ModificationTimestamp lt 2018-03-01

GET /Property?$filter=ModificationTimestamp ge 2018-03-01 and ModificationTimestamp lt 2018-04-01

...

2019, 2020, ..., etc.
```

The RESO Commander will fetch these data and record which fields were found as well as how often they had data in them without storing field data locally.

### Scoring

After data availability records are collected in the sampling process, they will be scored.

Scoring means that data collected during sampling are processed and *field availability* and *data availability* information is calculated for each field found in the payload during the sampling process, including local fields. This is also the information needed to generate reports, which is discussed in the [Reporting](https://docs.google.com/document/d/1hNMqmDdK0C31tKrfdZnHIk1WmJPbAuluV_eJbErddCo/edit#heading=h.4kc3anklrt45) section.

## Payloads Certification

Payloads Certification consists of evaluating the scored results produced in the [Payloads Sampling](https://docs.google.com/document/d/1hNMqmDdK0C31tKrfdZnHIk1WmJPbAuluV_eJbErddCo/edit#heading=h.l3oqsdcr9j2t) section.

### Evaluating Scored Results

#### Example: IDX Payload

Assume the sampling tool was run for the IDX Payload with 10,000 records randomly sampled from each resource found. The *scoring phase* would produce data similar to the following:

| **Field Name**        | **Data Availability RESO (n=10k)** | **Data Availability IDX** **(n=10k)** | **Required** | **Result** |
| --------------------- | ---------------------------------- | ------------------------------------- | ------------ | ---------- |
| ListingKey            | 100%                               | 100%                                  | True         | **PASS**   |
| ListPrice             | 80%                                | 50%                                   | False        | **WARN**   |
| ModificationTimestamp | 100%                               | 10%                                   | True         | **FAIL**   |

The following events occurred during certification:

* **PASS**  - ListingKey was found with both the RESO and IDX user 100% of the time, which meets the field level requirements. The String version of the key is required for IDX Payload resources.
* **WARN** - ListPrice was found 80% of the time using the RESO user and 50% of the time using the IDX user. Warn if the numbers are different for a given field
* **FAIL** - ModificationTimestamp was found 10% of the time for the IDX user but is required. Therefore, IDX Payloads certification fails.



# Reporting

## Data Collection

Metadata for a given server instance will be analyzed by the RESO Commander but not stored locally. Payloads data analysis is done in memory and discarded upon termination of the application so an applicant’s data is not retained.

The RESO Commander will produce summary test statistics and include relevant information such as how often data was found in available resources, fields, and enumerations found during sampling.

## Data Collection Pipeline

Test data will be collected for analytics purposes. This information will be stored on a cloud drive in order to catalog results for later use:

<p align="center">
  <img src="https://user-images.githubusercontent.com/88680702/137548342-c3363b43-65d1-40fa-8cba-23de8580806b.jpg" />
</p>

Once test results are stored, they will be sent to a collector service for analysis and to provide market statistics showing payload and field-level compliance and adoption rates.

## RESO Map

The collector service will drive the reporting portion of the RESO Certification website and [RESO Map](https://www.reso.org/certification/), which shows information about certified applicants in a geographical manner.

The map will be used to display the Payloads endorsements of data providers, will show provider data availability scores, and will link to detailed field-level data availability reports.

The map will also provide an automated reporting system for field and data quality violations directly from the map. 

## Display of Information on RESO Website

RESO may use anonymous sampling statistics and aggregates collected during the certification process for display on its public websites. These items consist of Resource, Field, and Enumeration data availability metrics. 

For example:

* For each discovered resource, how many implementations have that resource?

* For each discovered field, how many implementations have that field?

* For each discovered enumeration, how many implementations have that enumeration?

* For each field in a given RESO Payload, how often are data in that field, on average?

  

## Data Retention Policies

Applicants and certification recipients have the right to be forgotten.

Queries used during testing will be stored in RESOScript files that are automatically generated for the purposes of testing and sharing results with the applicant to confirm. These queries will contain key data but data retrieved during sampling will not be stored.



# Certification Workflow

The Certification workflow has been optimized around self-assessment prior to certification.

## Application

Those seeking RESO Certification will apply for review prior to having their application processed by Certification. Once the applicant's configuration has been verified, they will be instructed to apply for certification. 

## Self Assessment

It is expected that data providers will use the RESO Payloads testing tool **before** applying for certification to ensure that they pass certification. Once they have passed using the automated testing tools, they will forward their configuration to RESO staff for confirmation.

Guides exist to help them with the evaluation process (TODO: create guide).

Any questions regarding certification should be directed to [certification@reso.org](mailto:certification@reso.org).

## Certification Review

A RESOScript file is required for review. This file should contain credentials and the service location of the RESO Web API Server instance to be tested.

## Certification Issuance

Once RESO Certification verifies that a given configuration produces no errors, unhandled warnings, or exceptions, certification will be granted and the applicant will be issued a certification report. The certification report will be posted on the public RESO website.

## Testing Credentials

In both the [Metadata and Data Dictionary Validation](https://docs.google.com/document/d/1hNMqmDdK0C31tKrfdZnHIk1WmJPbAuluV_eJbErddCo/edit#heading=h.evm2d6urqz93) and [Payloads Testing](https://docs.google.com/document/d/1hNMqmDdK0C31tKrfdZnHIk1WmJPbAuluV_eJbErddCo/edit#heading=h.4gek7lensmk2) steps, A/B testing will be done with both: a) the full-access credentials already required by RESO for certification, and b) credentials that MUST match the access level to be used by consumers of the payload.

RESO MAY routinely retest applicants with these credentials without prior notice. If the credentials are found to be invalid or expired, the applicant is expected to provide new credentials upon request within five business days.



# Testing Framework

Payloads testing is provided by the [RESO Commander](https://github.com/RESOStandards/web-api-commander). 

The RESO Commander is an open source, cross-platform Java library created by RESO that uses established community libraries, such as the Apache Olingo OData Client, XML parsers, and JSON Schema Validators, to provide a testing API.

Payloads tests are written in a high-level testing language (DSL) called [Gherkin](https://cucumber.io/docs/gherkin/reference/). This is part of a [Behavior Driven Development](https://en.wikipedia.org/wiki/Behavior-driven_development) (BDD) platform called [Cucumber](https://cucumber.io/), which allows for the expression of testing workflows using a natural language that is intended to be accessible to business analysts, QA testers, and programmers alike.

BDD acceptance tests are automatically generated from the adopted Data Dictionary spreadsheet for a given version and can target any version of the Data Dictionary from 1.0 onwards. The benefit of this strategy is that when a Data Dictionary version is ratified, the tests may be generated and testing can begin right away, which significantly reduces testing tool development time.

A command-line interface has been provided during the initial development phase as an entry point into the testing API. This provides the environment used for certification and self-assessment, and it can even be run on a test automation server in a continuous integration and deployment platform such as [GitHub CI](https://help.github.com/en/actions/language-and-framework-guides/building-and-testing-java-with-gradle), [Jenkins](https://cucumber.io/docs/guides/continuous-integration/), [Travis](https://docs.travis-ci.com/user/languages/java/), or [CircleCI](https://circleci.com/blog/getting-started-with-cucumber-on-circleci/) to help prevent regressions in a RESO-certified codebase.

A graphical user interface (GUI) is also available through popular and free Integrated Development Environment (IDE) plugins for [IntelliJ](https://www.jetbrains.com/help/idea/enabling-cucumber-support-in-project.html) and [Eclipse](https://cucumber.github.io/cucumber-eclipse/). IDEs provide a superior testing platform, as they provide better informational messages and are able to run and debug the entire test suite as well as a given test individually. The availability of plugins saves significant time in testing, development, and certification. The level of community support is one of the reasons Cucumber was chosen as a testing platform.



# Feature Requests

Feature requests can be requested as [issues on the RESO Commander’s GitHub project](https://github.com/RESOStandards/web-api-commander/issues), or by contacting [dev@reso.org](mailto:dev@reso.org) or [certification@reso.org](mailto:certification@reso.org).



# Support 

Certification support will be provided by [certification@reso.org](mailto:certification@reso.org).
Tool support is provided by [dev@reso.org](mailto:dev@reso.org).



# Contributors

Thanks to the following contributors for their help with this project:

* Sergio DelRio
* Rob Larson
* Rick Trevino
* Chris Freeman
* Sam DeBord
* RESO Payloads Workgroup and Certification Subgroup attendees

If you would like to contribute, please contact [dev@reso.org](mailto:dev@reso.org) or [certification@reso.org](mailto:certification@reso.org). This could mean anything from QA or beta testing to technical writing to doing code reviews or writing code.

Also, thanks to FBS for donating prototypes of their BDD acceptance tests to be used as models and to the Austin Board of REALTORS® for donating RESO Web API reference servers.

