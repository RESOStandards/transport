# RESO (Endorsement Name) Endorsement

| **Version** | 2.0.0 |
| :--- | :--- |
| **Submitter** | [Geoff Rispin](mailto:grispin@t4bi.com) |
| **Co-Submitter** | [Geoff Rispin](mailto:your@t4bi.com) |
| **Written** | April 2022 |
| **Ratified** | |
| **RCP** | RCP-041 |
| **Related RCPs** | _None_ |

<br /><br />

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

<br /><br />

# Table of Contents
- [RESO (Endorsement Name) Endorsement](#reso-endorsement-name-endorsement)
- [RESO End User License Agreement (EULA)](#reso-end-user-license-agreement-eula)
- [Table of Contents](#table-of-contents)
- [Summary of Changes](#summary-of-changes)
- [Introduction](#introduction)
- [Section 1: Purpose](#section-1-purpose)
- [Section 2: Specification](#section-2-specification)
- [Section 3: Certification](#section-3-certification)
- [Section 4. Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 7: License](#section-7-license)

<br /><br />

# Summary of Changes

* Allow authorized end users with approved access to access the data through third party solutions without the for the third party to have client credentials issues by the the RESO API oAuth service platform. 

<br /><br />

# Introduction
OAuth allows for multiple authentication methods and the current ones documented require credentials to be directly issued between the RESO API provider and application provider.  By allowing **Authorization Code** oAuth model as optional and in addition to the current supported methods, we could allow users with end users to select third party applications to work with independent of the RESO API provider application integrations and partners. 

<br /><br />

# Section 1: Purpose
There are cases where the relationship between the client and server may only exist through the licensed end user of the WebApi provider.  In those cases, the end user cannot provide secure oAuth access to third party vendors without the WebApi provider being technically involved in the credentials issuance to the client.

<br /><br />

# Section 2: Specification
In all locations where oAuth supported methods are mentioned **Authorization Code** should be added as an optional support authentication path. 

The insdustry documentation is in [RFC 6749](https://tools.ietf.org/html/rfc6749#section-1.3.1)

# Section 3: Certification

The workflow requires interactivity with the oAuth service provider used by the WebApi interface for authentication.  This would a check box validation method as once the Bearer Token is aquired via Authentication Code process all further transport work is done identically as the current standard.

The technical validation process details would have to be discussed.  The fact the Authentication Code workflow starts and redirects to oAuth SP identity validation page, may be sufficient.      

<br /><br />

# Section 4. Contributors
This document was written by [Geoff Rispin](mailto:grispin@t4bi.com).

Thanks to the following contributors for their help with this project:

| Contributor | Company |
| --- | --- |
| Sergio Del Rio | Templates 4 Business |

<br /><br />

# Section 5: References

Please see the following references for more information regarding topics covered in this document:
* [RFC 6749](https://tools.ietf.org/html/rfc6749#section-1.3.1)

<br /><br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO](mailto:info@reso.org) if you have any questions.