# RESO Versioning
RESO uses [**Semantic Versioning (SemVer)**](https://semver.org/) in its specifications, which outlines the following:

> Given a version number MAJOR.MINOR.PATCH, increment the:
>   * **MAJOR** version when making incompatible API changes
>   * **MINOR** version when adding functionality in a backward compatible manner
>   * **PATCH** version when making backward compatible bug fixes

For Data Dictionary specifications, only major and minor versions are used formally (e.g., 1.7 or 2.0). RESO tracks patch-level changes for each revision of the Data Dictionary internally.

Transport includes the patch level in its specifications (e.g., Web API Core 2.0.0) but only uses major and minor versions in practice.

# API Changes and RESO Standards
Whether something is a "breaking change" depends on the lens through which it's viewed.

Removing something from a RESO specification, either in the Data Dictionary or Transport workgroups, means RESO won't test for it anymore in certification. However, providers don't have to remove that item from their APIs if they're still using it unless expressly forbidden, such as disallowed synonyms in the Data Dictionary. 

Similarly, adding tests where they didn't exist before could "break" an API since they might require changes to the current implementation to meet the standard. Policies have been established to avoid this scenario. 

This document focuses primarily on how RESO standards and certification use versioning rather than how systems version their APIs, though there is some overlap. 

# Major Changes
Major changes are also known as "breaking changes" and MUST NOT be implemented until major the version of the specification they apply to is ratified by the RESO Board of Directors. 

## Data Dictionary
The following changes are incompatible with current versions of the specification:
* A standard field has a type change.
* A disallowed synonym is added for a resource, field, or lookup (enumeration).
* An existing enumerated field is closed or items are removed from an existing closed enumeration.
* Duplicate or replacement data elements are added. 
* New testing rules are added to existing data elements, including Fast Track mappings. New testing rules may be added to new elements. 

Examples of type changes are things like a field that's currently a string becoming a number or enumeration or a String List, Single becomes String List, Multi. 

Disallowed synonyms are considered MAJOR because RESO would be forcing providers to remove items from their APIs that may currently be in use, therefore breaking clients. This is different from just deprecating a data element from the standard since providers can still use it in those cases. 

Closing an enumeration means it is no longer open to extension and providers that have options outside of what's in the standard will fail. Removing existing elements from closed enumerations forces deprecation and can break systems and clients if they're using them. 

When duplicate or replacement data elements are added, it causes the standard to diverge between the existing standard and the new one. Those expecting data to be in the current standard location now have to map to many different elements rather than what's in the standard. Providers who want to implement future duplicate or replacement elements may do so, but they must also implement the current standard. For example, a new field called WaterHeater is being added to the Data Dictionary that will replace the existing water heater elements in the Appliances field. This is a major change because the location of the existing elements is being moved. However, if providers want to implement the new WaterHeater field prior to the next major version, they may do so as long as the water heater elements are still available in the Appliances field. Renaming existing elements also falls under this category since the existing elements are being duplicated and replaced by the ones that have been renamed. 

If there's at least one major change in a proposed Data Dictionary version, it will be considered a MAJOR version.

## Transport
Major changes in Transport consist of disallowing previously standard behavior or making significant changes to existing functionality.

For example, RESO Common Format currently requires a `@reso.context` variable. If the Transport Workgroup were to change this to something else _and_ force deprecation, or change the type of this value from string to a nested object, it would constitute a MAJOR version. 

Another example: RESO is planning to deprecate OData multi-select enumerations in the future. If any are found on a RESO Web API server during testing at that point, certification will fail.

Similar to the Data Dictionary, functionality may be added to or removed from Transport specifications without causing breaking changes since providers can choose to keep their APIs the same. However, any forced deprecations or changes to data types or potential type mappings count as MAJOR.

# Minor Changes
Minor changes preserve compatibility with current Data Dictionary and Transport specifications. These are usually additions or deprecations.

## Data Dictionary
* Data elements such as resources, fields, or lookups (enumerations) are added to or removed from the specification.
* There are changes to classifications of PropertyType and PropertySubType groupings.

When items are added to or removed (deprecated) from the specification, providers don't have to make changes to their systems to reflect those changes.

Information about new additions and deprecated elements is available in the [**DD Wiki change log**](https://ddwiki.reso.org/display/DDW20/Change+Log+Summary) for each version and also included in new draft Data Dictionary specifications, such as [**DD 2.1**](https://docs.google.com/spreadsheets/d/1xKER4mvGwn9jeHQK86_fovuiCKnPQOmaLTiMw1qLWsU/edit?gid=889323793#gid=889323793).  

When data elements are deprecated from the RESO Data Dictionary, providers may still use them in their system as long as they don't become disallowed synonyms or are part of an enumeration that is closed. 

It's important to note that RESO does not validate related PropertyType and PropertySubType usage. There was some discussion about doing so in Certification, however the community felt it wasn't necessary at this time. As such, providers may not implement these relationships as intendend. Since systems and end users do utilize these fields in practice, however, changes to them have some impact. Therefore they are considered minor and are planned rather than being patches to the current version. 

## Transport
Minor changes in Transport usually mean adding something to the API specifications, such as a new kind of query or server-driven paging. Clients don't have to support those queries or new kinds of paging unless we force deprecation of the prior standard, which is uncommon. With RESO Common Format, changes are automatically accounted for as part of the corresponding Data Dictionary release and the version is communicated using the `@reso.context` variable so end users can track them.

# Patches
Patches are backwards compatible bug fixes. This usually means corrections to terms or wording in a specification, and includes changes to certain columns in the Data Dictionary reference sheets.

## Data Dictionary
Patches include changes to any of the following columns in the [**Data Dictionary specification**](https://docs.google.com/spreadsheets/d/1eOB4Nv3wrAayB1av7n2AWPBRWDeB-UkiDa8h8cdsIEI/edit?gid=1912290910#gid=1912290910):

* BEDES
* Definition\*
* DisplayName
* FrenchCanadianDisplayName
* FrenchCanadianLookupValue
* Groups
* LegacyLookupId
* LegacyLookupName
* LegacyRecordId
* LookupId
* LookupName
* LookupNameId
* Payloads
* RecordId
* SpanishDisplayName
* SpanishLookupValue
* SugMaxLength
* SugMaxPrecision
* WikiPageId
* WikiPageTitle
* WikiPageUrl

<br />

\***Definition changes** that don't materially change what a data element represents are considered patches. Changes to definitions that significantly change the meaning of a data element may result in a MAJOR or MINOR change.

Suggested max precision and length are considered patches since they're suggestions and will not result in failure during certification. If the attributes used by the provider differ from the suggested values, they will receive an informational message. 

At the API level, changing length, scale, or precision is a breaking change and providers should inform end users when doing so. 

## Transport
Patch-level changes in Transport are things like clarification of the specification, adding new examples or references, updating links, or fixing spelling and grammar.

Any changes to current testing rules are not patches and will be considered major or minor revisions.

---

_Last updated on October 21, 2025._

Questions? Please [**contact RESO**](mailto:transport@reso.org).

