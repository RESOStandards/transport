# RESO Versioning
RESO uses [**SemVer**](https://semver.org/) in its specifications, which outlines the following:

> Given a version number MAJOR.MINOR.PATCH, increment the:
> 
> * MAJOR version when making incompatible API changes
> * MINOR version when adding functionality in a backward compatible manner
> * PATCH version when making backward compatible bug fixes

For Data Dictionary specifications, only the major and minor versions are used formally at the moment. For example, 1.7 or 2.0. RESO tracks patch-level changes behind the scenes and has a commit for each version of each DD spec. 

Transport includes the patch level as well (e.g. Web API Core 2.0.0) but only uses major and minor versions in practice. 

# API Changes and RESO Standards
Whether something is a "breaking change" depends on the lens it's viewed through.

Removing something from a RESO specification, either in the Data Dictionary or Transport, means RESO won't test for it anymore. 
But, providers don't have to remove it from their APIs unless expressly forbidden, such as disallowed synonyms in the Data Dictionary. 

Similarly, adding tests where they didn't exist before could "break" an API since they might require changes to the current implementation to meet the standard. 

This document focuses on how RESO standards and certification uses versioning rather than how developers might on an API.

<br />

# Major Changes
Major changes are incompatible changes to a specification. 

## Data Dictionary
The following changes are incompatible with current versions of the specification:
* A standard field has a type change.
* A disallowed synonym is added for a resource, field, or enumeration.
* An enumerated field is _closed_, meaning items outside of that enumeration will fail during testing.

Examples of type changes are things like a field that's currently a string becoming a number or enumeration. Unlike [**minor changes**](#minor-changes), future type changes cannot be adopted until that version of the Data Dictionary without breaking the API. 

Disallowed synonyms are considered major because RESO is forcing providers to remove items that may currently be used from their API, which is different from just deprecating a data element from the standard since providers can still use it in those cases. 
Closing an enumeration is a forced deprecation on any of the items not currently defined and providers must remove them from their system and map to the standard values. 

If there's at least one data element with a major change in a given Data Dictionary version, the specification will be classified as major.

## Transport
Major changes in Transport consist of disallowing previously standard behavior or making significant changes to existing functionality. 

For example, RESO Common Format currently requires a `@reso.context` variable. If Transport and Certification were to change this to something else and force deprecation or change the type of this value from string to a nested object, that would be a major change. 
Another example is that RESO is planning on forcing the deprecation of OData multi-select enumerations in the future. If any are found on a RESO Web API server during testing, certification will fail. 

Similar to the Data Dictionary, changes might occur but any forced deprecations or changes to data types or potential type mappings count as major changes. 

<br />

# Minor Changes
Minor changes are future changes which preserve backward compatibility with respect to the Data Dictionary and Transport specifications. These are usually additions or deprecations.

## Data Dictionary
* Resources, fields, or enumerations are added or removed from the specification.
* Changes to the names of existing data elements as long as the old ones aren't disallowed.
* Changes to classifications of PropertyType and PropertySubType groupings.

When items are added or removed from the specification, providers don't have to make changes on their current systems to reflect those changes. In some cases it's possible that adding something to a future version of the 
Data Dictionary will cause a current local data element to fail in certification. However, if the provider has both the standard and local value they will pass testing in those cases. This is also the case when 
something is renamed in the Data Dictionary. 

RESO does not validate related PropertyType and PropertySubType usage at the moment, but has plans to in a future version of the Data Dictionary with warnings beginning in version 2.1. These items are used often enough
in practice that they're not just a patch and should wait until the next version to be applied. 

## Transport
Minor changes in transport usually mean adding something to the API specifications, such as a new kind of query or server-driven paging. Clients don't have to support those queries or new kinds of paging 
unless we force deprecation of the prior standard, which is uncommon. With RESO Common Format, the versioning is handled by the Data Dictionary rules. 

<br />

# Patches
Patches are backwards compatible bug fixes. Usually this means corrections to terms or wording in a specification, including certain columns in the Data Dictionary reference sheets. 

## Data Dictionary
Patches include changes to any of the following items in the Data Dictionary specifications: _DisplayName, Definition, Groups, SugMaxLength, SugMaxPrecision, FrenchCanadianDisplayName, SpanishDisplayName, RecordId, LegacyRecordId, LookupName, Payloads, WikiPageTitle, WikiPageUrl, WikiPageId, BEDES, FrenchCanadianLookupValue, SpanishLookupValue, LookupNameId, LookupId, LegacyLookupId, LegacyLookupName_

Suggested max precision and length are included here since they're just suggestions and won't result in issues during certification. If the attributes used by the provider differ from the suggested values they will receive an informational message. Therefore, any corrections to these values may be made within the current version.

## Transport
Patch-level changes in Transport are things like clarification of the specification, adding new examples or references, and fixing spelling and grammar.

Any changes to current testing rules are not patches and would be either [**major**](#major-changes) or [**minor**](#minor-changes) changes. 
 
---

Questions? Please [**contact RESO**](mailto:dev@reso.org).
 
