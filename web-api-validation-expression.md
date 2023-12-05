# RESO Validation Expressions

| **RCP** | 019 |
| :--- | :--- |
| **Version** | **1.0.0** |
| **Authors** | [Joshua Darnell](mailto:josh@kurotek.com)<br />[Paul Stusiak](mailto:pstusiak@falcontechnologies.com) |
| **Specification** | [**LINK TO RCP**](./web-api-validation-expression.md) |
| **Status** | **Approved** |
| **Status Date** | December 2023 |
| **Dependencies** | [Data Dictionary 1.7+](./data-dictionary.md)<br />[Validation Expression Grammar](./artifacts/grammars/RCP-019/rcp019.g4) |
| **Related Links** | [DD Wiki 1.7](https://ddwiki.reso.org/display/DDW17/RESO+Data+Dictionary+1.7)<br />[Data Dictionary Spreadsheet](https://docs.google.com/spreadsheets/d/1_59Iqr7AQ51rEFa7p0ND-YhJjEru8gY-D_HM1yy5c6w/edit?usp=sharing) |

<br /><br />

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

<br />

# Table of Contents
- [Introduction](#introduction)
- [Section 1: Purpose](#section-1-purpose)
- [Section 2: Specification](#section-2-specification)
- [Section 3: Certification](#section-3-certification)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: Appendices](#section-6-appendices)
- [Section 7: License](#section-7-license)

<br />

# Introduction
Business rules are an important, but challenging, topic and come in many forms. There are validation rules (like requiring list price being greater than 0) and process rules (such as requiring a contract date when a listing is sold). There are also display rules, such as additional elements being shown and/or required based on the current state of a record.

These rules come into play whenever a record is created, updated, or deleted. Display rules can also apply to readonly access to that data in an API or UI.

The goal of this specification is to provide a machine-executable business rules grammar, as well as a transport mechanism to convey rules. Another goal is that the rules themselves be human friendly. 

Executable business rules also provide a way to document a system's rules in a way that's both vendor and technology agnostic.

<br />

# Section 1: Purpose
The ability to express and exchange machine-executable business rules has several uses. For example, providing immediate feedback to users when adding a record to a system from their own frontend of choice, without calling back to a vendor system for validation. They can also be used in back-office systems so they can push records back to an MLS or other system for publication (disconnected edit).

The RESO Web API is built on OData, which does not provide machine-executable rules at this time. However, this is something RETS previously offered. 

After much discussion and several onsite meetings during 2016-2018, the community felt the best direction was to carry the work done in RETS forward to the RESO Web API. It had a few issues that needed to be addressed, as well as a need for a transport mechanism, but the grammar presented in this specification should mostly have backwards compatibility with what had come before for those who might have implemented it.

While there is still more work to do, such as the addition of a scope resolution operator to support rules for expanded data elements and new functions and special operations, it will be addressed in a future version.

<br />

# Section 2: Specification
This specification consists of the following:
* [Rules grammar](#section-212-validation-expression-bnf) expressed in [BNF](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form)
* [Rules Resource](https://ddwiki.reso.org/display/DDW17/Rules+Resource) to provide transport of business rules 


## Section 2.1: Validation Expression Language

The [RESO Validation Expression Grammar](./artifacts/grammars/RCP-019/rcp019.g4) is used in conjunction with a data structure of `FieldName`, `RuleAction`, `RuleExpression` tuples to provide an ordered list of instructions that can be used to convey business rules in a system independent way.

These rules are intended to be written from the perspective of the business object being upserted, such as a listing record in the Property Resource, and would be run as-needed before the client sends the record back to the system using the [RESO Add/Edit](./web-api-add-edit.md) specification.

For a client running the rules locally, making a change to the StandardStatus field would trigger rules relevant for that field (along with any related rules). 

Expressions a given upsert process MUST be evaluated in the order that they appear in the [RuleOrder](https://ddwiki.reso.org/display/DDW17/RuleOrder+Field) field.

While all rules may be executed each time data changes, clients SHOULD try and optimize execution so that only rules related to a given change are fired rather than all of them. There are a number of techniques that can be used to accomplish this, such as [instruction scheduling](https://en.wikipedia.org/wiki/Instruction_scheduling) or the [Rete algorithm](https://en.wikipedia.org/wiki/Rete_algorithm). 

Rule optimization is beyond the scope of this document, but it's helpful that there are existing techniques to address it.

### Section 2.1.1: Actions

The following actions are defined by this specification:

| Keyword | Type    | Purpose |
| ------- | ------- | ------- |
| **ACCEPT**  | `BOOLEAN` | If the expression is true, the field value is considered accepted without further testing. Immediately following SET expressions MUST be executed. If the expression is false, following validation expressions MUST be executed. If the expression is ERROR (evaluation failed) in client, the client SHOULD act as if the field was accepted, allowing the server to make the final decision. |
| **REJECT**  | `BOOLEAN` | If the expression is true, the field value is considered rejected without further testing. Subsequent SET expressions MUST NOT be evaluated. If the expression is false, following validation expressions MUST be executed. If the expression is ERROR, evaluation failed in client, the client SHOULD act as if the field was accepted, allowing the server to make the final decision. |
| **WARNING** | `BOOLEAN` | If the expression is true, the client should show a warning message to the user, and if the warning is okayed by the user, include a Warning-Response in the UPDATE request. If the user does not okay the warning, the field is considered rejected and following SET validation expressions MUST NOT be evaluated. <br /><br /> If the expression is false, the following validation expressions MUST be evaluated. |
| **SET** | `TYPEOF(Exp)` | The following expression is evaluated and the result stored in the designated field. |
| **SET_DEFAULT** | `TYPEOF(Exp)` | This expression MUST be executed ONLY when a NEW record is created. Supersedes the default value as indicated in the Update Metadata. |
| **SET_REQUIRED** | `BOOLEAN` | Expressions of this type are designed to evaluate an expression and set the field that the rule is applied on to Required if the expression returns true and to Non Required if the expression returns false. |
| **SET_READ_ONLY** | `BOOLEAN` | Expressions of this type are designed to evaluate an expression and set the field that the rule is being applied on to Read Only if the expression returns true and to updatable if the expression returns false. <br /><br /> The client application is expected to lock the value of the field the rule is being executed on to the value at the time the SET_REQUIRED expression is evaluated. |
| **RESTRICT_PICKLIST** | `LIST(CHAR)` | Expressions of this type are designed to return one or more lookup values that are to be removed from the lookup list that is defined for the field the rule is being executed on. This is always the entire set of values to remove from the lookup. <br /><br />In other words, if this returns a blank list or `.EMPTY.`, the entire set of lookup values is to be displayed.<br /><br />The value of this expression MUST be a `LIST`, rather than `Exp`. <br /><br />All members of the list MUST be of the same data type as that of the field the rule is being executed on. |
| **SET_PICKLIST** | `LIST(CHAR)` | Expressions of this type are designed to return one or more lookup values that are to be used in the lookup list defined for the field the rule is being executed on.<br /><br />The value of this expression MUST be a `LIST`, rather than `Exp`. <br /><br />Every member of the list MUST exist in the lookup list as defined in the metadata for the field the rule is being executed on. |
| **SET_DISPLAY** | `BOOLEAN` | Expressions of this type are designed to allow a client to make fields visible or invisible based on the evaluation of an expression. The result of this expression has no effect on whether a field is READ ONLY or not. |

### Section 2.1.2: Validation Expressions

There are two files that define the Validation Expressions grammar:
* [ANTLR 4 grammar](./artifacts/grammars/RCP-019/rcp019.g4) - Contains [non-terminal symbols](https://en.wikipedia.org/wiki/Terminal_and_nonterminal_symbols#Nonterminal_symbols)
* [ANTLR 4 lexer](./artifacts/grammars//RCP-019/rcp019Lexer.g4) - Contains [terminal symbols](https://en.wikipedia.org/wiki/Terminal_and_nonterminal_symbols#Terminal_symbols)

#### Basic Examples

##### Arithmetic Operators
`3 + 5`

`1 * 3 + 2 - 5`

##### FIELD_NAME
To get current value of any field, use the Data Dictionary name: `ListPrice`

##### Bracketed FIELD_NAMEs  
Supported for backwards compatibility with RETS: 
`[ListPrice]`

##### Simple Comparisons
`ListPrice > 0`

##### Access the Last Value of a Field
`LAST Status != 'Sold'`

##### Field Change Detection
Returns true if ListPrice has changed.

`ListPrice != LAST ListPrice`

##### Function Calls
`foo()`  
`foo('bar', 'baz', 42)`

_Functions MUST NOT use any FIELD_NAMEs or reserved words._

##### Lists of Expressions
`(ListPrice, Status, 3+5, LAST Status, (1, 2, 3))`

##### Complex Expressions with Grouping  
`ListPrice > 5.01 .AND. (1, 2, 3) .CONTAINS. 3 .OR. (Status .IN. ('Active', 'Pending') .AND. .MEMBER_MLS_SECURITY_CLASS. != 'Admin')`

##### Which parses differently than  
`ListPrice > 5.01 .AND. (1, 2, 3) .CONTAINS. 3 .OR. Status .IN. ('Active', 'Pending') .AND. .MEMBER_MLS_SECURITY_CLASS. != 'Admin'`
    
##### Collection Support
This proposal adds support for `LIST()` and `SET()`.

These functions take 0 or more items.

| Expression  | Result |  Comments |
| :--- | :--- | :--- |
|`LIST(1, 2, 2, 3)`|`LIST(1, 2, 2, 3`| Duplicate items are _preserved_|
|`SET(1, 2, 2, 3)`|`SET(1, 2, 3)`| Duplicate items are _removed_|

<br />

##### List and Set Difference, Intersection, and Union
This proposal also adds support for the following collection operators:
* `DIFFERENCE()`
* `INTERSECTION()`
* `UNION()`

Which are intended to produce types in the following manner:
  * Operations on _homogeneous_ collections of `LIST()` or `SET()` should 
    return `LIST()` or `SET()`, respectively. 
      * Example: `UNION(LIST('a', 'b', 3+5), LIST(6))` should return `LIST('a', 'b', 6, 8)`
  * Operations on _heterogenous_ collections of `LIST()` or `SET()` should return `LIST()`
      * Example: `DIFFERENCE(LIST(1, 2, 3), SET(3))` should return `LIST(1, 2)`    

These special functions require _at least two_ arguments of type `LIST()` or `SET()`:
 
 | Expression  | Result |  Comments |
 | :--- | :--- | :--- |
 | `DIFFERENCE(LIST(1, 2, 3), LIST(1, 2))` | `LIST(3)` |Collection operators require two or more `LIST()` or `SET()` arguments.|
 | `UNION(LIST(1, 2), SET(3))` | `LIST(1, 2, 3)` | Arguments of type `LIST()` are converted to `SET()` |
 | `INTERSECTION(SET(DIFFERENCE(LIST(1, 2, 3), SET(1))), SET(2))` | `SET(2)` |Since the return type of `collection` operators is `SET()` or `LIST()`, they can be composed

## Section 2.2: Rules Resource
The Validation Expression grammar itself is not enough to compute rules. Additional information is needed.

The [Rules Resource](https://ddwiki.reso.org/display/DDW17/Rules+Resource) has been created in Data Dictionary 1.7+ to transport business rules. 

At a minimum, these consist of the following:
* [**FieldName**](https://ddwiki.reso.org/pages/viewpage.action?pageId=1130838): A standard or local field to effect with the outcome of _expression_
* [**RuleAction**](https://ddwiki.reso.org/display/DDW17/RuleAction+Field): One of the Actions in [Section 2.1.1](#section-211-actions)
* [**RuleExpression**](https://ddwiki.reso.org/display/DDW17/RuleExpression+Field): The Validation Expression using the grammar [defined in Section 2.1.2](#section-212-validation-expressions)

### Example: Update PurchaseContractDate when Listing is Closed

| FieldName                   | RuleAction   | RuleExpression |
| --------------------------- | ------------ | -------------- |
| **PurchaseContractDate**    | `SET`        | `IIF(LAST StandardStatus != 'Closed' .AND. StandardStatus = 'Closed', #2023-12-04#, NULL)` |

It's also important that each rule contains human-friendly messages describing the outcome, when appropriate.

## Rule Ordering
Rule ordering is important. Dependent rules MUST be executed in the order appropriate for their dependencies.

Consider the basic operations: 

```
a = 1
b = 1 + a
c = a + b
```

If `a = 1` isn't executed first, then the second instruction will throw an error and the output won't be available to the assignment of `c`.

However, if we had another expression `d = 0`, it could be executed in any order since there are no dependencies on any of the other items.

The Rule Resource includes a [`RuleOrder`](https://ddwiki.reso.org/display/DDW17/RuleOrder+Field) property so ordering may be preserved.

## Rule Versioning
Sometimes it might be necessary to communicate that a given rule has changed.

The Rules Resource providers the [Rule Version](https://ddwiki.reso.org/display/DDW17/RuleVersion+Field) for these scenarios.

## Filtering by FieldName and RuleAction
Consider the case where a client is interested in rules that might reject changes to ListPrice.

These can be retrieved using the following request to the Rules Resource:

**REQUEST**
```
GET /Rules?$select=RuleKey,FieldName,RuleAction,RuleExpression,RuleWarningText,RuleVersion&$filter=FieldName eq 'ListPrice' and RuleAction eq 'REJECT'
```

**RESPONSE**
```json
{
  "@odata.context": "/Rules?$select=RuleKey,FieldName,RuleAction,RuleExpression,RuleWarningText,RuleVersion&$filter=FieldName eq 'ListPrice' and RuleAction eq 'REJECT'",
  "value": [{
    "RuleKey": "abc123",
    "FieldName": "ListPrice",
    "RuleAction:": "REJECT",
    "RuleExpression": "ListPrice GT LAST ListPrice * 2",
    "RuleWarningText": "ListPrice was greater than two times the original list price. Are you sure?",
    "RuleVersion": "1.234"
  }]
}
```

<br />

# Section 3: Certification
Testing rules consists of the following steps:
* Retrieve rules from the [Rules Resource](https://ddwiki.reso.org/display/DDW17/Rules+Resource) or having them provided in [RESO Common Format](./reso-common-format.md), and validating they're in the correct format 
* Parse the [Validation Expression grammar](./artifacts/grammars/RCP-019/rcp019.g4) to make sure it's correct
  * Will generate a parser from the grammar and host it as a service
  * Add more human-friendly error messages
* Make local changes to a record and validate both success and failure scenarios
  * Those being tested will provide several tests for both scenarios, along with the expected output
  * Testing tools will run the rule set and ensure the output matches
* Test that client can send record to test server with validation rules failing with initial values, then succeeding with known-good values, and the record being upserted correctly in that system (Optional)

Certification tools will be developed so a user can perform all these steps in an interactive manner through both a CLI or UI.

<br /><br />

# Section 4. Contributors
This document was rewritten from the original by [Joshua Darnell](mailto:josh@kurotek.com), and originally written by Joshua Darnell and [Paul Stusiak](mailto:pstusiak@falcontechnologies.com).

Parts of this proposal are based on the RETS Validation Expression language as expressed in RETS 1.9 and RCP 61. Thanks to Libor Viktorin, Mark Sleeman, Sergio Del Rio, and Paul Stusiak for that work. Thanks to Rob Larson for his collaboration on the Rules Resource proposal and also to Bryan Burgers and Zenlist for providing additional feedback during implementation.

<br /><br />

# Section 5: References

Please see the following references for more information regarding topics covered in this document:
* [Rules Resource [DD WIki]](https://ddwiki.reso.org/display/DDW17/Rules+Resource)
* [Original RCP-019 Proposal [Confluence - Login Required]](https://reso.atlassian.net/wiki/spaces/RESOWebAPIRCP/pages/2250178749/RCP+-+WEBAPI-019+Validation+Expression+in+the+WebAPI)
* [PDF of original RCP-019 Proposal](https://github.com/RESOStandards/reso-transport-specifications/files/8384860/RESOWebAPIRCP-RCP-WEBAPI-019ValidationExpressionintheWebAPI-300322-2353.pdf)
* [RESO Validation Expression Grammar](./artifacts/grammars/RCP-019/rcp019.g4)
* [RESO Common Format](./reso-common-format.md)
* [Formal Grammar [Wikipedia]](https://en.wikipedia.org/wiki/Formal_grammar)
* [Backus-Naur form (BNF) [Wikipedia]](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form)
* [ANTLR 4 Documentation](https://www.antlr.org/)



<br /><br />

# Section 6: Appendices

None at this time.

<br /><br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO](mailto:info@reso.org) if you have any questions.
