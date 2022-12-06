# RESO Validation Expressions

| **RCP** | 19 |
| :--- | :--- |
| **Version** | 19.1 |
| **Authors** | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org))<br />Paul Stusiak ([Falcon Technologies](mailto:pstusiak@falcontechnologies.com)) |
| **Specification** | [**LINK TO RCP**](./web-api-validation-expression.md) |
| **Status** | **DRAFT** |
| **Date Approved** | April 2018 |
| **Dependencies** | [Validation Expression grammar](https://github.com/darnjo/rcp019) <br /> [Data Dictionary 1.7+](./data-dictionary.md)<br />[Web API 2.0.0+](./web-api-core.md)<br /> |
| **Related Links** | [DD Wiki 1.7](https://ddwiki.reso.org/display/DDW17/RESO+Data+Dictionary+1.7)<br />[Data Dictionary Spreadsheet](https://docs.google.com/spreadsheets/d/1_59Iqr7AQ51rEFa7p0ND-YhJjEru8gY-D_HM1yy5c6w/edit?usp=sharing)<br /> |

RESO Validation Expressions build on the RETS Validation Expression grammar to provide a familiar, intermediate representation of computable business rules. The RCP-019 proposal consists of a Rules resource for transport and a grammar. 

[**RCP-019 Proposal in PDF format**](https://github.com/RESOStandards/reso-transport-specifications/files/8384860/RESOWebAPIRCP-RCP-WEBAPI-019ValidationExpressionintheWebAPI-300322-2353.pdf)


# Rules Resource
The [Rules Resource](https://ddwiki.reso.org/display/DDW17/Rules+Resource) has been added to Data Dictionary 1.7+ to provide a transport mechanism.


# Rules Grammar
The RESO Validation Expression grammar assumes that rules are ordered sequentially, and will be process in order when dependencies require it. Not all rules are dependent on each other. Both independent and dependent rules can be parallelized, but care must be taken in the latter case to avoid data hazards (out of order operations).

The Validation Expression grammar is not Turing complete by default. It requires the use of custom functions to iterate or recurse over a potentially unbounded collection of items. In other words, rule sets are always guaranteed to terminate. This is a good thing.

Some examples from the current [RCP-019 grammar](https://github.com/darnjo/rcp019), which uses the [ANTLR4 Format](https://www.antlr.org/):

### Basic Examples

### Arithmetic Operators
`3 + 5`

`1 * 3 + 2 - 5`

### Bracketed RETSNAMEs  
`[ListPrice] > 0`

### RCP61 added support for unbracketed RETSNAMEs. This also works for DICTNAMEs.
`ListPrice > 0`

### To get current value of any field, just use the `DICTNAME` of that field  
`ListPrice`

### Access the last value of a field

`LAST Status != "Sold"`

### Field change detection
`ListPrice != LAST ListPrice`

### Function calls are also supported
`foo()`  
`foo ('bar', 'baz', 42)`

_Cannot use any RETSNAMEs, DICTNAMEs, or reserved words_

### Lists of expressions
`(ListPrice, Status, 3+5, LAST Status, (1, 2, 3))`

### Supports complex expressions with grouping  
`ListPrice > 5.01 .AND. (1, 2, 3) .CONTAINS. 3 .OR. (Status .IN. ('Active', 'Pending') .AND. .USERLEVEL. != "Admin")`

### Which parses differently than  
`ListPrice > 5.01 .AND. (1, 2, 3) .CONTAINS. 3 .OR. Status .IN. ('Active', 'Pending') .AND. .USERLEVEL. != "Admin"`
    
<br />

## Collection Support
RCP-019.1 added support for collections, which include `LIST()` and `SET()`. 

These functions take 0 or more items.

Backwards compatibility with the previous list syntax of `()` in RCP-019 has been preserved.

| Expression  | Result |  Comments |
| :--- | :--- | :--- |
|`LIST(1, 2, 2, 3)`|`LIST(1, 2, 2, 3`| Duplicate items are _preserved_.|
|`SET(1, 2, 2, 3)`|`SET(1, 2, 3)`| Duplicate items are _removed_.|

<br />

## DIFFERENCE(), INTERSECTION(), and UNION()  
RCP-019.1 also added support for `DIFFERENCE()`, `INTERSECTION()`, and `UNION()`,
which are intended to produce types in the following manner:
  * Operations on _homogeneous_ collections of `LIST()` or `SET()` should 
    return `LIST()` or `SET()`, respectively. For example,
    _`UNION(LIST('a', 'b', 3+5), LIST(6))`_ should return _`LIST('a', 'b', 6, 8)`_.
  * Operations on _heterogenous_ collections of `LIST()` or `SET()`, for 
    instance _`DIFFERENCE(LIST(1, 2, 3), SET(3))`_ should return _`LIST()`_.      

These special functions require at least two arguments of type `LIST()` or `SET()`:
 
 | Expression  | Result |  Comments |
 | :--- | :--- | :--- |
 |`DIFFERENCE(LIST(1, 2, 3), LIST(1, 2))`|`LIST(3)`|Collection operators require two or more `LIST()` or `SET()` arguments.|
 |`UNION(LIST(1, 2), SET(3))` |`LIST(1, 2, 3)`|Arguments of type `LIST()` are converted to `SET()`.   |
 |`INTERSECTION(SET(DIFFERENCE(LIST(1, 2, 3), SET(1))), SET(2))`|`SET(2)`|Since the return type of `collection` operators is `SET()` or `LIST()`, they can be composed.|
