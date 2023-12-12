## Introduction

This document specifies the RETS VE language contained in the RCP19 Business Rules specification.

This document is intended for rule engine implementors.

## Characterization

The RETS VE language is a language that is designed from the ground up to execute valiation and processing business rules for use by real estate MLSes and their vendors.

The language is composed entirely of expressions. Unlike more common general purpose languages, there are no statements in the language, and there is no iteration.

Each validation expression consists of a top-level expression, potentially composed of sub-expressions. Evaluating a validation expression always reults in a single value.

## Types

Every expression in the language returns a value of one of the following types.

### `EMPTY`

The `EMPTY` type means that there is no value for the given expression. This is similar to `null` or `None` in other languages.

The `EMPTY` type can be constructed using the `.EMPTY.` literal, and also frequently occurs when referencing a resource's field that does not have a value.

### `BOOLEAN`

The `BOOLEAN` type is a type that represents one of two values: true or false.

The `BOOLEAN` type can be constructed using the `.TRUE.` and `.FALSE.` literals, can be obtained from a resource's field directly, and is also the result of many binary comparison operations.

### `FLOAT`

The `FLOAT` type is a type that represents a floating point number. The exact precision is not specified but suggested to be at least equivalent to IEEE 754 64-bit floating point.

### `INTEGER`

The `INTEGER` type is a type that represents an integral number. The exact precision is not specified but suggested to at least be capable of representing the range `-(2^53 - 1)`..`(2^53 - 1)` – the JavaScript save integers.

### `CHAR`

The `CHAR` type is a string of bytes in the form of a UTF-8 string.

### `DATE`

The `DATE` type represents a date in the Gregorian calendar. No timezone is attached.

`DATE`s can typically be found on a resource's field when an agent has entered the date by hand.

### `TIMESTAMP`

The `TIMESTAMP` type represents a particular timestamp on the Gregorian calendar. A timezone may or may not be attached.

`TIMESTAMP`s can typically be found on a resource's field for system-generated timestamps.

`TIMESTAMPS` SHOULD be able to represent time down to the millisecond.

### `OBJECT`

The `OBJECT` type represents a map from `CHAR` keys to any type of value.

This specification provides no way to *create* an `OBJECT` and very few ways to interact with an object. However, the specification includes this type because resource data may include `OBJECT`s or `LIST`s of `OBJECT`s.

For any expression that depends on equality and comparison (e.g. the `.IN.` operator, the `=` and `!=` operators, the `UNION` function), all objects evaluate as not equal, regardless of their contents.

### `LIST`

The `LIST` type represents a collection of values. This collection may be homogenous – for example, a list of `INTEGER`s – or it may be heterogenous – e.g. a list that contains `INTEGER`s, `CHAR`s, `BOOLEAN`s, and even other heterogenous lists.

Equality and comparison of two lists is defined as the piecewise comparison of their values. Lists that contain values of different types may result in an error.


## Expressions

A valid validation expression MUST be exactly one expression, potentially composed of multiple sub-expressions.

Valid expressions are defined below.

The exact grammar of the valid expressions is defined in the ANTLR4 grammar specifications provided in this specification.


### Literals

Literals are constructs in the grammar that return exactly the value that is literally written in the language.

#### Integer literal

An integer literal corresponds to the `INTEGER` type.

An integer literal is an optional plus or minus, followed by a non-zero number of digits. This parses as a decimal number.

Examples:

| Expression | Description |
|:--|:--|
| `0` | The number representing zero |
| `73` | The number representing seventy-three |
| `-258` | A literal using the a leading minus |
| `+19` | A literal using a leading plus. This is equivalent to the expression `19` |

#### Float literal

A float literal corresponds to the `FLOAT` type.

A float literal is any integer, followed by a `.` and a non-zero number of fractional digits.

Examples:

| Expression | Description |
|:--|:--|
| `0.123` | A positive float between zero and one |
| `73.5` | A positive float |
| `-258.7134` | A negative float |
| `+19.35` | A float using a leading `+` |

#### String literal

A string literal corresponds to the `CHAR` type.

A string literal can be any string delimited by either apostrophes or double-quotes.

A string literal delimited by apostrophes *MUST NOT* contain any apostrophes within it.

A string literal delimited by double-quotes *MUST NOT* contain any double-quotes within it.

A string literal of either type *MAY* contain literal newlines, carraige returns, tabs, etc.

Examples:

| Expression | Description |
|:--|:--|
| `"This is a string"` | A literal string delimited by double-quotes |
| `'This is also a string'` | A literal string delimted by single-quotes |

Note: it is not possible to create a string that contains both single and double quotes using a single literal. Using the concatenation operator (`||`) allows creating a string of this type.

#### Boolean literal

A boolean literal corresponds to the `BOOLEAN` type.

There are exactly two boolean literals:

- `.TRUE.` - a value that is true
- `.FALSE.` - a value that is false

#### Empty literal

An empty literal corresponds to the `EMPTY` type.

There is exactly one empty literal:

- `.EMPTY.`

#### Date literal

A date literal corresponds to the `DATE` type.

A date literal starts with a `#`, includes an [RFC3339 `full-date`], and finishes with a `#`.

Examples:

| Expression | Description |
|:--|:--|
| `#1985-04-21#` | The date representing April 21, 1985 |
| `#2012-01-06#` | The date representing January 6, 2012 |
| `#2014-12-04#` | The date representing December 4, 2014 |

[RFC3339 `full-date`]: https://www.rfc-editor.org/rfc/rfc3339#section-5

#### Timestamp literal

A timestamp literal corresponds to the `TIMESTAMP` type.

A timestamp literal starts and finishes with a `#`. Within the `#` the timestamp must be a [RFC3339 `date-time`]. Additionally, as a restriction on the RFC3339 format, the `T` and the `Z` *must* be uppercased.

Examples:

| Expression | Description |
|:--|:--|
| `#1985-04-12T23:20:50.52Z#` | This represents 20 minutes and 50.52 seconds after the 23rd hour of April 12th, 1985 in UTC.[^ts] |
| `#1996-12-19T16:39:57-08:00#` | This represents 39 minutes and 57 seconds after the 16th hour of December 19th, 1996 with an offset of -08:00 from UTC (Pacific Standard Time).  Note that this is equivalent to `#1996-12-20T00:39:57Z#` in UTC.[^ts] |

[^ts]: Both of the examples in the timestamp literal section are tken directly from [RFC3339].

[RFC3339 `date-time`]: https://www.rfc-editor.org/rfc/rfc3339#section-5
[RFC3339]: https://www.rfc-editor.org/rfc/rfc3339#section-5

#### List literal

List literals corresponds to the `LIST` type.

A list is formed by enclosing zero, two, or more than two expressions within parenthesis separated by commas.

Examples:

| Expression | Description |
|:--|:--|
| `()` |  an empty list |
| `(1, 2)` | a list with two `INTEGER` values |
| `("Active", "Pending", "Closed")` | a list with three `CHAR` values |
| `("Active", 2, (.TRUE., .FALSE., .EMPTY.), 7.5)` | a heterogenous list that contains values of different types, including another list |

Note that a single value enclosed in parentheses (e.g. `(7)`) is not a list literal because parentheses are used for grouping expressions and precedence. To create a single-item list, use the `SET` or `LIST` functions with a single parameter.


### Referencing resource data

#### Current value of a resource's field

The current value of a resource's field can be retrieved by using the field's name directly, or by bracketing the field's name.

The type of the value returned by this expression may be any of the possible types and depends on the resource data that is stored in that field.

"Current" means the current state of the resource after it was edited, and including the any `SET`s that have been performed. In the context of a rule executing within an MLS on a resource update, this represents the data that will be stored in the MLS if all of the rules pass. In the context of a rule executing on a frontend, this represents the data that contains all of the user's edits.

If the field does not exist on the resource, the expression evaluates to a value of type `EMPTY`.

Examples:

| Expression | Description |
|:--|:--|
| `ListPrice` | Typically evaluates to a `FLOAT` or an `INTEGER` that represent's a property's list price |
| `[ListPrice]` | Equivalent to `ListPrice` |
| `PublicRemarks` | Typically evaluates to a `CHAR` that reprents the remarks on a property |
| `[PublicRemarks]` | Equivalent to `PublicRemarks` |

#### Previous value of a resource's field

The previous value of a resource's field can be retrieved by prefixing the field name with the `LAST` token (and optionally bracketing the entire expression).

The type of the value returned by this expression may be any of the possible types and depends on the resource data that is stored in that field.

"Previous" means the previous state of the resource before editing and rule execution began. In the context of a rule executing within an MLS on a resource update, this represents the data stored in the MLS that will be replaced. In the context of a rule executing on a frontend, this represents the data as it was retrieved from the API without any user edits.

If the field does not exist on the resource, the expression evaluates to a value of type `EMPTY`.

Examples:

| Expression | Description |
|:--|:--|
| `LAST ListPrice` | Typically evaluates to a `FLOAT` or an `INTEGER` that represent's a property's list price before |
| `[LAST ListPrice]` | Equivalent to `LAST ListPrice` |
| `LAST PublicRemarks` | Typically evaluates to a `CHAR` that reprents the remarks on a property |
| `[LAST PublicRemarks]` | Equivalent to `LAST PublicRemarks` |


### Operators

#### `.OR.` Operator

Returns the logical disjunction of its two operands, returning a `BOOLEAN` value.

If the *first* operand returns `TRUE`, this expression immediately returns `TRUE` without evaluating the second operand.

If the *first* operand returns `FALSE`, this expression evalutes the second operand.

If either operand does not evaluate to a `BOOLEAN` the result of this expression is an error.

Examples:

| Expression | Description |
|:--|:--|
| `.TRUE. .OR. .TRUE.` | Evalutes to `.TRUE.` because at least one of the operands evaluates to `.TRUE.` |
| `.TRUE. .OR. .FALSE.` | Evalutes to `.TRUE.` because at least one of the operands evaluates to `.TRUE.` |
| `.FALSE. .OR. .FALSE.` | Evalutes to `.FALSE.` because neither of the operands evaluates to `.TRUE.` |
| `.TRUE. .OR. (1/0)` | Evaluates to `.TRUE.` without evaluating `(1/0)` which would evaluate to an error |
| `CoolingYN .OR. HeatingYN` | Evalutes based on field values contained in the resource |

#### `.AND.` Operator

Evaluates both of its operands and returns the logical conjunction of the two, returning a `BOOLEAN` value.

If either operand does not evaluate to a `BOOLEAN` the result of this expression is an error.

Returns the logical conjuction of its two operands, returning a `BOOLEAN` value.

If the *first* operand returns `FALSE`, this expression immediately returns `FALSE` without evaluating the second operand.

If the *first* operand returns `TRUE`, this expression evalutes the second operand.

If either operand does not evaluate to a `BOOLEAN` the result of this expression is an error.

| Expression | Description |
|:--|:--|
| `.TRUE. .AND. .TRUE.` | Evalutes to `.TRUE.` because both of the operands evaluate to `.TRUE.` |
| `.TRUE. .AND. .FALSE.` | Evalutes to `.FALSE.` because both of the operands do not evaluate to `.TRUE.` |
| `.FALSE. .AND. .FALSE.` | Evalutes to `.FALSE.` because both of the operands do not evaluate to `.TRUE.` |
| `.FALSE. .OR. (1/0)` | Evaluates to `.FALSE.` without evaluating `(1/0)` which would evaluate to an error |
| `CoolingYN .AND. HeatingYN` | Evalutes based on field values contained in the resource |

#### `.NOT.` Operator

A unary operator that evaluates its only operand and returns the logical negation of the operand, returning a `BOOLEAN` value.

If the operand does not evaluate to a `BOOLEAN` the result of this expression is an error.

| Expression | Description |
|:--|:--|
| `.NOT. .TRUE.` | Evalutes to `.FALSE.` |
| `.NOT. .FALSE.` | Evalutes to `.TRUE.` |
| `.NOT. (.FALSE. .OR. .TRUE.)` | Evalutes to `.FALSE.` because the operand evaluates to `.TRUE.` |
| `.NOT. CoolingYN` | Evalutes based on a field value contained in the resource |

#### `+` Operator

Evalutes both of its operands and performs an addition of the two. The return type depends on the types of the operands.

| First operand's type | Second operand's type | Result type | Description |
|:--|:--|:--|:--|
| `INTEGER`   | `INTEGER`   | `INTEGER`   | Performs integral addition |
| `FLOAT`     | `FLOAT`     | `FLOAT`     | Performs float addition |
| `INTEGER`   | `FLOAT`     | `FLOAT`     | Performs float addition |
| `FLOAT`     | `INTEGER`   | `FLOAT`     | Performs float addition |
| `DATE`      | `INTEGER`   | `DATE`      | Adds the given number of days to the date |
| `INTEGER`   | `DATE`      | `DATE`      | Adds the given number of days to the date |
| `TIMESTAMP` | `INTEGER`   | `TIMESTAMP` | Adds an integral number of "days" (sets of 86400 seconds) to the date. Note that this operator is not time-zone aware, so the result of adding values across a daylight saving time boundary may be unexpected. |
| `INTEGER`   | `TIMESTAMP` | `TIMESTAMP` | Adds an integral number of "days" (sets of 86400 seconds) to the date. Note that this operator is not time-zone aware, so the result of adding values across a daylight saving time boundary may be unexpected. |
| `TIMESTAMP` | `FLOAT`     | `TIMESTAMP` | Adds a fractional number of "days" (sets of 86400 seconds) to the date. Note that this operator is not time-zone aware, so the result of adding values across a daylight saving time boundary may be unexpected. |
| `FLOAT`     | `TIMESTAMP` | `TIMESTAMP` | Adds a fractional number of "days" (sets of 86400 seconds) to the date. Note that this operator is not time-zone aware, so the result of adding values across a daylight saving time boundary may be unexpected. |
| any         | any         | `<ERROR>`   | If the operands evaluate to any other type, including `EMPTY`, the result of the addition is an error |

#### `-` Operator

Evalutes both of its operands and performs a subtraction of the two. The return type depends on the types of the operands.

| First operand's type | Second operand's type | Result type | Description |
|:--|:--|:--|:--|
| `INTEGER`   | `INTEGER`   | `INTEGER`   | Performs integral subtraction |
| `FLOAT`     | `FLOAT`     | `FLOAT`     | Performs float subtraction |
| `INTEGER`   | `FLOAT`     | `FLOAT`     | Performs float subtraction |
| `FLOAT`     | `INTEGER`   | `FLOAT`     | Performs float subtraction |
| `DATE`      | `INTEGER`   | `DATE`      | Subtracts the given number of days from the date |
| `TIMESTAMP` | `INTEGER`   | `TIMESTAMP` | Subtracts an integral number of "days" (sets of 86400 seconds) from the date. Note that this operator is not time-zone aware, so the result of subtracting values across a daylight saving time boundary may be unexpected. |
| `TIMESTAMP` | `FLOAT`     | `TIMESTAMP` | Subtracts a fractional number of "days" (sets of 86400 seconds) from the date. Note that this operator is not time-zone aware, so the result of subtracting values across a daylight saving time boundary may be unexpected. |
| any         | any         | `<ERROR>`   | If the operands evaluate to any other type, including `EMPTY`, the result of the subtraction is an error |

#### `*` Operator

Evalutes both of its operands and performs a multiplication of the two. The return type depends on the types of the operands.

| First operand's type | Second operand's type | Result type | Description |
|:--|:--|:--|:--|
| `INTEGER`   | `INTEGER`   | `INTEGER`   | Performs integral multiplication |
| `FLOAT`     | `FLOAT`     | `FLOAT`     | Performs float multiplication |
| `INTEGER`   | `FLOAT`     | `FLOAT`     | Performs float multiplication |
| `FLOAT`     | `INTEGER`   | `FLOAT`     | Performs float multiplication |
| any         | any         | `<ERROR>`   | If the operands evaluate to any other type, including `EMPTY`, the result of the multiplication is an error |

#### `/` Operator

Evalutes both of its operands and performs a division of the two. The return type depends on the types of the operands.

If the second operand evaluates to a 0 (either an `INTEGER` zero or a `FLOAT` zero) the result of the division is an error.

| First operand's type | Second operand's type | Result type | Description |
|:--|:--|:--|:--|
| `INTEGER`   | `INTEGER`   | `INTEGER`   | Performs integral multiplication |
| `FLOAT`     | `FLOAT`     | `FLOAT`     | Performs float multiplication |
| `INTEGER`   | `FLOAT`     | `FLOAT`     | Performs float multiplication |
| `FLOAT`     | `INTEGER`   | `FLOAT`     | Performs float multiplication |
| any         | any         | `<ERROR>`   | If the operands evaluate to any other type, including `EMPTY`, the result of the division is an error |

#### `.MOD.` Operator

Evalutes both of its operands and performs an arithmetic modulo of the two. Both operands must be `INTEGER` value and the result is an `INTEGER` value.

If either of the operands does not evaluate to `INTEGER`, the result of the expression is an error.

#### `||` Operator

Evalutes both of its operands and performs a string concatenation of the two. The result type is a `CHAR`.

If either of the operands does not evaluate to `CHAR`, the result of the expression is an error.

#### `.CONTAINS.`

Evalutes both of its `CHAR` operands and performs a check on string containment.  The operation is `TRUE` if the left operand contains the right operand as a subsstring anywhere within it.

If either of the operands does not evaluate to `CHAR`, the result of the expression is an error.

#### `.IN.`

Evaluates the left operand to any type and the right operand to a `LIST`. If the left operand equals any item within the right operand, this evalutes to `TRUE`.

If the right operand does not evaluate to a `LIST`, the result of the expression is an error.

#### `=`, `!=` Operators

Evaluates both of its operands and determines if the operands are equal (`=`) or not equal (`!=`). The result type is a `BOOLEAN`.

#### `<`, `>`, `<=`, `>=`

Evalutes both of its operands and determines if the left operand is less than (`<`), greater than (`>`), less than or equal to (`<=`), or greater than or equal to (`>=`) the right operand.

### `IIF` expression

The `IIF` expression is a special expression that allows conditional evaluation of its sub-expressions.

The `IIF` is formed with the literal `IIF` followed by an open parenthesis, followed by three comma-separated expressions, and followed by a close parenthsis: `IIF(<condition>, <true>, <false>)`.

`IIF` evalutes the first expression.

* If the first expression evaluates to `TRUE`, the result of the entire `IIF` expression is the result of evaluating the second expression.
* If the first expression evaluates to `FALSE`, the result of the entire `IIF` expression is the result of evaluating the third expression.
* If the first expression evaluates to any other value, the result of the entire `IIF` expression is an error.

Note that this special expression type is different from a function expression because it does not eagerly evaluate all of its sub-expressions. This allows a rule to avoid errors that would result from one of the sub-expressions evaluating to an error.

For example, the expression

```
IIF(.FALSE., 7 / 0, 1)
```

evalutes the `.FALSE.` first sub-expression, does not evaluate the `7 / 0` sub-expression (which would result in an error), and only evalutes the `1` third sub-expression. The entire `IIF` expression successfully evaluates to `1`.

### Functions

Functions are formed with a function name, followed by an open parenthesis, followed by zero or more comma-separted parameter expressions, followed by a close parenthesis.

The parameters to a function are all eagerly evaluated before they are passed to the function for evaluation.

An implementation *MAY* define its own functions, but that may affect the interoperability of the expressions that are used.

An implementation *MUST* implement all of the standard functions listed below.

For the standard functions, additional parameters to the function are ignored unless otherwise specified.

#### `BOOL` function

Converts a single parameter to a `BOOLEAN` type.

The input may be one of the following types:

- `BOOLEAN`
- `CHAR`

The output is a `BOOLEAN` value.

The `CHAR` values that evaluate to `TRUE` are

* `"1"`
* `"true"` (in any upper-case/lower-case combination)
* `"yes"` (in any upper-case/lower-case combination)

All other `CHAR` values evaluate to `FALSE`.

#### `CHAR` function

Converts a single parameter to a `CHAR` type.

The input may be one of the following types:

- `BOOLEAN`
- `CHAR`
- `INTEGER`
- `DATE`
- `TIMESTAMP`

The output is a `CHAR` value.

Stringification for booleans returns either the string `"0"` or `"1"`. Stringification for integers results in a `CHAR` that could be parsed as a literal as defined for `INTEGER` literals. Stringification for dates and timestamps results in a `CHAR` that could be parsed as a literal as defined for `TIMESTAMP` and `DATE` literals, but without the leading and trailing `#`.

Examples

| Expression | Result |
|:--|:--|
| `CHAR("Identity")`   | `"Identity"` |
| `CHAR(.TRUE.)`       | `"1"` |
| `CHAR(73)`           | `"73"` |
| `CHAR(#1985-04-21#)` | `"1985-04-21"` |
| `CHAR(#2023-12-12T16:47:02.123Z#)` | `"2023-12-12T16:47:02.123Z"` |

#### `CHARF` function

A function that takes two parameters, a `FLOAT` and an `INTEGER` and returns a `CHAR`.

The first parameter (type `FLOAT`) is the value that will get turned into a string.

The second parameter (type `INTEGER`) represents how many decimal digits MUST appear after the point.

#### `TIME` function

Converts a single `CHAR` parameter to a `TIME` type.

The input may be one of the following types:

- `CHAR`
- `TIME` – identity function

If the input is a `CHAR`, it *must* be in the same format as a `TIMESTAMP` literal.

#### `DATE` function

Converts a single `CHAR` parameter to a `DATE` type.

The input may be one of the following types:

- `CHAR`
- `DATE` – identity function

If the input is a `CHAR`, it *must* be in the same format as a `DATE` literal.

#### `INT` function

Converts a single parameter to an `INTEGER` type.

The input may be one of the following types:

- `INT`
- `FLOAT`
- `BOOLEAN`
- `CHAR`

If the input is a `BOOLEAN`, the result is `0` if false and `1` if true.

If the input is a `CHAR`, it *must* be in the same format as an `INTEGER` literal.

#### `FLOAT` function

Converts a single parameter to an `FLOAT` type.

The input may be one of the following types:

- `INT`
- `FLOAT`
- `BOOLEAN`
- `CHAR`

If the input is a `BOOLEAN`, the result is `0` if false and `1` if true.

If the input is a `CHAR`, it *must* be in the same format as an `INTEGER` literal or a `FLOAT` literal.

#### `SUBSTR` function

A function that takes three parameters of type `CHAR`, `INT`, and `INT` and returns the substring of the initial parameter as a `CHAR`.

The second parameter is the starting position (1-based).

The third parameter is the ending position (1-based).

If either the second or the third parameter is out of range, the result of evaluating this function is an error.

#### `STRLEN` function

A function that takes one parameter of type `CHAR` and returns the length of the string as an `INTEGER`.

#### `LOWER` function

A function that takes one parameter of type `CHAR` and returns its parameter lower-cased as a `CHAR`.

#### `UPPER` function

A function that takes one parameter of type `CHAR` and returns its parameter upper-cased as a `CHAR`.

#### `YEAR` function

A function that takes one parameter of either type `DATE` or `TIMESTAMP` and returns the year associated with the date or timestamp, as an `INTEGER`.

For timestamps, no timezone conversion is performed before returning the year.

#### `MONTH` function

A function that takes one parameter of either type `DATE` or `TIMESTAMP` and returns the month associated with the date or timestamp, as an `INTEGER`.

For timestamps, no timezone conversion is performed before returning the month.

#### `DAY` function

A function that takes one parameter of either type `DATE` or `TIMESTAMP` and returns the day associated with the date or timestamp, as an `INTEGER`.

For timestamps, no timezone conversion is performed before returning the day.

#### `WEEKDAY` function

A function that takes one parameter of either type `DATE` or `TIMESTAMP` and returns the weekday associated with the date or timestamp, as an `INTEGER`. Sunday is `1`, Monday is `2`, .. , Saturday is `7`.

For timestamps, no timezone conversion is performed before returning the day.

#### `TYPEOF` function

A function that takes one parameter of any type and returns the type of the parameter as a `CHAR`. 

One of the following strings will be returned

* `"EMPTY"`
* `"BOOLEAN"`
* `"FLOAT"`
* `"INTEGER"`
* `"CHAR"`
* `"DATE"`
* `"TIMESTAMP"`
* `"OBJECT"`
* `"LIST"`

#### `MATCH` function

A function that takes two `CHAR` parameters and returns whether the first parameter (treated as a string) is matched by the second parameter (a regular expression).

Implementations *MUST* support a baseline regular expression language.

#### `LIST` function

A function that takes zero or more parameters of any type and returns a `LIST` of those parameters.

#### `SET` function

A function that takes zero or more parameters of any type and returns a `LIST` of those parameters. If any of the parameters are duplicated, the second and later occurrences are removed.

#### `UNION` function

A function that takes one or more `LIST` parameters and returns the set union of all of the parameters.

If any of the values are duplicated, the second and later occurrences are removed.

#### `DIFFERENCE` function

A function that takes exactly two `LIST` parameters and returns the set difference of the two lists.

#### `INTERSECTION` function

A function that takes two ore more `LIST` parameters and returns the set intersection of the two lists.

If more than two `LIST`s are provided, the result is the same as performing the intersection on the first two parameters, then performing the intersection of the result and the third parameter, and so on until all of the parameters are used.

### Special atoms

#### `.TODAY.` atom

Evaluates to a `DATE` value that represents the current date, in the timezone most appropriate to the execution.

#### `.NOW.` atom

Evalutes to a `TIMESTAMP` value that represents the current timestamp, either in UTC or the timezone most appropriate to the execution.

#### `.ENTRY.` atom

Evaluates to the value of the field specified in the rule.

#### `.OLDVALUE.` atom

Evalutes to the previous value of the field specified in the rule, or `.EMPTY.` if the resource is new.

#### `.USERID.` atom

The value of the user-id field returned by the function on the resource. Evaluates to a `CHAR` value.

#### `.USERCLASS.` atom

The value of the user-class field returned by the function on the resource. Evaluates to a `CHAR` value.

#### `.USERLEVEL.` atom

The value of the user-level field returned by the function on the resource. Evaluates to a `CHAR` value.

#### `.AGENTCODE.` atom

The value of the agent-code field returned by the function on the resource. Evaluates to a `CHAR` value.

#### `.BROKERCODE.` atom

The value of the broker-code field returned by the function on the resource. Evaluates to a `CHAR` value.

#### `.BROKERBRANCH.` atom

The value of the broker-branch field returned by the function on the resource. Evaluates to a `CHAR` value.

#### `.UPDATEACTION.` atom

Name of the UpdateAction for which this validation is performed. See table in [execution.md]. Evaluates to a `CHAR` value.

[execution.md]: execution.md

#### `.any.` atom

If the name of the `SpecVal` (stripped of the first and last dot) is equal to a name of one of the `info-token-keys` returned as part of the function on the Resource, then the type and value of this `SpecValue` is defined by that `info-token-key`. If no such `info-token-key` exists, evaluating this atom results in an error.


### Parentheses

A single expression inside of a parentheses is equivalent to the result of that expression.

This allows parentheses to be used for precedence.

| Expression | Description |
|:--|:--|
| `(1)` | Equivalent to the expression `1` |
| `(1 + 1)` | Equivalent to the expression `1 + 1` |
| `(1 + 1) * 3` | Results in `6`. Evalutes `1 + 1` first, then evaluates the multiplication of that result (`2`) and the right operand `3`. |

## Grammar

An [ANTLR4](https://www.antlr.org/) reference grammar is provided in the [grammar folder](./grammar/) of this specification.
