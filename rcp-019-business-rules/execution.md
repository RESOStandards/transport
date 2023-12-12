## Introduction

This document describes how to execute a set of rules against resource data. Information about retrieving the set of rules can be found at [api.md]. Information about how to evaluate a single rule can be found at [language.md].

This document is intended for rule engine implementors.

[api.md]: api.md
[language.md]: language.md

## Running rules

A compliant rule engine runs rules in the order that they are provided. A compliant rule engine *MAY* perform operations on the list of rules that reduce the number of rules that need to be executed as long as the resulting output is equivalent to running the entire list of rules in the order that they are provided.

Every rule has at least 4 parts to it:

- The order in which to to run the rule
- The field that the rule applies to
- The expression in the Validatin Language to execute

Each expression is executed against:

- The current value of the resource
  - Note that the `SET` action may update this resource; later expressions MUST reflect the values of previous SET actions
- The previous value of the resource
  - This is a fixed value that represents the value of the resource that is being replaced

## Actions

The following are the available actions that may be used in a rule.

| Keyword | Type | Purpose |
|:--------|:-----|:--------|
| `ACCEPT` | `BOOLEAN` | If the expression is true, the field value is considered accepted. |
| `REJECT` | `BOOLEAN` | If the expression is true, the field value is considered rejected without further testing. |
| `WARNING` | `BOOLEAN` | If the expression is true, the client should show a warning message to the user. |
| `SET` | `TYPEOF(EXP)` | Used to change the value of a field. |
| `SET_DEFAULT` | `TYPEOF(EXP)` | Used to set the value of a field when a new record is created. |
| `SET_REQUIRED` | `BOOLEAN` | If the expression is true, the field is treated as a required field. |
| `SET_READ_ONLY` | `BOOLEAN` | If the expression is true, the field is treated as a read-only field. |
| `SET_DISPLAY` | `BOOLEAN` | If the expression is true, the field should be visible. |
| `SET_PICKLIST` | `LIST of CHAR` | Set the list of available options for a picklist to a particular set. |
| `RESTRICT_PICKLIST` | `LIST of CHAR` | Remove from the list of available options for a picklist a particular set. |

### `ACCEPT`

Expected type: `BOOLEAN`.

The accept action indicates that the value for this field should be accepted immediately without evaluating further testing rules.

If evaluating the expression results in `true`, the field is considered valid and accepted without further testing. Immediately following `SET` expressions MUST be executed.

If evaluating the expression results in `false`, no determination on the validity of the field is made and rule execution continues.

If evaluating the expression results in any other value or an error, no determination on the validity of the field is made and rule execution continues.

### `REJECT`

Expected type: `BOOLEAN`.

The reject action indicates that the value for this field is invalid and processing should stop.

If evaluating the expression results in `true`, the field is considered *invalid* and the update is rejected. Immediately following `SET` expressions MUST NOT be executed. Rule processing stops.

If evaluating the expression results in `false`, no determination on the validity of the field is made and rule execution continues.

If evaluating the expression results in any other value or an error, no determination on the validity of the field is made and rule execution continues.

### `WARNING`

Expected type: `BOOLEAN`.

The warning action indicates that the value for this field may have something wrong with it and a message should be shown to the user.

If evaluating the expression results in `true`, the client should show a warning message to the user. (A previous version of this spec indicated including a `Warning-Response` in the UPDATE request, but that is not current specified in RCP-010).

If evaluating the expression results in `false`, no determination on the validity of the field is made and rule execution continues.

If evaluating the expression results in any other value or an error, no determination on the validity of the field is made and rule execution continues.

### `SET`

Expected type: `TYPEOF(EXP)`.

The set action is used to update a value.

The expression is evaluated and the result stored in the designated field.

### `SET_DEFAULT`

Expected type: `TYPEOF(EXP)`.

The set action is used to update a value when a new record is created.

This expression MUST be executed ONLY when a NEW record is created. Supersedes the default value as indicated in the Update Metadata.

### `SET_REQUIRED`

Expected type: `BOOLEAN`.

The set-required action indicates that a particular field is required.

If evaluating the expression results in `true`, this field should be considered required. This value remains regardless of any future `SET_REQUIRED` actions on this field.

If evaluating the expression results in `false`, any other value or an error, no change to this field is made.

### `SET_READ_ONLY`

Expected type: `BOOLEAN`.

The set-read-only action indicates that a particular field is read-only.

If evaluating the expression results in `true`, this field should be considered read-only and the system should prevent updating it. This value remains regardless of any future `SET_READ_ONLY` actions on this field.

If evaluating the expression results in `false`, any other value or an error, no change to this field is made.

### `SET_DISPLAY`

Expected type: `BOOLEAN`.

The set-display action indicates that a particular field is visible.

If evaluating the expression results in `true`, this field should be considered visible and the system should show the field.

If evaluating the expression results in `false`, this field should be considered invisible and the system should not show the field.

If evaluating the expression results in any other value or an error, no change to this field is made.

The result of this expression has no effect on whether a field is READ ONLY or not.

### `SET_PICKLIST`

Expected type: `LIST of CHAR`.

The set-picklist action replaces the provided values from the picklist for a field. The evaluation of this expression must return a `LIST`.

If evaluating the expression results in a `LIST`, then the current set of pick-list items for the given field is *replaced* with the returned list.

If evaluating the expression results in an empty `LIST`, then the current set of pick-list items for the given field is *replaced* with a list of 0 items; there are no items available to be picked.

If evaluating the expression results in any other type or in an error, no updates to the set of pick-list items are made.

### `RESTRICT_PICKLIST`

Expected type: `LIST of CHAR`.

The restrict-picklist action removes the provided values from the picklist for a field. The evaluation of this expression must return a `LIST`.

If evaluating the expression results in a `LIST`, then all items of that list are *removed* from the current set of pick-list items for the given field.

If evaluating the expression results in an empty `LIST`, then no items are *removed* from the current set of pick-list items for the given field.

If evaluating the expression results in any other type or in an error, no updates to the set of pick-list items are made.
