## Introduction

This document describes the RCP19 Business Rules specification at a high level and how rule authors would use this specification.

This document is intended for rule authors and anyone trying to familiarize themselves with the spec.

*Note: For specification details, see [language.md] for a complete description of the language, [execution.md] for a complete description of executing multiple sets of rules, and [api.md] for a complete description of how to obtain rules from a source system.*

[language.md]: language.md
[execution.md]: execution.md
[api.md]: api.md


## Why?

Why does this specification exist? And why would an MLS want to write business rules in this format in the first place?

First of all, MLSes want to ensure their data is valid and that their members are playing according to the rules. So every MLS has business rules of some sort, whether they're enforced manually, encoded into the code of their MLS system, encoded into the frontend forms, written in a standard language like this one, or take some other form.

There are two primary advantages to using *this* specification to encode business rules.

This first is that it provides a way to take your rules from one vendor to another, or to run two vendors in parallel that act on the same rules. When there is a single, standard encoding of an MLS's business rules, those rules can be reviewed and provided easily to various parties so that everyone is on the same page.

The second is that it provides a way to provide early, accurate feedback to your agents when they are filling out forms. If the frontend of your system is capable of executing rules in real-time, the rules can provide *immediate* feedback to agents about what is and isn't allowed in the MLS, reducing the amount of work customer support teams need to do and ensuring data is valid right from the start.


## A single rule

Every rule is made up of four different parts.

* Field
* Message
* Action
* Expression

The Field determines which field this rule is attached to; which field we show the error message on, which field gets updated.

The Message is what we display to the end user. The message should be something that an agent updating a listing understands.

The Action is what to do after we evaluate the rule. Do we REJECT the listing; say it’s an invalid listing in this state? Do we SET a particular value?

And the Expression is the bit of code that actually gets evaluated by a machine.

## Examples

### The list price must be greater than zero

- Field: ListPrice
- Message: The list price must be greater than zero
- Action: ACCEPT
- Expression:
  ```
  ListPrice > 0
  ```

This rule applies to the List Price (Field). It tells the agent that the list price must be greater than zero (Message). And it does this by accepting (Action) when the ListPrice is greater than zero (Expression).

This is a rule that could be run on the MLS to ensure that it has valid data *AND* on a frontend to provide early feedback to a user that is editing the ListPrice field.

### Public remarks must not contain a phone number

- Field: PublicRemarks
- Message: Public remarks must not contain a phone number
- Action: REJECT
- Expression:
  ```
  MATCH(
    PublicRemarks,
    "\d{3}-\d{4}"
  )
  ```

This rule ensures that the PublicRemarks field doesn't contain anything that looks like a phone number. If it finds something that *does* look like a phone number, it REJECTs the update.

This works because the language includes a `MATCH` function that runs a [regular expression](https://en.wikipedia.org/wiki/Regular_expression) (also commonly known as "regex" or "regexp") against the text.

This regex says three numbers (`\d{3}`), followed by a dash (`-`), followed by four numbers (`\d{4}`), anywhere in the text.

Again, this is a rule that could be run on the MLS to ensure that it has valid data *AND* on a frontend to provide early feedback to a user that is editing the PublicRemarks field.

### A close price is required when closing a listing

- Field: ClosePrice
- Message: A close price is required when closing a listing
- Action: SET_REQUIRED
- Expression:
  ```
  MlsStatus = "Closed"
  ```

This rule makes the `ClosePrice` required in a very specific circumstance: when the MlsStatus is "Closed". When the MlsStatus is *not* "Closed", the `ClosePrice` field is not required.

This rule can be run on the MLS (if `MlsStatus` is "Closed" but there is no value for `ClosePrice`, reject the update) *AND* on the frontend to provide early feedback that the field is required and needs to be filled in.

It's worth noting that the "Field" that we care about (`ClosePrice`) doesn't show up at all in the "Expression". That's OK.

### The close price is *visible* when closing a listing

- Field: ClosePrice
- Message: A close price is required when closing a listing
- Action: SET_DISPLAY
- Expression:
  ```
  MlsStatus = "Closed"
  ```

Extremely similar to the previous rule, this rule sets whether the `ClosePrice` field is *visible* to the user.

Unlike the previous rule, this probably won't run on the MLS when an update is performed. But this rule is useful when agents are editing a listing, because the number of fields the agent needs to care about can be reduced when they aren't appropriate.


### Reject if the new list price is more than 10 times the old list price

- Field: ListPrice
- Message: The new list price is over 10 times what the previous list price is. Did you accidentally add a zero?
- Action: REJECT
- Expression:
  ```
  ListPrice > 10 * LAST ListPrice
  ```

The validation language allows us to look at the *previous* value for a resource when performing updates or checks.

All we need to do is add a `LAST` before a field name to get the previous value.

So this rule allows us to make sure agents don't change the list price too drastically.

### Update the previous list price when a new price is set

- Field: PreviousListPrice
- Message: Update the previous list price when a new price is set
- Action: SET
- Expression:
  ```
  IIF(ListPrice != LAST ListPrice,
      LAST ListPrice,
      PreviousListPrice)
  ```

This is an example of a processing rule that would run on an MLS when an update is performed, but not on a frontend.

We want to update the PreviousListPrice field when the list price changes.

One of the things about this validation language is that every rule evaluates to a value. There's no way in the validation language to have a `SET` but only change the value if a certain condition is met.

So this rule takes a slightly different approach: if a certain condition is met (specifically, if `ListPrice != LAST ListPrice`) then we return the new value to set (`LAST ListPrice`). But if the condition *isn't* met, then we return the value it was set to before – or in other words we don't change the value.


### Not all business rules are representable by this grammar

- Field: ParcelNumber
- Message: Parcels must be closed for 30 days before a new property is created
- Action: REJECT
- Expression:
  ```
  <unrepresentable>
  ```

There are certain rules that can't be represented by the validation language, and that's OK.

For example, this rule. The validation language by default can only look at the current state of the property and the previous state of the property. It can't look at every other property in the database.

MLSes can still define business rules *outside* of the validation language, when necessary. However, by defining *most* rules within the validation language, an MLS can achieve the advantages of portable rules and rules that can be sent to the frontend for a positive user experience.
