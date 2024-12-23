# RESO DOM Increment Endorsement

| **RCP** | 51 |
| :--- | :--- |
| **Version** | **1.0.0** |
| **Authors** | Bryan Burgers, Zenlist<br />Al McElmon, CoreLogic<br />Sergio Del Rio, T4BI |
| **Specification** | [**LINK TO RCP**](#) |
| **Status** | IN PROGRESS |
| **Date Ratified** | Month YYYY |
| **Dependencies** | [Data Dictionary][RCP-40] |
| **Related Links** | [RESO Common Format][RCP-25]<br />[EntityEvent][RCP-27]<br />[Webhooks][RCP-28]<br /> |


<br /><br />

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

<br /><br />

# Table of Contents
- [Summary of Changes](#summary-of-changes)
- [Introduction](#introduction)
- [Section 1: Purpose](#section-1-purpose)
- [Section 2: Specification](#section-2-specification)
- [Section 3: Certification](#section-3-certification)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: License](#section-6-license)

<br /><br />

# Summary of Changes

This RCP addresses common synchronization and validity issues that arise when using the [DaysOnMarket] and [CumulativeDaysOnMarket] Data Dictionary fields to communicate Days on Market and Cumulative Days on Market, respectively.

It does this using 6 new fields (3 for Days on Market and 3 for Cumulative Days on Market) and a calculation method to determine what the current Days on Market or Cumulative Days on Market values are without requiring daily synchronization.

This RCP attempts to address these issues while continuing to allow MLSes to determine their own rules for Days on Market calculation and without requiring MLSes to share their rules regarding Days on Market calculation.

<br /><br />

# Introduction

DOM Increment defines an improved method for communicating Days on Market between MLSes and consumers that keeps the benefits of the [DaysOnMarket] and [CumulativeDaysOnMarket] fields while improving on their drawbacks.

The benefits retained:

* MLSes define how Days on Market is calculated and do not need to share their calculation rules with consumers.

The drawbacks improved:

* Days on Market does not become invalid after a day.
* Data transfer between producers and consumers can be more efficient.

<br /><br />

# Section 1: Purpose

Ensuring that Days on Market (DOM) is consistent between MLS systems and consumers is a significant challenge.

* Consumers that use [ModificationTimestamp] to get new updates often miss changes to [DaysOnMarket] because [DaysOnMarket] frequently changes without bumping [ModificationTimestamp].
* Consumers that use [EntityEvents][RCP-27] are not guaranteed to receive EntityEvents for [DaysOnMarket] changes.
* Consumers are required to perform daily reingests of listings that might have a [DaysOnMarket] change.
  * Consumers are required to guess at MLS rules for which statuses might cause changes to [DaysOnMarket] to implement these daily reingests.
  * Consumers are required to guess when an MLS increments [DaysOnMarket] to properly schedule their daily reingests.
* Providers are required to maintain enough server capacity to deal with all of their consumers performing daily reingests.
* Users of [RESO Common Format][RCP-25] do not always have a way to perform a reingest.

End consumers expect Days on Market to be reliable and consistent with the MLS. However, the synchronization of Days on Market makes this difficult, often requiring guesswork, inference, or detailed knowledge of an MLS's specific rules.

The motivation for this proposal is to provide a way for MLSes and producers to include Days on Market in their payloads in a way that
* doesn't require MLSes to publish their calculation rules, and
* can be valid for longer than a single day
and that allows consumers to have the same, correct, up-to-date information about Days on Market.

Because this method allows the Days on Market information to be valid for longer than a single day, this allows more efficient data transfer between producers and consumers such that daily updates are not required.

<br /><br />

# Section 2: Specification

DOM Increment adds 3 new fields for Days on Market and 3 new fields for Cumulative Days on Market to the Data Dictionary and defines a way to calculate the current Days on Market and Cumulative Days on Market from those two sets of 3.

Unlike [DaysOnMarket] and [CumulativeDaysOnMarket], the two sets of 3 fields remain valid and useable until the next property update is received.

## Fields

The following fields are added to the [Property Resource].

* `DaysOnMarketIncrementYN`
    * Type: Boolean
    * Description: Used to determine the number of days the listing is on market, as defined by the MLS business rules, this field determines whether that Days on Market is currently increasing every day. See RCP-51.
* `DaysOnMarketIncrementBase`
    * Type: Whole Number
    * Description: Used to determine the number of days the listing is on market, as defined by the MLS business rules, this field determines the base number for the Days on Market Increment calculation. See RCP-51.
* `DaysOnMarketIncrementTimestamp`
    * Type: Timestamp
    * Description: Used to determine the number of days the listing is on market, as defined by the MLS business rules, this field determines the timestamp to use for the Days on Market Increment calculation. See RCP-51.
* `CumulativeDaysOnMarketIncrementYN`
    * Type: Boolean
    * Description: Used to determine the cumulative number of days the listing is on market, as defined by the MLS business rules, this field determines whether that Cumulative Days on Market is currently increasing every day. See RCP-51.
* `CumulativeDaysOnMarketIncrementBase`
    * Type: Whole Number
    * Description: Used to determine the cumulative number of days the listing is on market, as defined by the MLS business rules, this field determines the base number for the Days on Market Increment calculation. See RCP-51.
* `CumulativeDaysOnMarketIncrementTimestamp`
    * Type: Timestamp
    * Description: Used to determine the cumulative number of days the listing is on market, as defined by the MLS business rules, this field determines the timestamp to use for the Days on Market Increment calculation. See RCP-51.

Requirements:

* If `DaysOnMarketIncrementBase` is provided, `DaysOnMarketIncrementYN` *MUST* be provided.
* If `DaysOnMarketIncrementYN` is provided, `DaysOnMarketIncrementBase` *MUST* be provided.
* If `DaysOnMarketIncrementYN` is `true`, `DaysOnMarketIncrementTimestamp` *MUST* be provided.
* If `CumulativeDaysOnMarketIncrementBase` is provided, `CumulativeDaysOnMarketIncrementYN` *MUST* be provided.
* If `CumulativeDaysOnMarketIncrementYN` is provided, `CumulativeDaysOnMarketIncrementBase` *MUST* be provided.
* If `CumulativeDaysOnMarketIncrementYN` is `true`, `CumulativeDaysOnMarketIncrementTimestamp` *MUST* be provided.

## Calculation

Consumers of a listing MUST use the following algorithm to calculate the "current Days on Market".

1. Look at the value of `DaysOnMarketIncrementYN`. If it is false, continue to step 2. If it is true, skip to step 3.
2. The calculated DOM is the value of `DaysOnMarketIncrementBase`. Stop.
3. Retrieve the current timestamp in UTC.
4. Retrieve the value of `DaysOnMarketIncrementTimestamp`, in UTC.
5. If [PropertyTimeZoneName] is set, convert both timestamps to the timezone represented.
6. Calculate the number of full days between the two timestamps. *(See note below.)*
7. Add the number of full days to DaysOnMarketIncrementBase.

Or, in pseudocode

```
if DaysOnMarketIncrementYN == false:
    return DaysOnMarketIncrementBase
else:
    let now = GetCurrentTimestampInUtc()
    let timestamp = DaysOnMarketIncrementTimestamp
    if PropertyTimeZoneName:
        now = now.withTimeZone(PropertyTimeZoneName)
        timestamp = timestamp.withTimeZone(PropertyTimeZoneName)
    let diff = DateDifferenceDays(timestamp, now)
    return DaysOnMarketIncrementBase + diff
```

*Note on step 6*: partial days are truncated down. This allows the calculated Days on Market value to remain the same throughout the day until it gets incremented on the day "anniversary" of `DaysOnMarketIncrementTimestamp`.

## Payload examples

### Non-incrementing listings

Many listings do not increment their Days on Market value. For example, in many MLSes, "Closed" listings don't update Days on Market.

```json
{
    "ListingKey": "12345",
    "DaysOnMarketIncrementYN": false,
    "DaysOnMarketIncrementBase": 300
}
```

According to the calculation – epecially steps 1 and 2 – the calculation will always return the number 300.

### Incrementing listing

For listings that do increment their Days on Market value, `DaysOnMarketIncrementYN` is set to `true`.

```json
{
    "ListingKey": "1",
    "DaysOnMarketIncrementYN": true,
    "DaysOnMarketIncrementBase": 1,
    "DaysOnMarketIncrementTimestamp": "2025-01-01T06:00:00Z"
}
```

Step 1 of the calculation determines that this listing *will* increment its Days on Market over time (until the next property update is received from the source).

Because the calculation depends on the current timestamp, a table is provided based on various possible current timestamps and their calculations.

| Current timestamp | `DaysOnMarketIncrementTimestamp` | Step 6 – days between | `DaysOnMarketIncrementBase` | Step 7 - Days on Market |
|:--|:--|--:|--:|--:|
| `2025-01-01T07:00:00Z` | `2025-01-01T06:00:00Z` | 0 | 1 | 1 |
| `2025-01-02T05:00:00Z` | `2025-01-01T06:00:00Z` | 0 | 1 | 1 |
| `2025-01-02T07:00:00Z` | `2025-01-01T06:00:00Z` | 1 | 1 | 2 |
| `2025-01-06T07:00:00Z` | `2025-01-01T06:00:00Z` | 5 | 1 | 6 |
| `2025-01-31T07:00:00Z` | `2025-01-01T06:00:00Z` | 30 | 1 | 31 |
| `2025-02-01T07:00:00Z` | `2025-01-01T06:00:00Z` | 31 | 1 | 32 |
| `2025-02-01T19:11:36Z` | `2025-01-01T06:00:00Z` | 31 | 1 | 32 |

The `DaysOnMarketIncrementBase` is not *required* to be either 0 or 1. MLSes may choose to use this fact when a listing resumes incrementing Days on Market after a period of not incrementing Days on Market (for example, if a listing goes to Pending and then returns to Active in an MLS that pauses Days on Market incrementing while a listing is pending).

In this case, a payload that includes the following:

```json
{
    "ListingKey": "2",
    "DaysOnMarketIncrementYN": true,
    "DaysOnMarketIncrementBase": 17,
    "DaysOnMarketIncrementTimestamp": "2025-04-21T06:00:00Z"
}
```

would result in the following Days on Market calculations at the following times.

| Current timestamp | `DaysOnMarketIncrementTimestamp` | Step 6 – days between | `DaysOnMarketIncrementBase` | Step 7 - Days on Market |
|:--|:--|--:|--:|--:|
| `2025-04-21T07:00:00Z` | `2025-04-21T06:00:00Z` | 0 | 17 | 17 |
| `2025-04-22T05:00:00Z` | `2025-04-21T06:00:00Z` | 0 | 17 | 17 |
| `2025-04-22T07:00:00Z` | `2025-04-21T06:00:00Z` | 1 | 17 | 18 |
| `2025-04-30T07:00:00Z` | `2025-04-21T06:00:00Z` | 9 | 17 | 26 |
| `2025-05-01T07:00:00Z` | `2025-04-21T06:00:00Z` | 10 | 17 | 27 |
| `2025-05-08T19:11:36Z` | `2025-04-21T06:00:00Z` | 17 | 17 | 34 |

<br /><br />

# Section 3: Certification

The following tests should be added to RESO certification

* ```gherkin
  Scenario: DaysOnMarketIncrementYN
      When "DaysOnMarketIncrementYN" exists in the "Property" metadata
      Then "DaysOnMarketIncrementYN" MUST be "Boolean" data type
  ```

* ```gherkin
  Scenario: DaysOnMarketIncrementBase
      When "DaysOnMarketIncrementBase" exists in the "Property" metadata
      Then "DaysOnMarketIncrementBase" MUST be "Number" data type
  ```

* ```gherkin
  Scenario: DaysOnMarketIncrementTimestamp
      When "DaysOnMarketIncrementTimestamp" exists in the "Property" metadata
      Then "DaysOnMarketIncrementTimestamp" MUST be "Timestamp" data type
  ```

* ```gherkin
  Scenario: CumulativeDaysOnMarketIncrementYN
      When "CumulativeDaysOnMarketIncrementYN" exists in the "Property" metadata
      Then "CumulativeDaysOnMarketIncrementYN" MUST be "Boolean" data type
  ```

* ```gherkin
  Scenario: CumulativeDaysOnMarketIncrementBase
      When "CumulativeDaysOnMarketIncrementBase" exists in the "Property" metadata
      Then "CumulativeDaysOnMarketIncrementBase" MUST be "Number" data type
  ```

* ```gherkin
  Scenario: CumulativeDaysOnMarketIncrementTimestamp
      When "CumulativeDaysOnMarketIncrementTimestamp" exists in the "Property" metadata
      Then "CumulativeDaysOnMarketIncrementTimestamp" MUST be "Timestamp" data type
  ```


Additionally, a few per-record tests should be added to certification.

1. If `DaysOnMarketIncrementYN` is found on a property, then `DaysOnMarketIncrementBase` MUST be found on that property
2. If `DaysOnMarketIncrementBase` is found on a property, then `DaysOnMarketIncrementYN` MUST be found on that property
3. If `DaysOnMarketIncrementYN` is found on a property and is `true`, then `DaysOnMarketIncrementTimestamp` MUST be found on that property
4. If `CumulativeDaysOnMarketIncrementYN` is found on a property, then `CumulativeDaysOnMarketIncrementBase` MUST be found on that property
5. If `CumulativeDaysOnMarketIncrementBase` is found on a property, then `CumulativeDaysOnMarketIncrementYN` MUST be found on that property
6. If `CumulativeDaysOnMarketIncrementYN` is found on a property and is `true`, then `CumulativeDaysOnMarketIncrementTimestamp` MUST be found on that property
7. If `DaysOnMarketIncrementYN` if found on a property and is `false` and `DaysOnMarket` is found on that property, then `DaysOnMarket` and `DaysOnMarketIncrementBase` MUST be identical
8. If `CumulativeDaysOnMarketIncrementYN` if found on a property and is `false` and `CumulativeDaysOnMarket` is found on that property, then `CumulativeDaysOnMarket` and `CumulativeDaysOnMarketIncrementBase` MUST be identical
9. If `DaysOnMarketIncrementYN` is found on a property and `DaysOnMarket` is found on that property, then `DaysOnMarket` SHOULD equal the output of the DOM Increment calculation
10. If `CumulativeDaysOnMarketIncrementYN` is found on a property and `CumulativeDaysOnMarket` is found on that property, then `CumulativeDaysOnMarket` SHOULD equal the output of the DOM Increment calculation

Tests 1–6 exist to ensure that all of the fields required to perform the calculation are present if at least one of them is.

Tests 7–10 exist to check that `DaysOnMarket` and DOM Increment give the same value.


<br /><br />

# Section 4. Contributors

This document was written by Bryan Burgers.

Thanks to the following contributors for their help with this specification:

| Contributor | Company |
| --- | --- |
| Jeff LaJoie | Zenlist |
| Geoff Rispin | Templates 4 Business, Inc. |
| Josh Darnell | RESO |

And countless others within Zenlist and within RESO for providing feedback and advice.

<br /><br />

# Section 5: References

Please see the following references for more information regarding topics covered in this document:

* [Property Resource]
* [DaysOnMarket Field][DaysOnMarket]
* [CumulativeDaysOnMarket Field][CumulativeDaysOnMarket]
* [PropertyTimeZoneName Field][PropertyTimeZoneName]

<br /><br />

# Section 6: License

This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO](mailto:info@reso.org) if you have any questions.




[DaysOnMarket]: https://ddwiki.reso.org/display/DDW20/DaysOnMarket+Field
[CumulativeDaysOnMarket]: https://ddwiki.reso.org/display/DDW20/CumulativeDaysOnMarket+Field
[PropertyTimeZoneName]: https://ddwiki.reso.org/display/DDW20/PropertyTimeZoneName+Field
[ModificationTimestamp]: https://ddwiki.reso.org/display/DDW20/ModificationTimestamp+Field
[Property Resource]: https://ddwiki.reso.org/display/DDW20/Property+Resource

[RCP-25]: https://github.com/RESOStandards/transport/blob/a99fb6ca307208280ac51bca1f573e89cb67b202/proposals/reso-common-format.md
[RCP-27]: https://github.com/RESOStandards/transport/blob/1091f1e82b2108c5a6712af65a417c8db6c76c4c/proposals/entity-events.md
[RCP-28]: https://github.com/RESOStandards/transport/blob/main/proposals/webhooks-push.md
[RCP-40]: https://github.com/RESOStandards/transport/blob/a99fb6ca307208280ac51bca1f573e89cb67b202/proposals/data-dictionary.md
