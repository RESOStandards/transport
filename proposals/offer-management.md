# RESO Offer Management Endorsement

| **RCP** | 57 |
| :--- | :--- |
| **Version** | **1.0.0** |
| **Authors** | [Chris Haran](mailto:chris.haran@mredllc.com)<br />[Josh Darnell](mailto:josh@darnjo.com) |
| **Specification** | [**LINK TO RCP**](#) |
| **Status** | IN PROGRESS |
| **Date Ratified** | TBD |
| **Dependencies** | [Data Dictionary 2.1](https://github.com/RESOStandards/transport/blob/main/proposals/data-dictionary.md)<br />[RESO Common Format](https://github.com/RESOStandards/transport/blob/main/proposals/reso-common-format.md)<br />[RESO Listing Advertisement (RCP-52)](https://github.com/RESOStandards/transport/discussions/162) |
| **Related Links** | [ULI Resolution Protocol (RCP-54)](https://github.com/RESOStandards/transport/pull/222)<br />[Organization and System Identifiers (RCP-55)](https://github.com/RESOStandards/transport/pull/243)<br />[Feed Entitlements and Visibility (RCP-35)](https://github.com/RESOStandards/transport/pull/169)<br />[Web API Add/Edit](https://github.com/RESOStandards/transport/blob/main/proposals/web-api-add-edit.md) |


<br /><br />

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

<br /><br />

# Table of Contents
- [Summary of Changes](#summary-of-changes)
- [Introduction](#introduction)
- [Section 1: Purpose](#section-1-purpose)
- [Section 2: Specification](#section-2-specification)
  - [Section 2.1: Participation and Confidentiality](#section-21-participation-and-confidentiality)
  - [Section 2.2: ActivityPub Usage](#section-22-activitypub-usage)
  - [Section 2.3: Offer Identity](#section-23-offer-identity)
  - [Section 2.4: The Offer Resource](#section-24-the-offer-resource)
  - [Section 2.5: The OfferSubmission Resource](#section-25-the-offersubmission-resource)
  - [Section 2.6: The OfferPropertyGroup Resource](#section-26-the-offerpropertygroup-resource)
  - [Section 2.7: Offer States](#section-27-offer-states)
  - [Section 2.8: Activity Streams Mapping](#section-28-activity-streams-mapping)
  - [Section 2.9: Counter Offers](#section-29-counter-offers)
  - [Section 2.10: Web API Conformance](#section-210-web-api-conformance)
  - [Section 2.11: Authentication and Authorization](#section-211-authentication-and-authorization)
  - [Section 2.12: Worked Examples](#section-212-worked-examples)
- [Section 3: Certification](#section-3-certification)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: Appendices](#section-6-appendices)
- [Section 7: License](#section-7-license)

<br /><br />

# Summary of Changes

* Moves offer handling out of the [RESO Listing Advertisement](https://github.com/RESOStandards/transport/discussions/162) proposal into its own endorsement. The workgroups approved the separation and Interoperability has cleared it.
* Introduces three Data Dictionary resources: `Offer`, `OfferSubmission` and `OfferPropertyGroup`, defined in [Section 2.4](#section-24-the-offer-resource) through [Section 2.6](#section-26-the-offerpropertygroup-resource).
* Introduces two lookups, `OfferSubmissionStatus` and `OfferReceivedStatus`, defined in [Section 2.7](#section-27-offer-states).
* Binds two kinds of implementer with one model: systems serving the resources over OData on the Web API, and systems exchanging offers over ActivityPub through offer management hubs.
* Amends [RCP-52](https://github.com/RESOStandards/transport/discussions/162) Section 2.3, whose worked example carries offer terms in the text of an activity. [Section 2.2](#section-22-activitypub-usage) places that content in the referenced payload instead.

<br /><br />

# Introduction

An offer is the point in a transaction where the most value and the most risk meet, and it is the point with the least standardization. Offers move as email attachments, as PDFs, and through portals that each model an offer differently. A listing agent receiving offers from several buyer agents commonly receives them in several shapes and reconciles them by hand.

The Data Dictionary has no Offer resource. Of its 43 resources, none models an offer, and the only place offers appear at all is two values of `TransactionType`, `PurchaseOffer` and `LeaseOffer`, which classify a transaction rather than describe an offer. There is nothing to extend, so this proposal defines the shape.

Two things make an offer different from the records the Data Dictionary already carries, and both shape this specification.

An offer is a **conversation**, not a record. It is submitted, acknowledged, countered, countered again, and finally accepted, rejected, withdrawn or expired. Each turn is a new statement by a different party, and the sequence is the substance. A single mutable row cannot represent it.

An offer is **confidential**. It carries the legal name, address and telephone number of a buyer, the price that buyer will pay and the financing behind it. This is the most sensitive data in the proposal, and possibly in the Data Dictionary. The design assumes confidentiality rather than adding it later.

<br /><br />

# Section 1: Purpose

This specification gives an offer a standard shape and a standard set of states, so that:

* a buyer agent can submit an offer from the system of their choice and have it arrive intact in a listing agent's system;
* a listing agent can compare offers from different sources side by side without rekeying them;
* a counter offer is a new statement in a thread rather than an edit that destroys what came before;
* the status of an offer is machine-readable on both sides, so that neither party has to telephone to ask; and
* the parties to an offer, and only those parties, can read its contents.

The offer is the unit of scope. What happens after acceptance – the executed contract, escrow and title, contingency management, closing and archive – is transaction management, and is addressed separately. This specification may be referenced from that work but does not attempt it.

<br /><br />

# Section 2: Specification

## Section 2.1: Participation and Confidentiality

An offer is exchanged between named parties. Unlike a listing, an offer is not published to a network and is not discoverable. Participation is by being addressed: the buyer side addresses the listing side, and the listing side replies.

Implementations MUST NOT publish offer content to a public audience. An activity conveying an offer or a change to one MUST be addressed to the specific parties entitled to see it, and MUST NOT use the public collection `https://www.w3.org/ns/activitystreams#Public`.

The network MUST NOT store offer content. Offer data lives behind the originator's protected link ([Section 2.2](#section-22-activitypub-usage)) and is read only by parties the originator has authorized ([Section 2.11](#section-211-authentication-and-authorization)).

A provider decides what it exposes. The state of an offer MAY be withheld from the thread entirely, in which case a consumer reads it from the payload ([Section 2.8](#section-28-activity-streams-mapping)). Withholding lowers what counterparties can see without dereferencing, and does not break the exchange.

## Section 2.2: ActivityPub Usage

Implementations MUST use the standard [Activity Streams 2.0 vocabulary](https://www.w3.org/TR/activitystreams-vocabulary/), for example [`Offer`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-offer), [`Accept`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-accept), [`Reject`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-reject) and [`Note`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-note). Implementations MUST NOT extend [ActivityPub](https://www.w3.org/TR/activitypub/)'s JSON-LD with custom terms.

Offer content MUST be carried in the RESO Common Format payload that the activity references, and MUST NOT appear in the ActivityPub object. This applies to every element of [Section 2.4](#section-24-the-offer-resource) through [Section 2.6](#section-26-the-offerpropertygroup-resource): price, financing, contingencies, dates, buyer and co-buyer details, notes and media. An identifier is not content and MAY appear in the activity, as it does in a thread reference.

The payload MUST be expressed in RESO Common Format. The payload MUST be reachable through a link the activity references, and MUST NOT be embedded in the activity. That link is not required to be a RESO Web API endpoint: any endpoint that returns the payload in RESO Common Format satisfies this specification. The link MUST refuse an unauthenticated dereference ([Section 2.11](#section-211-authentication-and-authorization)).

This rule is the same one adopted by the [ULI Resolution Protocol](https://github.com/RESOStandards/transport/pull/222) in its Section 2.2, and it is the reason this specification adds no vocabulary to ActivityPub. Where an offer concept has no Activity Streams equivalent, it is represented by the payload and not by a new term ([Section 2.8](#section-28-activity-streams-mapping)).

## Section 2.3: Offer Identity

Every offer and every submission carries an identifier. In an ActivityPub thread the identifier is the activity's `id`, a URL of the form used in the Listing Advertisement proposal:

```json
"id": "https://my.offercloud.com/offer/XYZ999"
```

The identifier MUST be unique. Where the identifier is visible, it MUST be immutable: an implementation MUST NOT reissue, renumber or recycle it.

The identifier is **not required to be** the `OfferKey`, the `OfferId` or any other element of [Section 2.4](#section-24-the-offer-resource). A provider that does not wish to expose a meaningful identifier in plain view MAY publish an opaque one and require a consumer to dereference the payload to resolve the underlying record. Implementations MUST NOT infer that the trailing segment of an identifier is an `OfferId`, and MUST NOT parse an identifier to recover offer data.

The choice is the provider's, and it MUST be explicit rather than assumed. A provider publishing meaningful identifiers is making a disclosure decision, not a formatting one.

## Section 2.4: The Offer Resource

The `Offer` resource is the top-level object. One `Offer` exists for one offer between two parties on one listing, and it persists for the life of that negotiation. The turns of the negotiation are `OfferSubmission` records ([Section 2.5](#section-25-the-offersubmission-resource)).

| Field | Type | Nullable | Max length | Lookup | Definition |
| :--- | :--- | :--- | :--- | :--- | :--- |
| OfferKey | String | No | 255 | | The unique system identifier for the offer. |
| OfferId | String | Yes | 255 | | The well-known identifier assigned to an offer by the system it originated in. |
| ListingId | String | Yes | 255 | | The well-known identifier of the listing the offer is made against. |
| ListingKey | String | Yes | 255 | | The system identifier of the listing the offer is made against. |
| OfferNotes | String | Yes | | | Notes that apply to the offer as a whole rather than to one submission. |
| OfferOriginatingSystemId | String | Yes | 255 | | The Organization Unique Identifier of the system with authoritative control over the listing being offered on. |
| OfferOriginatingSystemName | String | Yes | 255 | | The name of the system with authoritative control over the listing being offered on. |
| OfferSourceSystemId | String | Yes | 255 | | The Organization Unique Identifier of the system the listing record was directly received from, which may differ from the originating system. |
| OfferSourceSystemName | String | Yes | 255 | | The name of the system the listing record was directly received from. |
| ModificationTimestamp | Timestamp | No | | | The date and time the offer record was last modified. |

An `Offer` MUST carry either `ListingId` or `ListingKey`. An offer that identifies no listing cannot be routed to a listing agent.

The four system fields overlap [Organization and System Identifiers (RCP-55)](https://github.com/RESOStandards/transport/pull/243). Where that proposal is ratified first, these fields adopt its definitions rather than restating them.

## Section 2.5: The OfferSubmission Resource

An `OfferSubmission` is one turn in the negotiation: an initial offer, a counter, or a re-counter. Submissions are threaded onto one `Offer` by `OfferId`, and they are append-only. An implementation MUST NOT modify a submission to represent a counter; it MUST create a new one ([Section 2.9](#section-29-counter-offers)).

| Field | Type | Nullable | Max length | Lookup | Definition |
| :--- | :--- | :--- | :--- | :--- | :--- |
| OfferSubmissionKey | String | No | 255 | | The unique system identifier for this submission. |
| OfferId | String | No | 255 | | The offer this submission belongs to. |
| OfferAgentId | String | Yes | 255 | | The well-known identifier of the agent submitting the offer or counter offer. |
| OfferAgentKey | String | Yes | 255 | | The system identifier of the agent submitting the offer or counter offer. |
| OfferBuyerLegalName | String | Yes | 255 | | The legal name of the buyer submitting the offer. |
| OfferBuyerLegalAddress | String | Yes | 255 | | The legal address of the buyer submitting the offer. |
| OfferBuyerPhone | String | Yes | 50 | | The telephone number of the buyer submitting the offer. |
| OfferCoBuyerLegalName | String | Yes | 255 | | The legal name of the co-buyer submitting the offer. |
| OfferCoBuyerLegalAddress | String | Yes | 255 | | The legal address of the co-buyer submitting the offer. |
| OfferCoBuyerPhone | String | Yes | 50 | | The telephone number of the co-buyer submitting the offer. |
| PurchasePrice | Number | Yes | | | The purchase price being offered. |
| CreditAtClosing | Number | Yes | | | The credit being requested at closing. |
| EarnestMoney | Number | Yes | | | The earnest money that will be submitted. |
| ClosingDate | Date | Yes | | | The closing date proposed in the offer. |
| BuyerFinancing | String List, Single | Yes | | BuyerFinancing | The financing proposed in the offer. |
| WaiverOfInspection | Boolean | Yes | | | Indicates whether or not the inspection is being waived. |
| Contingencies | String List, Multi | Yes | | Contingency | The contingencies attached to the offer. |
| AsIsCondition | Boolean | Yes | | | Indicates whether or not the offer includes taking the property in as-is condition. |
| EscalationClause | Boolean | Yes | | | Indicates whether or not an escalation clause is included. |
| Concessions | String | Yes | | | The concessions being submitted as part of the offer. |
| OfferExpirationDate | Date | Yes | | | The date the submitted offer expires. |
| OfferInspectionDate | Date | Yes | | | The date requested for an inspection. |
| OfferAppraisalDate | Date | Yes | | | The date requested for an appraisal. |
| OfferEscrowCompany | String | Yes | 255 | | The name of the company that will hold escrow. |
| BuyerBrokerageCompensation | String | Yes | 255 | | The buyer brokerage compensation proposed in the offer. |
| OfferSubmissionNotes | String | Yes | | | The notes related to the offer being submitted. |
| OfferSubmissionStatus | String List, Single | Yes | | OfferSubmissionStatus | The status of the offer as recorded by the submitting side. |
| OfferReceivedStatus | String List, Single | Yes | | OfferReceivedStatus | The status of the offer as recorded by the receiving side. |
| OfferSubmissionTimestamp | Timestamp | Yes | | | The date and time the offer was submitted. |
| CounterOfferSubmissionTimestamp | Timestamp | Yes | | | The date and time a counter offer was submitted. |
| OfferAcceptanceTimestamp | Timestamp | Yes | | | The date and time the offer was accepted. |
| ModificationTimestamp | Timestamp | No | | | The date and time the submission was last modified. |

`BuyerFinancing`, `Contingencies` and `BuyerBrokerageCompensation` reuse existing Data Dictionary elements and their lookups rather than introducing offer-specific equivalents. An implementation MUST use the existing standard values.

Media and documents attached to a submission, including executed contract documents, are carried through the existing `Media` resource and are not duplicated here.

The buyer and co-buyer fields are personal data. [Section 2.11](#section-211-authentication-and-authorization) governs who may read them, and they are subject to [Feed Entitlements and Visibility (RCP-35)](https://github.com/RESOStandards/transport/pull/169) where that proposal applies.

## Section 2.6: The OfferPropertyGroup Resource

The `OfferPropertyGroup` identifies the subject property of a submission. It exists because an offer may be made on a property that the receiving system does not hold a listing record for, so the address must travel with the offer.

| Field | Type | Nullable | Max length | Lookup | Definition |
| :--- | :--- | :--- | :--- | :--- | :--- |
| OfferPropertyGroupKey | String | No | 255 | | The unique system identifier for the property group. |
| ListingId | String | Yes | 255 | | The listing the offer is made against. |
| StreetNumber | String | Yes | 25 | | The street number of the subject property. |
| StreetName | String | Yes | 50 | | The street name of the subject property. |
| City | String | Yes | 50 | | The city of the subject property. |
| StateOrProvince | String List, Single | Yes | | StateOrProvince | The state or province of the subject property. |
| PostalCode | String | Yes | 10 | | The postal code of the subject property. |
| CountyOrParish | String List, Single | Yes | | CountyOrParish | The county or parish of the subject property. |
| Country | String List, Single | Yes | | Country | The country of the subject property. |
| ParcelNumber | String | Yes | 50 | | The parcel number of the subject property. |
| UniversalPropertyId | String | Yes | 255 | | The universal property identifier of the subject property. |

Every field of this resource reuses an existing Data Dictionary element. An `OfferPropertyGroup` MUST carry enough to identify the property unambiguously: either `UniversalPropertyId`, or `ParcelNumber` with `StateOrProvince` and `CountyOrParish`, or a complete street address.

## Section 2.7: Offer States

An offer carries two states, one per side. `OfferSubmissionStatus` is what the submitting side records. `OfferReceivedStatus` is what the receiving side records. They are separate lookups because the two sides observe different things and their value sets may diverge.

Each is single-valued. An offer holds one status at a time on each side; the history of an offer is the sequence of its submissions and their timestamps, not a list of statuses on one record.

**OfferSubmissionStatus.** The lookup is open with enumerations; its standard values are:

| Lookup Value | Definition |
| :--- | :--- |
| Submitted | The offer has been submitted. |
| Delivered | The offer has been delivered to the receiving system. |
| Received | The offer has been received. |
| Acknowledged | The offer has been acknowledged. |
| Countered | The offer has been countered. |
| Accepted | The offer has been accepted. |
| Rejected | The offer has been rejected. |
| Withdrawn | The offer has been withdrawn. |
| Expired | The offer has expired. |
| RequestHighestAndBest | The highest and best offer has been requested. |
| ScheduledToPresent | The offer is scheduled to be presented to the seller. |
| Finalized | The offer has been finalized. |
| Archived | The offer has been archived. |

**OfferReceivedStatus.** The lookup is open with enumerations; its standard values are:

| Lookup Value | Definition |
| :--- | :--- |
| Submitted | The offer has been submitted. |
| Delivered | The offer has been delivered to the receiving system. |
| Received | The offer has been received. |
| Acknowledged | The offer has been acknowledged. |
| Countered | The offer has been countered. |
| Accepted | The offer has been accepted. |
| Rejected | The offer has been rejected. |
| Withdrawn | The offer has been withdrawn. |
| Expired | The offer has expired. |
| RequestHighestAndBest | The highest and best offer has been requested. |
| Finalized | The offer has been finalized. |
| Archived | The offer has been archived. |

The definitions follow `DocumentStatus` in the Data Dictionary, so that an implementation already handling document status does not meet a second vocabulary for the same idea.

## Section 2.8: Activity Streams Mapping

Where an offer state corresponds to an Activity Streams activity whose intent matches, an implementation MUST use that activity. This is what keeps the exchange legible to a general ActivityPub client and is why no offer-specific vocabulary is added.

| Offer state | Activity Streams type | Note |
| :--- | :--- | :--- |
| Submitted | [`Offer`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-offer) | The activity that opens the thread. |
| Countered | [`Offer`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-offer) | A counter is an `Offer` posted `inReplyTo` the activity it answers. |
| Acknowledged | [`Read`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-read) | The receiving party has read the offer. |
| Accepted | [`Accept`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-accept) | |
| Rejected | [`Reject`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-reject) | |
| Withdrawn | [`Undo`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-undo) | An actor undoes only its own prior activity. |
| RequestHighestAndBest | [`Question`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-question) | Carries the deadline in the referenced payload. |

The remaining states have no Activity Streams equivalent whose intent matches, and an implementation MUST NOT invent one. `Delivered`, `Received`, `ScheduledToPresent`, `Finalized` and `Archived` are represented by a [`Note`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-note) in the thread, with the state itself in the referenced payload.

[`TentativeAccept`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-tentativeaccept) and [`TentativeReject`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-tentativereject) have no state in this specification. An implementation MAY post them to signal an intention that is not yet binding, and a consumer MUST NOT treat either as a state change.

**An absent state is not an error.** A consumer MUST NOT treat the absence of a state in the thread as a failure, and MUST NOT infer a state from the activity type alone where the payload is available. Where a state is not published in the thread, a consumer resolves it by dereferencing the payload ([Section 2.2](#section-22-activitypub-usage)). This applies to every element that is not required. A provider that publishes no states at all remains conformant, and its counterparties read the payload.

## Section 2.9: Counter Offers

A counter offer is not a new kind of record and is not an edit. It is an `OfferSubmission` ([Section 2.5](#section-25-the-offersubmission-resource)) carrying the same `OfferId` as the submission it answers, with its own `OfferSubmissionKey` and its own `CounterOfferSubmissionTimestamp`.

An implementation MUST NOT modify a prior submission when a counter is made. The sequence of submissions under one `OfferId`, ordered by timestamp, is the negotiation history, and destroying a prior turn destroys the record of what was agreed and when.

In the thread, a counter is an `Offer` posted `inReplyTo` the activity it answers ([Section 2.8](#section-28-activity-streams-mapping)). Re-countering repeats this: each turn replies to the one before it, so the thread is the same sequence the `OfferSubmission` records hold.

## Section 2.10: Web API Conformance

An implementation MAY expose these resources over OData on the RESO Web API without participating in any ActivityPub exchange. Such an implementation MUST satisfy the following, which are the parts of this specification that do not depend on the transport:

* The resources and fields of [Section 2.4](#section-24-the-offer-resource) through [Section 2.6](#section-26-the-offerpropertygroup-resource), with the types and nullability given.
* The lookups and standard values of [Section 2.7](#section-27-offer-states), single-valued on each side.
* The append-only rule for submissions ([Section 2.9](#section-29-counter-offers)).
* The identity rules of [Section 2.3](#section-23-offer-identity) as they apply to `OfferKey` and `OfferId`.
* The authorization rules of [Section 2.11](#section-211-authentication-and-authorization).

Where an implementation supports [Web API Add/Edit](https://github.com/RESOStandards/transport/blob/main/proposals/web-api-add-edit.md), creating a counter offer is a create against `OfferSubmission`, never an update of an existing submission.

## Section 2.11: Authentication and Authorization

Every link to an offer payload MUST be protected, and MUST refuse an unauthenticated dereference. An implementation MUST NOT publish an offer payload at an unauthenticated URL.

Authorization is by party. The parties to an offer are the submitting agent and their brokerage, the listing agent and their brokerage, and the systems acting on behalf of any of them. An implementation MUST NOT serve offer content to a requester outside that set, and MUST NOT include a party in one offer's authorization set on the strength of their involvement in another.

The buyer and co-buyer fields of [Section 2.5](#section-25-the-offersubmission-resource) are the most sensitive elements this specification defines. An implementation MAY omit them from a payload served to a party that does not require them, and a consumer MUST NOT treat their absence as an error.

## Section 2.12: Worked Examples

Worked examples of each sequence – submission, acknowledgement, counter, re-counter, acceptance, rejection, withdrawal, expiry and a request for highest and best – will be added here, in the form used by the ULI Resolution Protocol's Section 2.12: the request and response of each activity, with the referenced payload alongside it.

<br /><br />

# Section 3: Certification

Certification proves that an offer keeps its shape, its history and its confidentiality as it crosses systems. The rules derive from the Section 2 requirements. Certification is per-interface: an implementation may be certified for the Web API interface, the ActivityPub interface, or both.

RESO will validate the following during certification:

**Model**
* The candidate MUST serve the resources, fields, types and nullability of [Section 2.4](#section-24-the-offer-resource) through [Section 2.6](#section-26-the-offerpropertygroup-resource), and a payload it produces MUST validate as RESO Common Format against the declared Data Dictionary version.
* The candidate MUST accept and serve the standard values of [Section 2.7](#section-27-offer-states) and MUST reject a multi-valued status on either side.
* The candidate MUST use the existing standard values for `BuyerFinancing`, `Contingencies` and `BuyerBrokerageCompensation` and MUST NOT substitute offer-specific equivalents ([Section 2.5](#section-25-the-offersubmission-resource)).
* An `Offer` the candidate accepts MUST carry either `ListingId` or `ListingKey`, and an `OfferPropertyGroup` MUST identify the property by one of the three permitted combinations ([Section 2.6](#section-26-the-offerpropertygroup-resource)).

**History**
* On a counter, the candidate MUST create a new `OfferSubmission` and MUST leave every prior submission byte-identical ([Section 2.9](#section-29-counter-offers)). A candidate that modifies a prior submission fails.
* The submissions the candidate holds for one `OfferId`, ordered by timestamp, MUST reproduce the negotiation as it was conducted.

**Protocol**
* Every activity the candidate posts MUST use only Activity Streams 2.0 vocabulary and MUST NOT carry custom JSON-LD terms ([Section 2.2](#section-22-activitypub-usage)).
* An activity the candidate posts MUST NOT carry offer content; that content MUST be reachable only through the protected link the activity references ([Section 2.2](#section-22-activitypub-usage)).
* The candidate MUST use the mapped Activity Streams type for every state that has one, and MUST use a `Note` rather than an invented type for every state that does not ([Section 2.8](#section-28-activity-streams-mapping)).
* The candidate MUST NOT address an offer activity to the public collection ([Section 2.1](#section-21-participation-and-confidentiality)).
* The candidate MUST NOT fail when a counterparty publishes no state in the thread, and MUST resolve the state from the payload instead ([Section 2.8](#section-28-activity-streams-mapping)).
* The candidate MUST NOT parse an activity identifier to recover offer data, and MUST NOT require an identifier to contain an `OfferId` ([Section 2.3](#section-23-offer-identity)).

**Confidentiality**
* Every payload link the candidate publishes MUST refuse an unauthenticated dereference ([Section 2.11](#section-211-authentication-and-authorization)).
* The candidate MUST refuse to serve offer content to a requester outside the parties to that offer ([Section 2.11](#section-211-authentication-and-authorization)).
* The candidate MUST NOT treat the absence of buyer or co-buyer fields as an error ([Section 2.11](#section-211-authentication-and-authorization)).

<br /><br />

# Section 4: Contributors

This document was written by [Chris Haran](mailto:chris.haran@mredllc.com) and [Josh Darnell](mailto:josh@darnjo.com).

Thanks to the following contributors for their help with this project:

| Contributor | Company |
| --- | --- |
| | |

<br /><br />

# Section 5: References

Please see the following references for more information regarding topics covered in this document:
* [W3C ActivityPub](https://www.w3.org/TR/activitypub/)
* [W3C Activity Streams 2.0 Vocabulary](https://www.w3.org/TR/activitystreams-vocabulary/)
* [RESO Common Format](https://github.com/RESOStandards/transport/blob/main/proposals/reso-common-format.md)
* [RESO Listing Advertisement (RCP-52)](https://github.com/RESOStandards/transport/discussions/162)
* [ULI Resolution Protocol (RCP-54)](https://github.com/RESOStandards/transport/pull/222)
* [Organization and System Identifiers (RCP-55)](https://github.com/RESOStandards/transport/pull/243)
* [Feed Entitlements and Visibility (RCP-35)](https://github.com/RESOStandards/transport/pull/169)
* [RESO Data Dictionary](https://dd.reso.org/)

<br /><br />

# Section 6: Appendices

## Proposed Data Dictionary elements

This proposal introduces three resources and two lookups. They are defined in [Section 2.4](#section-24-the-offer-resource) through [Section 2.7](#section-27-offer-states) rather than repeated here.

| Resource | Defined in | Fields |
| :--- | :--- | ---: |
| Offer | [Section 2.4](#section-24-the-offer-resource) | 10 |
| OfferSubmission | [Section 2.5](#section-25-the-offersubmission-resource) | 32 |
| OfferPropertyGroup | [Section 2.6](#section-26-the-offerpropertygroup-resource) | 11 |

| Lookup | Defined in | Values |
| :--- | :--- | ---: |
| OfferSubmissionStatus | [Section 2.7](#section-27-offer-states) | 13 |
| OfferReceivedStatus | [Section 2.7](#section-27-offer-states) | 12 |

The following existing elements are reused without change: `BuyerFinancing`, `Contingency`, `BuyerBrokerageCompensation`, `StreetNumber`, `StreetName`, `City`, `StateOrProvince`, `PostalCode`, `CountyOrParish`, `Country`, `ParcelNumber`, `UniversalPropertyId`, `ListingId`, `ListingKey` and the `Media` resource.

This proposal deprecates no element.

| Resource | Deprecated Field | Replaced by | Note |
| :--- | :--- | :--- | :--- |
| None | | | |

## Design rationale

**Why the data is not in the activity.** Putting offer terms in an ActivityPub object publishes them to every server the activity federates to, and federation is not revocable. An offer is confidential, so the activity carries a reference and the data stays behind an authenticated link the originator controls. This also keeps the vocabulary standard, since nothing offer-specific has to be expressed in JSON-LD.

**Why the identifier need not be meaningful.** A provider that must expose `OfferId` in an activity identifier discloses, to anyone who can see the thread, how many offers it has issued and in what order. Allowing an opaque identifier removes that disclosure without weakening the reference, because the payload behind the link resolves the record.

**Why submissions are append-only.** A negotiation is evidence. If a counter overwrites the offer it answers, the record of what was offered, when, and by whom is lost, and the parties have no common account of what happened. Append-only keeps the sequence, and the sequence is what an offer is.

**Why two status lookups.** The submitting side and the receiving side observe different events. `Delivered` is knowable by the sender's system before the recipient has done anything, and `Received` is the recipient's statement. Collapsing them into one field would force one side to assert what the other side knows. Their values are close today and may diverge.

**Why an absent state is not an error.** Some systems will not publish offer states to a thread, for business reasons that are theirs to weigh. A specification that failed on absence would exclude those systems or push them to publish what they would rather not. Treating absence as a signal to dereference keeps them in the exchange and keeps the data where its owner wants it.

<br /><br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO Transport](mailto:transport@reso.org) with questions about this proposal, or [RESO developer support](mailto:dev@reso.org) with specific development questions.
