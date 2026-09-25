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
    - [Section 2.12.1: Submitting an Offer](#section-2121-submitting-an-offer)
    - [Section 2.12.2: Acknowledging Receipt](#section-2122-acknowledging-receipt)
    - [Section 2.12.3: Countering](#section-2123-countering)
    - [Section 2.12.4: A State With No Activity Streams Verb](#section-2124-a-state-with-no-activity-streams-verb)
    - [Section 2.12.5: Requesting Highest and Best](#section-2125-requesting-highest-and-best)
    - [Section 2.12.6: Accepting](#section-2126-accepting)
    - [Section 2.12.7: Withdrawing](#section-2127-withdrawing)
- [Section 3: Certification](#section-3-certification)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: Appendices](#section-6-appendices)
  - [Open Questions](#open-questions)
  - [Adoption of the Reused Elements](#adoption-of-the-reused-elements)
- [Section 7: License](#section-7-license)

<br /><br />

# Summary of Changes

* Moves offer handling out of the [RESO Listing Advertisement](https://github.com/RESOStandards/transport/discussions/162) proposal into its own endorsement. The Transport ActivityPub Subgroup directed the work to Interoperability in August 2025 and discussed a lightweight offer management specification in September 2025; Interoperability reported the specification back to the subgroup in October 2025 and voted in September 2026 to send its data elements to the Data Dictionary Workgroup.
* Introduces three Data Dictionary resources: `Offer`, `OfferSubmission` and `OfferPropertyGroup`, defined in [Section 2.4](#section-24-the-offer-resource) through [Section 2.6](#section-26-the-offerpropertygroup-resource).
* Introduces two lookups, `OfferSubmissionStatus` and `OfferReceivedStatus`, defined in [Section 2.7](#section-27-offer-states).
* Recommends that an implementation new to offer exchange adopt Unique Organization and System Identifiers from the outset, ahead of the Data Dictionary carrying them and of Data Dictionary 3.0 requiring one in certification. A provider whose only available value today is an originating system name or identifier remains conformant. See [Section 2.4](#section-24-the-offer-resource).
* Binds two kinds of implementer with one model: systems serving the resources over OData on the Web API, and systems exchanging offers over ActivityPub through offer management hubs.
* Amends [RCP-52](https://github.com/RESOStandards/transport/discussions/162) Section 2.3, whose worked example carries offer terms in the text of an activity. [Section 2.2](#section-22-activitypub-usage) places that content in the referenced payload instead.

<br /><br />

# Introduction

An offer is the point in a transaction where the most value and the most risk meet, and it is the point with the least standardization. Offers move as email attachments, as PDFs and through portals that each model an offer differently. A listing agent receiving offers from several buyer agents commonly receives them in several shapes and reconciles them by hand.

The Data Dictionary has no Offer resource. Of its 43 resources, none models an offer, and the only place offers appear at all is two values of `TransactionType`, `PurchaseOffer` and `LeaseOffer`, which classify a transaction rather than describe an offer. There is nothing to extend, so this proposal defines the shape.

Two things make an offer different from the records the Data Dictionary already carries, and both shape this specification.

An offer is a **conversation**, not a record. It is submitted, acknowledged, countered, countered again and finally accepted, rejected, withdrawn or expired. Each turn is a new statement by a different party, and the sequence is the substance. A single mutable row cannot represent it.

An offer is **confidential**. It carries the legal name, address and telephone number of a buyer, the price that buyer will pay and the financing behind it. This is the most sensitive data in the proposal, and possibly in the Data Dictionary. The design assumes confidentiality rather than adding it later.

<br /><br />

# Section 1: Purpose

This specification gives an offer a standard shape and a standard set of states, so that:

* a buyer agent can submit an offer from the system of their choice and have it arrive intact in a listing agent's system;
* a listing agent can compare offers from different sources side by side without rekeying them;
* a counter offer is a new statement in a thread rather than an edit that destroys what came before;
* the status of an offer is machine-readable on both sides, so that neither party has to telephone to ask;
* the parties to an offer, and only those parties, can read its contents; and
* the exchange is platform agnostic, so that a party sees every offer regardless of which product or platform each one came from.

The offer is the unit of scope. What happens after acceptance – the executed contract, escrow and title, contingency management, closing and archive – is transaction management, and is addressed separately. This specification may be referenced from that work but does not attempt it.

<br /><br />

# Section 2: Specification

## Section 2.1: Participation and Confidentiality

An offer is exchanged between named parties. Unlike a listing, an offer is not published to a network and is not discoverable. Participation is by being addressed: the buyer side addresses the listing side, and the listing side replies.

Offers are **private by design**. An activity conveying an offer or a change to one MUST be addressed to the specific parties entitled to see it, and SHOULD NOT use the public collection `https://www.w3.org/ns/activitystreams#Public`.

A public thread is a permitted alternative for a provider that wants one, under one condition: the activity MUST carry **no offer information whatsoever**. Every element of [Section 2.4](#section-24-the-offer-resource) through [Section 2.6](#section-26-the-offerpropertygroup-resource), and the offer states of [Section 2.7](#section-27-offer-states), MUST be reachable only through the authenticated payload ([Section 2.2](#section-22-activitypub-usage)), whether that payload is served from a RESO Web API or any other endpoint. A public activity therefore announces that something happened and nothing about what it was.

Whether the industry wants the fact of an offer to be public while its contents stay private is an open question ([Section 6, Open Questions](#open-questions)).

An Offer Hub and any intermediary relaying an activity MUST NOT store offer content. Offer data lives behind the originator's protected link ([Section 2.2](#section-22-activitypub-usage)) and is read only by parties the originator has authorized ([Section 2.11](#section-211-authentication-and-authorization)).

A provider decides what it exposes. The state of an offer MAY be withheld from the thread entirely, in which case a consumer reads it from the payload ([Section 2.8](#section-28-activity-streams-mapping)). Withholding lowers what counterparties can see without dereferencing, and does not break the exchange.

## Section 2.2: ActivityPub Usage

Implementations MUST use the standard [Activity Streams 2.0 vocabulary](https://www.w3.org/TR/activitystreams-vocabulary/), for example [`Offer`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-offer), [`Accept`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-accept), [`Reject`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-reject) and [`Note`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-note). Implementations MUST NOT extend [ActivityPub](https://www.w3.org/TR/activitypub/)'s JSON-LD with custom terms.

Offer content MUST be carried in the RESO Common Format payload that the activity references, and MUST NOT appear in the ActivityPub object. Offer content is every element of [Section 2.4](#section-24-the-offer-resource) through [Section 2.6](#section-26-the-offerpropertygroup-resource) **other than an identifier**: price, financing, contingencies, dates, buyer and co-buyer details, notes, status and media.

An identifier is not content. The identifiers of [Section 2.3](#section-23-offer-identity), and the members of the listing coordinate of [Section 2.4](#section-24-the-offer-resource), MAY appear in the activity, as an identifier does in a thread reference. A provider that would rather not publish them has the options in those sections: an opaque activity identifier, or a hashed coordinate.

The payload MUST be expressed in RESO Common Format. The payload MUST be reachable through a link the activity references, and MUST NOT be embedded in the activity. That link is not required to be a RESO Web API endpoint: any endpoint that returns the payload in RESO Common Format satisfies this specification. The link MUST refuse an unauthenticated dereference ([Section 2.11](#section-211-authentication-and-authorization)).

This rule is the same one adopted by the [ULI Resolution Protocol](https://github.com/RESOStandards/transport/pull/222) in its Section 2.2, and it is the reason this specification adds no vocabulary to ActivityPub. Where an offer concept has no Activity Streams equivalent, it is represented by the payload and not by a new term ([Section 2.8](#section-28-activity-streams-mapping)).

## Section 2.3: Offer Identity

Every offer and every submission carries an identifier. In an ActivityPub thread the identifier is the activity's `id`, a URL of the form used in the Listing Advertisement proposal:

```json
"id": "https://my.offercloud.com/offer/XYZ999"
```

The identifier MUST be unique. Where the identifier is visible, it MUST be immutable: an implementation MUST NOT reissue, renumber or recycle it.

The identifier is **not required to be** the `OfferKey`, the `OfferId` or any other element of [Section 2.4](#section-24-the-offer-resource). A provider that does not wish to expose a meaningful identifier in plain view MAY publish an opaque one and require a consumer to dereference the payload to resolve the underlying record. Implementations MUST NOT infer that the trailing segment of an identifier is an `OfferId`, and MUST NOT parse an identifier to recover offer data.

The choice is the provider's. A provider publishing meaningful identifiers is making a disclosure decision rather than a formatting one, and should make it deliberately.

## Section 2.4: The Offer Resource

The `Offer` resource is the top-level object. One `Offer` exists for one offer between two parties on one listing, and it persists for the life of that negotiation. The turns of the negotiation are `OfferSubmission` records ([Section 2.5](#section-25-the-offersubmission-resource)).

| Field | Type | Nullable | Max length | Lookup | Definition |
| :--- | :--- | :--- | :--- | :--- | :--- |
| OfferKey | String | No | 255 | | The unique system identifier for the offer. |
| OfferId | String | Yes | 255 | | The well-known identifier assigned to an offer by the system it originated in. |
| ListingId | String | Yes | 255 | | The well-known identifier of the listing the offer is made against. |
| ListingKey | String | Yes | 255 | | The system identifier of the listing the offer is made against. |
| OfferNotes | String | Yes | | | Notes that apply to the offer as a whole rather than to one submission. |
| OfferOriginatingSystemId | String | Yes | 255 | | The originating system identifier of the listing being offered on, as carried in current practice. |
| OfferOriginatingSystemName | String | Yes | 255 | | The name of the system with authoritative control over the listing being offered on. |
| OfferSourceSystemId | String | Yes | 255 | | The source system identifier of the listing record, which may differ from the originating system. |
| OfferSourceSystemName | String | Yes | 255 | | The name of the system the listing record was directly received from. |
| OfferUoi | String | Yes | 25 | | The Unique Organization Identifier of the organization the listing being offered on originated with. |
| OfferUsi | String | Yes | 25 | | The Unique System Identifier of the system, within that organization, the listing being offered on was input in. |
| ModificationTimestamp | Timestamp | No | | | The date and time the offer record was last modified. |

An offer that identifies no listing cannot be routed to a listing agent, so an `Offer` MUST identify one. A listing identifier alone is not sufficient to do that unambiguously.

`ListingId` is human-friendly, often short and often numeric, and two unrelated organizations can issue the same value. `ListingKey` has the same exposure, since a system numbering its listings from one collides with every other system that does. Neither is globally unique on its own.

An `Offer` therefore carries a **coordinate** rather than a single identifier: a listing identifier, plus the organization or system that issued it. An `Offer` MUST carry `ListingId` or `ListingKey`, and MUST carry at least one of `OfferUoi`, `OfferOriginatingSystemName` or `OfferOriginatingSystemId`. More of them narrow the coordinate further, and `OfferUsi` narrows it to the system a listing was input on rather than to the organization alone.

`OfferUoi` and `OfferOriginatingSystemId` overlap deliberately. The Data Dictionary defines an originating system identifier as holding an organization identifier, so the element is named for a system and carries an organization, and it cannot express the system within that organization at all. `OfferUoi` names the organization explicitly and `OfferUsi` adds the system, which the legacy pair never distinguished.

RESO is transitioning to Unique Organization Identifiers through [Organization and System Identifiers (RCP-55)](https://github.com/RESOStandards/transport/pull/243). The expected path is that the Data Dictionary carries them in a minor version, and that Data Dictionary 3.0 requires a Unique Organization Identifier in certification where a provider would otherwise supply only an originating system name or identifier.

An implementation new to offer exchange SHOULD support `OfferUoi` and `OfferUsi` from the outset. There is no legacy to preserve in a system that has not exchanged offers before, and starting on the identifier the standard is moving to avoids a migration later. A provider whose only available value today is an originating system name or identifier remains conformant and SHOULD continue to populate it. Where RCP-55 ratifies first, these fields adopt its definitions rather than restating them.

An organization that hosts offers and does not already hold a Unique Organization Identifier can be issued one by RESO. This is what makes the coordinate work in practice rather than in principle: a locally issued identifier is collision-free once it is qualified by a centrally issued organization identifier, because no two organizations share one. A participant therefore does not have to change how it numbers its own records, and does not need to be an MLS to take part.

The members of a coordinate MAY be hashed together to produce a single opaque value, where a provider does not wish to publish the parts. A party already holding the parts can verify such a value; a party that does not, cannot read them out of it.

## Section 2.5: The OfferSubmission Resource

An `OfferSubmission` is one turn in the negotiation: an initial offer, a counter or a re-counter. Submissions are threaded onto one `Offer` by `OfferId`, and they are append-only. An implementation MUST NOT modify a submission to represent a counter; it MUST create a new one ([Section 2.9](#section-29-counter-offers)).

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
| RequestedClosingDate | Date | Yes | | | The closing date proposed in the offer. |
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
| OfferAcceptedTimestamp | Timestamp | Yes | | | The date and time the offer was accepted. |
| ModificationTimestamp | Timestamp | No | | | The date and time the submission was last modified. |

`BuyerFinancing`, `Contingencies` and `BuyerBrokerageCompensation` reuse existing Data Dictionary elements and their lookups rather than introducing offer-specific equivalents. An implementation MUST use the existing standard values.

`RequestedClosingDate` is a new element rather than a reuse of `CloseDate`. `CloseDate` records the date a transaction actually closed; this records the date an offer proposes. The two are different facts and one cannot stand for the other, so the offer element takes a name that says which it is.

Some providers do not offer compensation information. `BuyerBrokerageCompensation` is therefore optional, and a consumer MUST NOT treat its absence as an error. Where compensation is not present and a party needs it, it is obtained by contacting the agent or brokerage directly.

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

Every field of this resource reuses an existing Data Dictionary element.

An `OfferPropertyGroup` MUST carry enough to identify the property, by one of two combinations:

* `ParcelNumber` with `StateOrProvince` and `CountyOrParish`; or
* `StreetNumber`, `StreetName`, `City`, `StateOrProvince` and `PostalCode`.

`OfferPropertyGroupKey` identifies the property group within the system that issued it, and is resolved by dereferencing the payload that carries it, as the other keys this proposal introduces are ([Section 2.3](#section-23-offer-identity)).

`UniversalPropertyId` is an optional additional discriminator. It identifies the property rather than the listing, and a property may carry many listings over time, so it does not replace the listing coordinate of [Section 2.4](#section-24-the-offer-resource). What it does is rule out two listings that could not be the same property, which is where offers most often go wrong across systems.

A universal property identifier composed in the plain form embeds a parcel number. Where a provider does not wish to publish one, the opaque form defined by the RESO universal property identifier work SHOULD be used instead: a party already holding the components can verify it, and a party that does not cannot recover them.

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

A system that hosts offer threads on behalf of participants is an **Offer Hub**. The term is used throughout this specification for that role, whether the host is an offer management product, a brokerage or an MLS.

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

Each example shows the activity posted to the thread and the RESO Common Format payload the activity references. The activity carries identity and intent. The payload carries the offer. In every example the payload is retrieved from the `url` of the activity and requires authorization ([Section 2.11](#section-211-authentication-and-authorization)).

The thread is the listing thread established by the Listing Advertisement proposal. An offer joins it by replying to the listing activity.

### Section 2.12.1: Submitting an Offer

The buyer agent's system posts an `Offer` in reply to the listing, addressed to the listing agent and not to the public collection.

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Offer",
  "id": "https://my.offercloud.com/offer/XYZ999",
  "published": "2026-09-15T14:02:00Z",
  "actor": "https://listing.network/AmyAgent",
  "attributedTo": ["https://listing.network/MyOfferApp"],
  "inReplyTo": "https://listing.network/BobAgent/133",
  "to": ["https://listing.network/BobAgent"],
  "url": {
    "type": "Link",
    "href": "https://my.offercloud.com/payload/XYZ999",
    "mediaType": "application/json"
  }
}
```

The identifier `XYZ999` is opaque. It is not the `OfferId` and cannot be parsed to recover one ([Section 2.3](#section-23-offer-identity)). Nothing in the activity states the price, the buyer or the terms.

The payload behind `url`:

```json
{
  "@reso.context": "urn:reso:metadata:2.1:resource:offersubmission",
  "OfferSubmissionKey": "9f2c7a10-5b3e-4a1d-9c88-2e6b0d4f1a73",
  "OfferId": "MRED-2026-0004412",
  "OfferAgentId": "AG55021",
  "OfferBuyerLegalName": "Dana R. Whitfield",
  "OfferBuyerPhone": "+1-312-555-0147",
  "PurchasePrice": 530000,
  "EarnestMoney": 15000,
  "RequestedClosingDate": "2026-11-02",
  "BuyerFinancing": "Conventional",
  "Contingencies": ["Inspection", "Financing"],
  "AsIsCondition": false,
  "OfferExpirationDate": "2026-09-18",
  "OfferSubmissionStatus": "Submitted",
  "OfferSubmissionTimestamp": "2026-09-15T14:02:00Z",
  "ModificationTimestamp": "2026-09-15T14:02:00Z",
  "OfferPropertyGroup": {
    "UniversalPropertyId": "US-17031-N-1234567890-R-N",
    "StreetNumber": "1803",
    "StreetName": "Bayshore Rd",
    "City": "Chicago",
    "StateOrProvince": "IL",
    "PostalCode": "60614",
    "CountyOrParish": "Cook",
    "Country": "US"
  }
}
```

The submission belongs to an `Offer`, which carries the listing coordinate ([Section 2.4](#section-24-the-offer-resource)) and persists for the life of the negotiation:

```json
{
  "@reso.context": "urn:reso:metadata:2.1:resource:offer",
  "OfferKey": "3d51a08c-9f47-4c62-b0aa-71e5d2c84b19",
  "OfferId": "MRED-2026-0004412",
  "ListingId": "11284417",
  "ListingKey": "MRED-L-11284417",
  "OfferUoi": "M00000136",
  "OfferUsi": "50039",
  "OfferOriginatingSystemName": "Midwest Real Estate Data",
  "OfferNotes": "Buyer is relocating and has asked for an early response.",
  "ModificationTimestamp": "2026-09-15T14:02:00Z"
}
```

`ListingId` alone would not identify this listing: another organization may issue `11284417` for something else entirely. `OfferUoi` supplies the organization and `OfferUsi` the system it was input in, and together with the listing identifier they form the coordinate. A provider that would rather not publish the parts may carry a single hashed value in their place.

### Section 2.12.2: Acknowledging Receipt

The listing agent's system acknowledges the offer. `Acknowledged` maps to `Read` ([Section 2.8](#section-28-activity-streams-mapping)).

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Read",
  "id": "https://listing.network/BobAgent/133/ack/1",
  "published": "2026-09-15T15:40:00Z",
  "actor": "https://listing.network/BobAgent",
  "object": "https://my.offercloud.com/offer/XYZ999",
  "to": ["https://listing.network/AmyAgent"]
}
```

The receiving side records `OfferReceivedStatus` as `Acknowledged` in its own payload. No offer content moves in either direction here.

### Section 2.12.3: Countering

A counter is an `Offer` posted `inReplyTo` the activity it answers, by the other party. It creates a new `OfferSubmission` under the same `OfferId`; it does not modify the first ([Section 2.9](#section-29-counter-offers)).

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Offer",
  "id": "https://bobsbrokerage.example/counter/7731",
  "published": "2026-09-15T18:20:00Z",
  "actor": "https://listing.network/BobAgent",
  "inReplyTo": "https://my.offercloud.com/offer/XYZ999",
  "to": ["https://listing.network/AmyAgent"],
  "url": {
    "type": "Link",
    "href": "https://bobsbrokerage.example/payload/7731",
    "mediaType": "application/json"
  }
}
```

The payload is a second `OfferSubmission`, carrying the same `OfferId` and its own key:

```json
{
  "@reso.context": "urn:reso:metadata:2.1:resource:offersubmission",
  "OfferSubmissionKey": "b41d8e56-7c09-42fa-8d31-5a7e2c9b6f04",
  "OfferId": "MRED-2026-0004412",
  "PurchasePrice": 545000,
  "EarnestMoney": 20000,
  "RequestedClosingDate": "2026-10-26",
  "Contingencies": ["Financing"],
  "OfferExpirationDate": "2026-09-17",
  "OfferSubmissionStatus": "Countered",
  "CounterOfferSubmissionTimestamp": "2026-09-15T18:20:00Z",
  "ModificationTimestamp": "2026-09-15T18:20:00Z"
}
```

Both submissions remain retrievable. Ordered by timestamp under `MRED-2026-0004412`, they are the negotiation.

### Section 2.12.4: A State With No Activity Streams Verb

`ScheduledToPresent` has no Activity Streams equivalent, so it is a `Note` and the state itself stays in the payload. An implementation MUST NOT invent a type for it.

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Note",
  "id": "https://listing.network/BobAgent/133/note/4",
  "published": "2026-09-15T16:05:00Z",
  "actor": "https://listing.network/BobAgent",
  "inReplyTo": "https://my.offercloud.com/offer/XYZ999",
  "to": ["https://listing.network/AmyAgent"],
  "url": {
    "type": "Link",
    "href": "https://bobsbrokerage.example/payload/7728",
    "mediaType": "application/json"
  }
}
```

A provider that does not publish states at all posts no activity here. Its counterparty resolves the state by dereferencing the payload, and MUST NOT treat the silence as an error ([Section 2.8](#section-28-activity-streams-mapping)).

### Section 2.12.5: Requesting Highest and Best

`RequestHighestAndBest` maps to `Question`. The deadline is in the payload, not in the activity.

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Question",
  "id": "https://listing.network/BobAgent/133/hb/1",
  "published": "2026-09-16T09:00:00Z",
  "actor": "https://listing.network/BobAgent",
  "inReplyTo": "https://listing.network/BobAgent/133",
  "to": [
    "https://listing.network/AmyAgent",
    "https://listing.network/CarlaAgent"
  ],
  "url": {
    "type": "Link",
    "href": "https://bobsbrokerage.example/payload/hb1",
    "mediaType": "application/json"
  }
}
```

The request is addressed to each offering party individually. It MUST NOT be addressed to the public collection, and one offering party MUST NOT be able to learn the terms of another's offer from it ([Section 2.1](#section-21-participation-and-confidentiality)).

### Section 2.12.6: Accepting

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Accept",
  "id": "https://listing.network/BobAgent/133/accept/1",
  "published": "2026-09-16T17:12:00Z",
  "actor": "https://listing.network/BobAgent",
  "object": "https://bobsbrokerage.example/counter/7731",
  "to": ["https://listing.network/AmyAgent"],
  "url": {
    "type": "Link",
    "href": "https://bobsbrokerage.example/payload/7740",
    "mediaType": "application/json"
  }
}
```

The `object` is the submission being accepted, which in a negotiation that has countered is the most recent counter rather than the original offer. The payload records `OfferAcceptedTimestamp` and sets the status on both sides.

Acceptance ends the scope of this specification. What follows is transaction management.

### Section 2.12.7: Withdrawing

An actor withdraws its own offer with `Undo`. An actor MUST NOT `Undo` an activity posted by another party.

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Undo",
  "id": "https://my.offercloud.com/offer/XYZ999/undo",
  "published": "2026-09-15T17:55:00Z",
  "actor": "https://listing.network/AmyAgent",
  "object": "https://my.offercloud.com/offer/XYZ999",
  "to": ["https://listing.network/BobAgent"]
}
```

`Undo` withdraws the offer going forward. It does not delete the submission: the record of what was offered, and that it was withdrawn, remains ([Section 2.9](#section-29-counter-offers)).

<br /><br />

# Section 3: Certification

Certification proves that an offer keeps its shape, its history and its confidentiality as it crosses systems. The rules derive from the Section 2 requirements. Certification is per-interface: an implementation may be certified for the Web API interface, the ActivityPub interface or both.

RESO will validate the following during certification:

**Model**
* The candidate MUST serve the resources, fields, types and nullability of [Section 2.4](#section-24-the-offer-resource) through [Section 2.6](#section-26-the-offerpropertygroup-resource), and a payload it produces MUST validate as RESO Common Format against the declared Data Dictionary version.
* The candidate MUST accept and serve the standard values of [Section 2.7](#section-27-offer-states) and MUST reject a multi-valued status on either side.
* The candidate MUST use the existing standard values for `BuyerFinancing`, `Contingencies` and `BuyerBrokerageCompensation` and MUST NOT substitute offer-specific equivalents ([Section 2.5](#section-25-the-offersubmission-resource)).
* An `Offer` the candidate accepts MUST carry `ListingId` or `ListingKey`, and MUST carry at least one of `OfferUoi`, `OfferOriginatingSystemName` or `OfferOriginatingSystemId`. A candidate that accepts a listing identifier with no organization or system member fails ([Section 2.4](#section-24-the-offer-resource)).
* An `OfferPropertyGroup` the candidate accepts MUST identify the property by one of the two permitted combinations ([Section 2.6](#section-26-the-offerpropertygroup-resource)).
* Where the candidate publishes a hashed coordinate, it MUST be reproducible: the same listing MUST yield the same value on repeated construction ([Section 2.4](#section-24-the-offer-resource)).

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
* The candidate MUST NOT post an `Undo` for an activity another party posted ([Section 2.12.7](#section-2127-withdrawing)).

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
* [RESO Web API Add/Edit](https://github.com/RESOStandards/transport/blob/main/proposals/web-api-add-edit.md)
* [RESO Universal Property Identifier](https://upi.reso.org/)
* [DocumentStatus, RESO Data Dictionary](https://dd.reso.org/DD2.0/Property/DocumentStatus/)

<br /><br />

# Section 6: Appendices

## Proposed Data Dictionary elements

This proposal introduces three resources and two lookups. They are defined in [Section 2.4](#section-24-the-offer-resource) through [Section 2.7](#section-27-offer-states) rather than repeated here.

| Resource | Defined in | Fields |
| :--- | :--- | ---: |
| Offer | [Section 2.4](#section-24-the-offer-resource) | 12 |
| OfferSubmission | [Section 2.5](#section-25-the-offersubmission-resource) | 32 |
| OfferPropertyGroup | [Section 2.6](#section-26-the-offerpropertygroup-resource) | 11 |

| Lookup | Defined in | Values |
| :--- | :--- | ---: |
| OfferSubmissionStatus | [Section 2.7](#section-27-offer-states) | 13 |
| OfferReceivedStatus | [Section 2.7](#section-27-offer-states) | 12 |

The following existing elements are reused without change: `BuyerFinancing`, `Contingency`, `BuyerBrokerageCompensation`, `StreetNumber`, `StreetName`, `City`, `StateOrProvince`, `PostalCode`, `CountyOrParish`, `Country`, `ParcelNumber`, `UniversalPropertyId`, `ListingId`, `ListingKey` and the `Media` resource.

## Open Questions

These are recorded rather than settled, and are for the workgroups.

**Should the fact of an offer be public?** [Section 2.1](#section-21-participation-and-confidentiality) makes offers private by design and permits a public thread only where the activity carries no offer information at all. Whether the industry wants more than that, for example publishing that a listing has received offers without publishing anything about them, was raised in the ActivityPub Subgroup in August 2025 and has not been decided. There is precedent for wanting it: buyers are commonly notified how many competing offers exist, and a status of this kind has been requested of the Data Dictionary before.

**Should a universal property identifier be required?** [Section 2.6](#section-26-the-offerpropertygroup-resource) makes it optional. Requiring it would strengthen cross-system matching and would exclude providers who cannot compose one.

**Which offer elements should the endorsement require?** Several elements this proposal reuses are sparsely populated in the industry data, but that data is drawn almost entirely from MLSs and offers are not MLS domain. An offer management provider implementing this endorsement would populate them. Requiring a subset is therefore viable and is a question for the workgroup rather than an inference from current adoption.

**How is a hashed coordinate constructed?** [Section 2.4](#section-24-the-offer-resource) permits the members of a listing coordinate to be hashed together into a single opaque value, and [Section 2.6](#section-26-the-offerpropertygroup-resource) permits the same for a universal property identifier. A hash is only useful if it is comparable: two providers hashing the same listing must produce the same value, or neither can verify the other. That requires an agreed algorithm, an agreed order for the members, and agreed normalization of each member before hashing, none of which this proposal fixes. Whether the construction should be shared with the opaque universal property identifier, and whether hashing should be optional or the default, are open.

**What becomes of `OfferIndication` and `OfferRevocation`?** Both appear in earlier working material. An offer indication signals that a signed offer exists, with the property, the irrevocability period and where it was submitted, and has a direct analogue in Canadian practice. A revocation is not a record but an unresolved question about how an offer is unwound and by whom.

## Adoption of the Reused Elements

Element counts below are from the RESO Data Dictionary Industry Aggregates, June 2026, across 424 reporting markets.

**Read the denominator first.** 95.3% of that population is MLSs. For listing identity, address and member elements, which are MLS domain, the counts are meaningful. For offer-specific elements they are not, because an MLS has no reason to carry them, and a low count there is evidence about the population measured rather than about the element.

Meaningful for this proposal:

| Element | Markets | Share |
| :--- | ---: | ---: |
| `ListingId` | 419 | 98.8% |
| `ListingKey` | 416 | 98.1% |
| `OriginatingSystemName` | 397 | 93.6% |
| `OriginatingSystemId` | 311 | 73.3% |
| `StateOrProvince` | 415 | 97.9% |
| `CountyOrParish` | 394 | 92.9% |
| `ParcelNumber` | 368 | 86.8% |
| `Country` | 177 | 41.7% |
| `UniversalPropertyId` | 90 | 21.2% |

Two consequences, both about what a listing can be identified by rather than about what an offer participant must supply. `OriginatingSystemName` is better populated than `OriginatingSystemId`, which is why the coordinate in [Section 2.4](#section-24-the-offer-resource) accepts either. And while only 21.2% publish a universal property identifier, 83.0% already publish the parts needed to compose one, which is why [Section 2.6](#section-26-the-offerpropertygroup-resource) treats it as an optional discriminator rather than a requirement.

These counts describe listing data as MLSs publish it. They do not describe what an Offer Hub or an offer management provider is required to carry, and they are not an argument against requiring more of an offer participant than an MLS happens to publish today. The recommendation in [Section 2.4](#section-24-the-offer-resource) that new implementations start with `OfferUoi` rests on where the standard is going, not on these figures.

`Country` is the weakest component, present in 41.7% of markets and thinly populated where present, so it is defaulted rather than required.

This proposal deprecates no element.

| Resource | Deprecated Field | Replaced by | Note |
| :--- | :--- | :--- | :--- |
| None | | | |

## Design rationale

**Why the data is not in the activity.** Putting offer terms in an ActivityPub object publishes them to every server the activity federates to, and federation is not revocable. An offer is confidential, so the activity carries a reference and the data stays behind an authenticated link the originator controls. This also keeps the vocabulary standard, since nothing offer-specific has to be expressed in JSON-LD.

**Why the identifier need not be meaningful.** A provider that must expose `OfferId` in an activity identifier discloses, to anyone who can see the thread, how many offers it has issued and in what order. Allowing an opaque identifier removes that disclosure without weakening the reference, because the payload behind the link resolves the record.

**Why submissions are append-only.** A negotiation is evidence. If a counter overwrites the offer it answers, the record of what was offered, when and by whom is lost, and the parties have no common account of what happened. Append-only keeps the sequence, and the sequence is what an offer is.

**Why two status lookups.** The submitting side and the receiving side observe different events. `Delivered` is knowable by the sender's system before the recipient has done anything, and `Received` is the recipient's statement. Collapsing them into one field would force one side to assert what the other side knows. Their values are close today and may diverge.

**Why an absent state is not an error.** Some systems will not publish offer states to a thread, for business reasons that are theirs to weigh. A specification that failed on absence would exclude those systems or push them to publish what they would rather not. Treating absence as a signal to dereference keeps them in the exchange and keeps the data where its owner wants it.

<br /><br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO Transport](mailto:transport@reso.org) with questions about this proposal, or [RESO developer support](mailto:dev@reso.org) with specific development questions.
