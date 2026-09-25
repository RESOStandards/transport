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
    - [Primitives This Specification Reuses](#primitives-this-specification-reuses)
  - [Section 2.3: Offer Identity](#section-23-offer-identity)
  - [Section 2.4: The Offer Resource](#section-24-the-offer-resource)
    - [How Many Offers a Buyer May Have on a Listing](#how-many-offers-a-buyer-may-have-on-a-listing)
  - [Section 2.5: The OfferSubmission Resource](#section-25-the-offersubmission-resource)
  - [Section 2.6: The OfferPropertyGroup Resource](#section-26-the-offerpropertygroup-resource)
  - [Section 2.7: Offer States](#section-27-offer-states)
    - [Where the Current State Is](#where-the-current-state-is)
    - [How a State Moves](#how-a-state-moves)
  - [Section 2.8: Activity Streams Mapping](#section-28-activity-streams-mapping)
  - [Section 2.9: Counter Offers](#section-29-counter-offers)
    - [The Thread](#the-thread)
    - [Ordering](#ordering)
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
    - [Section 2.12.8: Public Addressing](#section-2128-public-addressing)
- [Section 3: Certification](#section-3-certification)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: Appendices](#section-6-appendices)
  - [Open Questions](#open-questions)
  - [Adoption of the Reused Elements](#adoption-of-the-reused-elements)
  - [Public Advertising, Side by Side](#public-advertising-side-by-side)
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
* the exchange is platform agnostic, so that a party sees every offer regardless of which product or platform each one came from; and
* every participating organization holds a Unique Organization Identifier, so that it can issue its own local identifiers without collision.

The offer is the unit of scope. What happens after acceptance – the executed contract, escrow and title, contingency management, closing and archive – is transaction management, and is addressed separately. This specification may be referenced from that work but does not attempt it.

<br /><br />

# Section 2: Specification

## Section 2.1: Participation and Confidentiality

An offer is exchanged between named parties. Unlike a listing, an offer is not published to a network and is not discoverable. Participation is by being addressed: the buyer side addresses the listing side, and the listing side replies.

**A participant MUST be resolvable to a Unique Organization Identifier or a Unique System Identifier.** An Offer Hub, and any party exchanging offers under this specification, MUST post under an actor that resolves to one of the two.

The rule applies per actor, not once per exchange. An Offer Hub is itself a system and resolves to its own identifier. A client posting from another system is a separate actor and resolves to that system's identifier. Both appear in the same thread, and they do not share an identity.

Which of the two identifiers an actor resolves to depends on what that actor is. An organization resolves to its own organization identifier. A party acting on behalf of one resolves by affiliation, since an agent is not an organization but the brokerage the agent acts for is. A system resolves to its system identifier, which names the system rather than the organization running it, and is the finer of the two where an organization runs more than one.

This is a requirement on who takes part, and it is separable from how a listing is identified. A participant is a party to the exchange and is therefore accountable within it, whereas a listing carries whatever provenance the system it originated in published ([Section 2.4](#section-24-the-offer-resource)).

There is no legacy to accommodate here. Offer exchange under this specification is new, so every participant is new to it, and requiring an identifier of each one costs nothing that already exists. An organization that does not hold one is issued one by RESO.

The examples in this specification show offers on a network assumed to require authorization to view. Public advertising is equally possible, with separate authorization still governing the underlying data. Which of the two an implementation chooses depends on its own business needs.

The two controls are independent. Addressing decides who learns that something happened. Authentication on the payload decides who learns what it was, and the payload is protected under either model ([Section 2.11](#section-211-authentication-and-authorization)).

**Addressed to named parties.** The form the examples use. An activity is addressed to the parties entitled to see it, and the thread is not discoverable.

**Addressed publicly.** An activity MAY be addressed to the public collection `https://www.w3.org/ns/activitystreams#Public` on one condition: it MUST carry no offer information. Every element of [Section 2.4](#section-24-the-offer-resource) through [Section 2.6](#section-26-the-offerpropertygroup-resource), and the offer states of [Section 2.7](#section-27-offer-states), MUST remain reachable only through the authenticated payload. Such an activity announces that something happened and nothing about what it was. [Section 2.12.8](#section-2128-public-addressing) shows the form.

Where an activity carries offer content, it MUST NOT be addressed to the public collection. That is the single rule that separates the two.

An Offer Hub and any intermediary relaying an activity MUST NOT store offer content. Offer data lives behind the originator's protected link ([Section 2.2](#section-22-activitypub-usage)) and is read only by parties the originator has authorized ([Section 2.11](#section-211-authentication-and-authorization)).

A provider decides what it exposes. The state of an offer MAY be withheld from the thread entirely, in which case a consumer reads it from the payload ([Section 2.8](#section-28-activity-streams-mapping)). Withholding lowers what counterparties can see without dereferencing, and does not break the exchange.

## Section 2.2: ActivityPub Usage

Implementations MUST use the standard [Activity Streams 2.0 vocabulary](https://www.w3.org/TR/activitystreams-vocabulary/), for example [`Offer`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-offer), [`Accept`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-accept), [`Reject`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-reject) and [`Note`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-note). Implementations MUST NOT extend [ActivityPub](https://www.w3.org/TR/activitypub/)'s JSON-LD with custom terms.

Offer content MUST be carried in the RESO Common Format payload that the activity references, and MUST NOT appear in the ActivityPub object. Offer content is every element of [Section 2.4](#section-24-the-offer-resource) through [Section 2.6](#section-26-the-offerpropertygroup-resource) **other than an identifier**: price, financing, contingencies, dates, buyer and co-buyer details, notes, status and media.

An identifier is not content. The identifiers of [Section 2.3](#section-23-offer-identity), and the members of the listing coordinate of [Section 2.4](#section-24-the-offer-resource), MAY appear in the activity, as an identifier does in a thread reference. A provider that would rather not publish them has the options in those sections: an opaque activity identifier, or a hashed coordinate.

The payload MUST be expressed in RESO Common Format. The payload MUST be reachable through a link the activity references, and MUST NOT be embedded in the activity. That link is not required to be a RESO Web API endpoint: any endpoint that returns the payload in RESO Common Format satisfies this specification. The link MUST refuse an unauthenticated dereference ([Section 2.11](#section-211-authentication-and-authorization)).

This rule is the same one adopted by the [ULI Resolution Protocol](https://github.com/RESOStandards/transport/pull/222) in its Section 2.2, and it is the reason this specification adds no vocabulary to ActivityPub. Where an offer concept has no Activity Streams equivalent, it is represented by the payload and not by a new term ([Section 2.8](#section-28-activity-streams-mapping)).

### Primitives This Specification Reuses

An offer does not invent a way to reach the network. It reuses the primitives the [Listing Advertisement](https://github.com/RESOStandards/transport/discussions/162) proposal establishes for a listing thread. That proposal is not yet promoted, and offer management is the first part of it to be specified on its own, so the primitives are stated here rather than cited. An implementation reading only this document has what it needs.

**An activity is created through its actor's outbox.** A client POSTs the object it wants to publish to its own ActivityPub server. The server wraps it in the activity, assigns the `id`, and delivers it to the inboxes of the parties addressed. The worked examples of [Section 2.12](#section-212-worked-examples) show the resulting activity, which is what a recipient sees. They do not show the POST that produced it, and an implementation MUST NOT read them as requiring a client to author an `id`.

**The server assigns the identifier.** This is why an `id` need not be meaningful and why an opaque one is available to any provider that wants it ([Section 2.3](#section-23-offer-identity)). A server MAY assign an identifier that embeds the actor and the thread, and a consumer MUST NOT depend on either shape.

**A reply names its parent by `inReplyTo`.** Every turn in a negotiation is a reply, and the shape of the thread is recoverable from those references alone ([Section 2.9](#section-29-counter-offers)).

**A payload is referenced, never embedded.** The activity carries a `Link` in `url` with a `mediaType`, and the payload behind it is RESO Common Format.

**A listing reaches the hub in one of two forms.** The root activity's link MAY resolve to a listing served over the RESO Web API, or to RESO Common Format served by any HTTP host. Both satisfy this specification and an implementation MUST accept either. Dereferencing is the same in both cases, under the rules of [Section 2.11](#section-211-authentication-and-authorization), so a consumer needs no separate credential model for one or the other.

**A listing is open to offers once it has been published to the hub for offers.** That act is what opens it, and the activity carrying it is the root of the thread an offer replies into ([Section 2.9](#section-29-counter-offers)). Eligibility does not follow from a listing's marketing phase. A listing being prepared for market is not open to offers merely by existing, and a provider that wants offers on a premarketed listing MAY publish it for offers before it goes to market. `ComingSoon` in `StandardStatus` is the ordinary case.

## Section 2.3: Offer Identity

Every offer and every submission carries an identifier. In an ActivityPub thread the identifier is the activity's `id`, a URL of the form used in the Listing Advertisement proposal:

```json
"id": "https://my.offercloud.com/offer/XYZ999"
```

The identifier MUST be unique. Where the identifier is visible, it MUST be immutable: an implementation MUST NOT reissue, renumber or recycle it.

The identifier is **not required to be** the `OfferKey`, the `OfferId` or any other element of [Section 2.4](#section-24-the-offer-resource). A provider that does not wish to expose a meaningful identifier in plain view MAY publish an opaque one and require a consumer to dereference the payload to resolve the underlying record. Implementations MUST NOT infer that the trailing segment of an identifier is an `OfferId`, and MUST NOT parse an identifier to recover offer data.

The choice is the provider's. A provider publishing meaningful identifiers is making a disclosure decision rather than a formatting one, and should make it deliberately.

## Section 2.4: The Offer Resource

The `Offer` resource is the top-level object. One `Offer` is one negotiation between two parties on one listing, and it persists for the life of that negotiation. The turns of the negotiation are `OfferSubmission` records ([Section 2.5](#section-25-the-offersubmission-resource)).

### How Many Offers a Buyer May Have on a Listing

More than one. The distinction that decides it is whether an act continues a negotiation or starts one. A new turn in a negotiation already under way is an `OfferSubmission` on the existing `Offer`. A new negotiation is a new `Offer` with its own `OfferId`.

Countering, re-countering and answering a request for highest and best are all turns, so they are submissions and they MUST NOT create a second `Offer` ([Section 2.9](#section-29-counter-offers)).

A negotiation that has ended does not reopen. Where an offer was withdrawn, rejected or expired and the same buyer offers again on the same listing, that is a new negotiation and MUST be a new `Offer`. An implementation MUST NOT append a submission to an ended offer.

A buyer MAY also hold more than one live `Offer` on one listing at the same time, where the offers are genuine alternatives rather than successive turns. A cash offer at one price and a financed offer at a higher one is the ordinary case, and the seller is being asked to choose between them rather than to answer a revision.

A consumer therefore MUST NOT assume at most one `Offer` per buyer on a listing, and MUST NOT key, deduplicate or match on the listing coordinate together with buyer identity. `OfferId` is what distinguishes one negotiation from another ([Section 2.3](#section-23-offer-identity)).

What a buyer cannot do is counter another buyer's offer. No offering party learns another's terms ([Section 2.11](#section-211-authentication-and-authorization)), so there is nothing for such a party to answer. Competing offers are answered by the listing side, each on its own `Offer`.

| Field | Type | Nullable | Max length | Lookup | Definition |
| :--- | :--- | :--- | :--- | :--- | :--- |
| OfferKey | String | No | 255 | | The unique system identifier for the offer. |
| OfferId | String | No | 255 | | The well-known identifier assigned to an offer by the system it originated in. Every `OfferSubmission` correlates to its `Offer` by this value. |
| ListingId | String | Yes | 255 | | The well-known identifier of the listing the offer is made against. |
| ListingKey | String | Yes | 255 | | The system identifier of the listing the offer is made against. |
| OfferNotes | String | Yes | | | Notes that apply to the offer as a whole rather than to one submission. |
| OfferOriginatingSystemId | String | Yes | 255 | | The originating system identifier of the listing being offered on, as carried in current practice. |
| OfferOriginatingSystemName | String | Yes | 255 | | The name of the system with authoritative control over the listing being offered on. |
| OfferSourceSystemId | String | Yes | 255 | | The source system identifier of the listing record, which may differ from the originating system. |
| OfferSourceSystemName | String | Yes | 255 | | The name of the system the listing record was directly received from. |
| OfferUoi | String | Yes | 25 | | The Unique Organization Identifier of the organization the listing being offered on originated with. This is the provenance of the listing, not the identity of a participant. |
| OfferUsi | String | Yes | 25 | | The Unique System Identifier of the system, within that organization, the listing being offered on was input in. |
| ModificationTimestamp | Timestamp | No | | | The date and time the offer record was last modified. |

An offer that identifies no listing cannot be routed to a listing agent, so an `Offer` MUST identify one. A listing identifier alone is not sufficient to do that unambiguously.

`ListingId` is human-friendly, often short and often numeric, and two unrelated organizations can issue the same value. `ListingKey` has the same exposure, since a system numbering its listings from one collides with every other system that does. Neither is globally unique on its own.

An `Offer` therefore carries a **coordinate** rather than a single identifier: a listing identifier, plus the organization or system that issued it. An `Offer` MUST carry `ListingId` or `ListingKey`, and MUST carry at least one of `OfferUoi`, `OfferOriginatingSystemName` or `OfferOriginatingSystemId`. More of them narrow the coordinate further, and `OfferUsi` narrows it to the system a listing was input on rather than to the organization alone.

The three are permitted but not equivalent. `OfferUoi` is an identifier and the other two are not: a name varies in spelling, changes when an organization rebrands or merges, and nothing obliges two parties to write it the same way. A coordinate qualified only by an originating system name prevents most accidental collisions and guarantees nothing, so it is the weakest form this specification allows. Carry `OfferUoi` wherever it can be determined.

**This narrows at Data Dictionary 3.0.** The expectation is that an `Offer` will then MUST carry `OfferUoi`, and MAY carry `OfferOriginatingSystemName` and `OfferOriginatingSystemId` alongside it. The legacy pair is expected to be deprecated at that point in the sense RESO versioning gives the term: removed from the specification while providers may continue to use them. It is not a forced deprecation, which would be a major change on its own terms.

This is said here as a warning rather than as a footnote. An implementer starting now should build on `OfferUoi` and treat the originating system name and identifier as values it accepts from others rather than as the foundation of its own records. Building on the legacy pair means building on something expected to be deprecated.

`OfferUoi` and `OfferOriginatingSystemId` overlap deliberately. The Data Dictionary defines an originating system identifier as holding an organization identifier, so the element is named for a system and carries an organization, and it cannot express the system within that organization at all. `OfferUoi` names the organization explicitly and `OfferUsi` adds the system, which the legacy pair never distinguished.

RESO is transitioning to Unique Organization Identifiers through [Organization and System Identifiers (RCP-55)](https://github.com/RESOStandards/transport/pull/243). The expected path is that the Data Dictionary carries them in a minor version, and that Data Dictionary 3.0 requires a Unique Organization Identifier in certification where a provider would otherwise supply only an originating system name or identifier.

A provider therefore needs both for a time: the organization identifier going forward, and the originating system name or identifier while that is what a counterparty has. `OfferOriginatingSystemName` and `OfferOriginatingSystemId` are added to this proposal in full knowledge that they are intended to be deprecated, because a listing today commonly carries nothing else.

An implementation new to offer exchange SHOULD support `OfferUoi` and `OfferUsi` from the outset. There is no legacy to preserve in a system that has not exchanged offers before, and starting on the identifier the standard is moving to avoids a migration later. A provider whose only available value today is an originating system name or identifier remains conformant and SHOULD continue to populate it. Where RCP-55 ratifies first, these fields adopt its definitions rather than restating them.

A goal of this specification is that every Offer Hub and participating organization holds a Unique Organization Identifier, so that each can issue its own local identifiers freely and without collision.

Most participants already hold one. The RESO organization registry carries 1,977 organizations as of September 2026, each with one. They are mostly MLSs and associations, but technology companies, brokerages, commercial organizations and pooled platforms are all represented, which covers the population likely to host offers. RESO issues an identifier to an organization that does not yet hold one.

This is what makes the coordinate work in practice rather than in principle. A locally issued identifier becomes collision-free once it is qualified by a centrally issued organization identifier, because no two organizations share one. A participant therefore keeps numbering its own records however it already does, and does not need to be an MLS to take part. The organization member of a coordinate resolves against the registry.

The coordinate members describe the listing, so they carry whatever the originating system published. A participant is required to hold an organization identifier ([Section 2.1](#section-21-participation-and-confidentiality)); a listing is not, because a listing is not a party to anything and the system that published it may predate the transition entirely.

Holding an organization identifier does not mean every counterparty supplies one. A participant will receive coordinates from organizations that have not populated theirs, and an implementation MUST accept a coordinate whose organization member is an originating system name or identifier. It MUST NOT reject an offer on that ground.

Where a participant can determine the organization identifier for a listing it received qualified only by a name, for example by resolving the name against the registry, it SHOULD record the identifier alongside what it was given. That is what makes the narrowing at Data Dictionary 3.0 reachable rather than abrupt.

Note that holding an identifier and publishing one are different things. Most organizations are in the registry, while a smaller share carry an organization identifier in the data they publish, so the name is often the only value a counterparty has to hand.

A crosswalk in which organizations declare the local names and identifiers they publish, and what each resolves to, is under consideration and is not specified here ([Section 6, Open Questions](#open-questions)).

The members of a coordinate MAY be hashed together to produce a single opaque value, where a provider does not wish to publish the parts. A party already holding the parts can verify such a value; a party that does not, cannot read them out of it.

## Section 2.5: The OfferSubmission Resource

An `OfferSubmission` is one turn in the negotiation: an initial offer, a counter or a re-counter. Every `OfferSubmission` MUST correlate to an `Offer`, by carrying that offer's `OfferId`, and an implementation MUST NOT accept a submission that correlates to no offer. `OfferId` is therefore required on both, and submissions are append-only. An implementation MUST NOT modify a submission to represent a counter; it MUST create a new one ([Section 2.9](#section-29-counter-offers)).

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
| OfferSubmissionSequence | Number | No | | | The position of this submission in its offer, assigned as described in [Section 2.9](#section-29-counter-offers). |
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

Both live on `OfferSubmission`, and neither lives on `Offer`. A state is a fact about a turn in the negotiation, so it is recorded on the turn it describes.

### Where the Current State Is

The current state of an offer is the pair of statuses carried by its highest-sequence submission ([Section 2.9](#section-29-counter-offers)). A consumer derives it by reading that submission. An implementation MUST NOT require a consumer to look anywhere else for it, and MUST NOT publish a separate current-state record that could disagree with the submissions.

Each is single-valued, so an offer holds one status at a time on each side. The history of an offer is the sequence of its submissions, not a list of statuses on one record.

### How a State Moves

Two kinds of act change an offer, and they are recorded differently.

An act that **changes the terms** is a new turn. The party MUST create a new `OfferSubmission` carrying the new terms and the status that act produces, and MUST NOT modify an existing one. Submitting, countering and re-countering are acts of this kind.

An act that **changes no terms** is not a new turn. The party MUST record it by setting its own status field on the current submission, and MUST NOT create a submission for it. Acknowledgement, acceptance, rejection, withdrawal and expiry are acts of this kind.

A submission is therefore writable for its status only while it is the current one. Once a later submission supersedes it, its statuses are the record of what the parties held at that point and MUST NOT be changed ([Section 2.9](#section-29-counter-offers)).

Each side writes only its own field. The submitting side sets `OfferSubmissionStatus` and the receiving side sets `OfferReceivedStatus`. An implementation MUST NOT set the other side's field on that side's behalf, and MUST NOT treat a disagreement between the two as an error: the parties hold separate systems and act at different times, so the pair is expected to differ while an act is in flight. What each side holds is what that side observed.

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

### The Thread

**Publishing a listing is what makes it eligible for offers.** A listing is published to the network by its point of entry, usually the MLS, and that activity is the root of the thread. Until it exists there is nothing to reply to and no offer can be made. What opens a listing to offers is that publishing act rather than the listing's marketing phase, so a premarketed listing published for offers is eligible and a marketed listing never published to the hub is not ([Section 2.2](#section-22-activitypub-usage)). An offer is posted `inReplyTo` the root, by its identifier. A counter is posted `inReplyTo` the offer it answers, and a re-counter `inReplyTo` the counter. Every turn names its parent, so the negotiation is a single-rooted tree and its shape is recoverable from the thread alone.

An implementation MUST NOT accept an offer that references no published listing activity, and an `Offer` MUST correspond to a listing that was published to the network.

The root's host does not order what follows. An offer is addressed to the listing side, not to the system that published the listing, so that system does not see the offers made on it and cannot number them. Ordering is settled between the parties to each offer.

### Ordering

ActivityPub does not guarantee delivery order, so arrival order is not evidence of sequence. Each `OfferSubmission` therefore carries `OfferSubmissionSequence`, typed as the positive portion of int64, durable, immutable and monotonic.

This is the same shape as `EntityEventSequence` in [EntityEvent](https://github.com/RESOStandards/transport/blob/main/proposals/entity-events.md), for the same reason, so an implementation that already maintains a logical clock can use the same machinery. The resemblance is a convenience and not a dependency: nothing here requires EntityEvent.

The two differ in one respect, because the situations differ. `EntityEventSequence` orders the events of a single system, so a plain counter suffices and its producer is the only writer. An offer is written by two parties who may act without having seen each other, so assignment has to tolerate that.

The two compose for an implementation that wants them to. A system emitting EntityEvent records for its offer records lets a consumer track back to an `Offer` or an `OfferSubmission` through the event stream, by `ResourceName` and `ResourceRecordKey`, and replay from a known `EntityEventSequence`. For that to be possible at all, `ResourceName` has to be able to name these resources, which is the one place this proposal touches EntityEvent and the reason it adds those values to that lookup ([Proposed Data Dictionary elements](#proposed-data-dictionary-elements)).

An implementation creating a submission MUST set `OfferSubmissionSequence` to one greater than the highest value it has seen for that `OfferId`, and MUST set it to 1 for the first submission of an offer. An implementation MUST NOT renumber a submission after creating it.

Two submissions in one offer MAY carry the same sequence. That is not a defect to repair: it means both parties acted without having seen the other, which is a real event and the reason the number is worth carrying. Where it happens, an implementation MUST order the two by the Unique Organization Identifier of the submitting party, ascending, so that every party reaches the same ordering from the same facts. It MUST NOT resolve the tie by arrival time, which differs per recipient.

A consumer MUST order the submissions of an offer by `OfferSubmissionSequence`, and MUST NOT rely on `OfferSubmissionTimestamp` for ordering. Timestamps come from different clocks and a counter may legitimately carry an earlier one than the submission it answers.

Where a single system hosts a whole negotiation, this rule produces a plain counter: 1, 2, 3. The tie-break never fires, and nothing is lost by following the general rule.

## Section 2.10: Web API Conformance

A system that hosts offer threads on behalf of participants is an **Offer Hub**. The term is used throughout this specification for that role, whether the host is an offer management product, a brokerage or an MLS.

An implementation MAY expose these resources over OData on the RESO Web API without participating in any ActivityPub exchange. Such an implementation MUST satisfy the following, which are the parts of this specification that do not depend on the transport:

* The resources and fields of [Section 2.4](#section-24-the-offer-resource) through [Section 2.6](#section-26-the-offerpropertygroup-resource), with the types and nullability given.
* The lookups and standard values of [Section 2.7](#section-27-offer-states), single-valued on each side.
* The append-only rule for submissions ([Section 2.9](#section-29-counter-offers)).
* The rule that the current state of an offer is the pair of statuses on its highest-sequence submission ([Section 2.7](#section-27-offer-states)).
* The identity rules of [Section 2.3](#section-23-offer-identity) as they apply to `OfferKey` and `OfferId`.
* The authorization rules of [Section 2.11](#section-211-authentication-and-authorization).

Where an implementation supports [Web API Add/Edit](https://github.com/RESOStandards/transport/blob/main/proposals/web-api-add-edit.md), creating a counter offer is a create against `OfferSubmission`, never an update of an existing submission.

## Section 2.11: Authentication and Authorization

### Two boundaries

Access is decided at two points, and they are independent.

**Network admission** governs who can reach a thread at all. Where offers are addressed to named parties ([Section 2.1](#section-21-participation-and-confidentiality)), the network is closed and an implementation MAY admit participants through single sign-on using OpenID Connect. That admission MAY be federated, so that an identity issued by one participant is accepted across the network. Where offers are advertised publicly there is no admission step, because the thread is open by construction.

**Payload authorization** governs who can read a particular offer. It applies identically under both addressing models, because the payload is protected either way.

**Authentication federates. Authorization does not.** Accepting an identity another participant issued is a statement about who the requester is, and a network can agree to trust that in common. Whether that requester may read a particular offer is a statement about one offer, held by the implementation that holds it, and no other participant is in a position to make it. An implementation MUST NOT delegate the payload decision to the issuer of a requester's identity, and MUST NOT accept an assertion of entitlement from another participant in place of its own determination.

The token is core to both. Single sign-on establishes and federates identity; it does not authorize a request. Every payload request carries a bearer token regardless of how the requester was admitted, and that token is what an implementation resolves and checks against its own record of who the parties are.

Admission to the network MUST NOT be treated as entitlement to an offer. A participant who is on the network is on the network; it is a party to the offers it is a party to, and to no others. An implementation MUST make the payload decision on its own terms, for every request, regardless of how the requester reached the thread.

### Authenticating

A payload link MUST be protected and MUST refuse an unauthenticated dereference. An implementation MUST NOT publish an offer payload at an unauthenticated URL.

A client presents a bearer token:

```
Authorization: Bearer <token>
```

It obtains that token one of two ways, which are the same two RESO certification already uses:

* **A token supplied directly.** The client is configured with the payload endpoint and a bearer token.
* **A client credentials grant.** The client is configured with the payload endpoint, a client identifier, a client secret, a token endpoint and optionally a scope, and exchanges them for a bearer token at the token endpoint.

An implementation MUST accept a bearer token presented this way. It MAY additionally accept other mechanisms, and MUST NOT require one in place of this.

### Authorizing

Authentication says which client is asking. Authorization says whether that client may read this particular offer, and the two are decided separately.

A requester resolves to a Unique Organization Identifier or a Unique System Identifier, by the same rule that governs participants in [Section 2.1](#section-21-participation-and-confidentiality). An implementation MUST determine that identifier from the presented token, and MUST NOT infer it from any value carried in the request itself.

The parties to an offer are the submitting agent and their brokerage, the listing agent and their brokerage, and the systems acting on behalf of any of them. An implementation MUST serve an offer payload only to a requester whose resolved identifier is a party to that offer, and MUST NOT treat involvement in one offer as involvement in another.

### Refusing

A refusal distinguishes the two decisions, so that a caller can tell a credential problem from an entitlement one:

| Condition | Response |
| :--- | :--- |
| No token, or a token that does not authenticate | `401 Unauthorized` |
| Authenticated, but not a party to this offer | `403 Forbidden` |
| Authenticated and a party, but no such offer | `404 Not Found` |

An implementation MUST NOT answer `404` where the offer exists and the requester is simply not a party to it, and MUST NOT answer `403` in a way that confirms an offer exists to a requester with no entitlement to know. Where the distinction itself would disclose something, `404` is the safer answer and is permitted.

### Withholding fields

The buyer and co-buyer fields of [Section 2.5](#section-25-the-offersubmission-resource) are the most sensitive elements this specification defines. An implementation MAY omit them from a payload served to a party that does not require them, and a consumer MUST NOT treat their absence as an error.

Omission is not the same as refusal. A payload served with those fields withheld is a successful response, and the requester is a party to the offer; it has simply been given the subset it needs.

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
  "OfferSubmissionSequence": 1,
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
  "OfferSubmissionSequence": 2,
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

The request is addressed to each offering party separately, one activity per party, so that no offering party learns from it who else is bidding. An implementation MUST NOT include the terms of one offering party's offer in an activity addressed to another ([Section 2.11](#section-211-authentication-and-authorization)).

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

The `object` is the submission being accepted, which in a negotiation that has countered is the most recent counter rather than the original offer. That submission is the current one, so acceptance changes no terms and creates nothing: the accepting side records `OfferAcceptedTimestamp` and sets `OfferReceivedStatus` to `Accepted` on it. The submitting side, on receiving the `Accept`, sets `OfferSubmissionStatus` to `Accepted` on its own copy. Each side writes its own field and the two then agree ([Section 2.7](#section-27-offer-states)).

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

### Section 2.12.8: Public Addressing

An implementation advertising offers publicly addresses the activity to the public collection and keeps every element of the offer behind the payload link. The activity below is conformant and discloses nothing about the offer itself.

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Offer",
  "id": "https://my.offercloud.example/offer/QRS421",
  "published": "2026-09-15T14:02:00Z",
  "actor": "https://listing.example/AmyAgent",
  "inReplyTo": "https://listing.example/BobAgent/133",
  "to": ["https://www.w3.org/ns/activitystreams#Public"],
  "url": {
    "type": "Link",
    "href": "https://my.offercloud.example/payload/QRS421",
    "mediaType": "application/json"
  }
}
```

A reader of the thread learns that an offer was made on listing 133 and nothing else. The price, the buyer, the financing and the status are in the payload at `url`, which refuses an unauthenticated dereference exactly as it does under private addressing ([Section 2.11](#section-211-authentication-and-authorization)).

The obligation is the same under both models, because the payload is OAuth2 protected in either case. The only difference is whether a token is also required to reach the thread. A provider that additionally chooses to serve its payload without authentication has left this specification, which requires the link to refuse an unauthenticated dereference.

What a public activity does disclose is that this actor made an offer on this listing at this time. In a competitive situation that is itself information, and it is the trade an implementation weighs when choosing between the two.

<br /><br />

# Section 3: Certification

Certification proves that an offer keeps its shape, its history and its confidentiality as it crosses systems. The rules derive from the Section 2 requirements. Certification is per-interface: an implementation may be certified for the Web API interface, the ActivityPub interface or both.

RESO will validate the following during certification:

**Model**
* The candidate MUST serve the resources, fields, types and nullability of [Section 2.4](#section-24-the-offer-resource) through [Section 2.6](#section-26-the-offerpropertygroup-resource), and a payload it produces MUST validate as RESO Common Format against the declared Data Dictionary version.
* The candidate MUST accept and serve the standard values of [Section 2.7](#section-27-offer-states) and MUST reject a multi-valued status on either side.
* The candidate MUST use the existing standard values for `BuyerFinancing`, `Contingencies` and `BuyerBrokerageCompensation` and MUST NOT substitute offer-specific equivalents ([Section 2.5](#section-25-the-offersubmission-resource)).
* The candidate MUST NOT accept an offer that references no published listing activity ([Section 2.9](#section-29-counter-offers)).
* Every `OfferSubmission` the candidate accepts MUST correlate to an `Offer` it holds, by `OfferId`. A candidate that accepts a submission correlating to no offer fails ([Section 2.5](#section-25-the-offersubmission-resource)).
* The candidate MUST accept a second `Offer` from the same buyer on the same listing, both where an earlier offer has ended and where both are live. A candidate that rejects it as a duplicate, or that merges it into the earlier offer, fails ([Section 2.4](#section-24-the-offer-resource)).
* The candidate MUST NOT append a submission to an offer that has been withdrawn, rejected or has expired ([Section 2.4](#section-24-the-offer-resource)).
* An `Offer` the candidate accepts MUST carry `ListingId` or `ListingKey`, and MUST carry at least one of `OfferUoi`, `OfferOriginatingSystemName` or `OfferOriginatingSystemId`. A candidate that accepts a listing identifier with no organization or system member fails ([Section 2.4](#section-24-the-offer-resource)).
* An `OfferPropertyGroup` the candidate accepts MUST identify the property by one of the two permitted combinations ([Section 2.6](#section-26-the-offerpropertygroup-resource)).
* Where the candidate publishes a hashed coordinate, it MUST be reproducible: the same listing MUST yield the same value on repeated construction ([Section 2.4](#section-24-the-offer-resource)).

**History**
* On a counter, the candidate MUST create a new `OfferSubmission`, and every submission that was already superseded MUST remain byte-identical to what it held when the counter was created ([Section 2.9](#section-29-counter-offers)). A candidate that modifies a superseded submission fails.
* The candidate MUST record an act that changes no terms by setting its own status field on the current submission, and MUST NOT create a submission for it. A candidate that emits a submission for an acknowledgement, acceptance, rejection or withdrawal fails ([Section 2.7](#section-27-offer-states)).
* The current state the candidate reports for an offer MUST equal the pair of statuses on its highest-sequence submission, at every point in the exchange ([Section 2.7](#section-27-offer-states)).
* The candidate MUST NOT set the counterparty's status field, and MUST NOT reject an offer whose two status fields disagree ([Section 2.7](#section-27-offer-states)).
* Every submission the candidate creates MUST carry `OfferSubmissionSequence` set to one greater than the highest it has seen for that `OfferId`, or to 1 for the first, and MUST keep that value unchanged thereafter ([Section 2.9](#section-29-counter-offers)).
* Where two submissions of one offer carry the same sequence, the candidate MUST order them by the submitting party's Unique Organization Identifier ascending, and MUST NOT order them by arrival time ([Section 2.9](#section-29-counter-offers)).
* Every submission created during the test MUST remain retrievable under its `OfferId` after later submissions are created, and MUST be returned in `OfferSubmissionSequence` order ([Section 2.9](#section-29-counter-offers)).

**Protocol**
* Every activity the candidate posts MUST use only Activity Streams 2.0 vocabulary and MUST NOT carry custom JSON-LD terms ([Section 2.2](#section-22-activitypub-usage)).
* An activity the candidate posts MUST NOT carry offer content; that content MUST be reachable only through the protected link the activity references ([Section 2.2](#section-22-activitypub-usage)).
* The candidate MUST use the mapped Activity Streams type for every state that has one, and MUST use a `Note` rather than an invented type for every state that does not ([Section 2.8](#section-28-activity-streams-mapping)).
* The candidate MUST NOT address an activity carrying offer content to the public collection, and an activity it addresses to the public collection MUST carry no element of [Section 2.4](#section-24-the-offer-resource) through [Section 2.7](#section-27-offer-states) ([Section 2.1](#section-21-participation-and-confidentiality)).
* The candidate MUST NOT fail an inbound activity on the ground that it was addressed to the public collection, where that activity carries no offer content ([Section 2.1](#section-21-participation-and-confidentiality)).
* The candidate MUST NOT fail when a counterparty publishes no state in the thread, and MUST resolve the state from the payload instead ([Section 2.8](#section-28-activity-streams-mapping)).
* The candidate MUST NOT parse an activity identifier to recover offer data, and MUST NOT require an identifier to contain an `OfferId` ([Section 2.3](#section-23-offer-identity)).
* The candidate MUST NOT post an `Undo` for an activity another party posted ([Section 2.12.7](#section-2127-withdrawing)).

**Participation**
* The actor the candidate posts under MUST resolve to a Unique Organization Identifier or a Unique System Identifier: its own, that of the organization it acts for, or that of the system making the requests ([Section 2.1](#section-21-participation-and-confidentiality)).
* The candidate MUST accept a listing coordinate whose organization member is an originating system name or identifier, and MUST NOT reject an offer on the ground that no organization identifier was supplied for the listing ([Section 2.4](#section-24-the-offer-resource)).

**Confidentiality**
* Every payload link the candidate publishes MUST refuse an unauthenticated dereference ([Section 2.11](#section-211-authentication-and-authorization)).
* The candidate MUST determine a requester's identifier from the presented token and MUST NOT infer it from a value carried in the request ([Section 2.11](#section-211-authentication-and-authorization)).
* The candidate MUST NOT accept another participant's assertion that a requester is entitled to an offer in place of its own determination ([Section 2.11](#section-211-authentication-and-authorization)).
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
* [RESO EntityEvent](https://github.com/RESOStandards/transport/blob/main/proposals/entity-events.md)
* [RESO Universal Property Identifier](https://upi.reso.org/)
* [RESO Organizations registry](https://services.reso.org/orgs)
* [RESO Versioning](https://github.com/RESOStandards/transport/blob/main/versioning.md)
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

This proposal also adds three values to the existing `ResourceName` lookup, whose standard values are currently `Association`, `Contacts`, `Member`, `Office` and `Property`. Without them no existing resource can name an offer record: `Media` cannot attach a contract document to a submission, and `EntityEvent` cannot carry a change to one.

| Lookup | Lookup Value | Definition |
| :--- | :--- | :--- |
| ResourceName | Offer | The Offer resource. |
| ResourceName | OfferSubmission | The OfferSubmission resource. |
| ResourceName | OfferPropertyGroup | The OfferPropertyGroup resource. |

The following existing elements are reused without change: `BuyerFinancing`, `Contingency`, `BuyerBrokerageCompensation`, `StreetNumber`, `StreetName`, `City`, `StateOrProvince`, `PostalCode`, `CountyOrParish`, `Country`, `ParcelNumber`, `UniversalPropertyId`, `ListingId`, `ListingKey` and the `Media` resource.

## Open Questions

These are recorded rather than settled, and are for the workgroups.

**Should anything about an offer be public beyond its existence?** [Section 2.1](#section-21-participation-and-confidentiality) supports both addressing models and permits a public activity that carries no offer information, so the fact of an offer may already be published by a provider that wants to. What is not settled is whether the industry wants more than the bare fact, for example a count of offers received on a listing, or a status visible without authentication. That was raised in the ActivityPub Subgroup in August 2025 and has not been decided. There is precedent for wanting it: buyers are commonly notified how many competing offers exist, and a status of this kind has been requested of the Data Dictionary before.

**Should a universal property identifier be required?** [Section 2.6](#section-26-the-offerpropertygroup-resource) makes it optional. Requiring it would strengthen cross-system matching and would exclude providers who cannot compose one.

**Which offer elements should the endorsement require?** Several elements this proposal reuses are sparsely populated in the industry data, but that data is drawn almost entirely from MLSs and offers are not MLS domain. An offer management provider implementing this endorsement would populate them. Requiring a subset is therefore viable and is a question for the workgroup rather than an inference from current adoption.

**Which status values belong on which side?** [Section 2.7](#section-27-offer-states) gives the submitting side thirteen standard values and the receiving side twelve, on the principle that the two sides observe different events and their value sets may diverge. That principle is sound and the division has not been reviewed value by value. `ScheduledToPresent` is the clearest case. It appears only on the submitting side, yet presenting an offer to the seller is done by the listing side, which is the receiving side. `Delivered` and `Received` raise the same question from the other direction, since each is naturally observed by one side and asserted about the other. Whether each value belongs on one side, the other or both is a question for the workgroup.

**Should RESO maintain a crosswalk from local originating system values to organization and system identifiers?** A participant will regularly receive an originating system name or identifier where no organization identifier was published. Rather than inferring a mapping, RESO could carry a registry in which each organization declares the local names and identifiers it publishes and the organization or system identifier each one resolves to. That is authoritative rather than inexact, it handles an organization that publishes several spellings of its own name, and it can resolve to a system identifier where an organization runs more than one. Much of the mapping is likely already held in certification records, which capture the values a provider publishes against the organization being certified, so the exercise may be closer to verifying and exposing what RESO knows than to collecting it. That needs confirming. Whether RESO should offer this, and who maintains the entries once it exists, is open. [Section 2.4](#section-24-the-offer-resource) requires only that a name be accepted.

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

This proposal deprecates no existing element. It introduces two whose disposition is already expected: `OfferOriginatingSystemName` and `OfferOriginatingSystemId` carry what a listing commonly holds today and are intended for deprecation at Data Dictionary 3.0, in the sense RESO versioning gives the term, where a deprecated element is removed from the specification and providers may continue to use it ([Section 2.4](#section-24-the-offer-resource)).

| Resource | Field | Expected disposition | Note |
| :--- | :--- | :--- | :--- |
| Offer | OfferOriginatingSystemName | Deprecated at Data Dictionary 3.0 | Providers may continue to use it. Added because a name is often all a listing carries today. |
| Offer | OfferOriginatingSystemId | Deprecated at Data Dictionary 3.0 | As above. |

## Public Advertising, Side by Side

[Section 2.1](#section-21-participation-and-confidentiality) supports offers addressed to named parties and offers advertised publicly. This appendix shows what actually differs between the two, because the answer is narrower than it first appears.

The same submission, addressed to named parties:

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Offer",
  "id": "https://my.offercloud.example/offer/XYZ999",
  "actor": "https://listing.example/AmyAgent",
  "inReplyTo": "https://listing.example/BobAgent/133",
  "to": ["https://listing.example/BobAgent"],
  "url": { "type": "Link", "href": "https://my.offercloud.example/payload/XYZ999", "mediaType": "application/json" }
}
```

And advertised publicly:

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Offer",
  "id": "https://my.offercloud.example/offer/XYZ999",
  "actor": "https://listing.example/AmyAgent",
  "inReplyTo": "https://listing.example/BobAgent/133",
  "to": ["https://www.w3.org/ns/activitystreams#Public"],
  "url": { "type": "Link", "href": "https://my.offercloud.example/payload/XYZ999", "mediaType": "application/json" }
}
```

One member differs. Everything else about the activity, and everything about the payload behind `url`, is identical.

### What changes

| | Addressed to named parties | Advertised publicly |
| :--- | :--- | :--- |
| Who can see the activity | The named parties | Anyone who can reach the server |
| Who learns an offer exists on the listing | The named parties | Anyone |
| Who learns which actor made it, and when | The named parties | Anyone |
| Reaching the thread | Requires authorization to the network | No authorization required |
| Discoverable by a crawler or aggregator | No | Yes |

### What does not change

| | Both models |
| :--- | :--- |
| The payload | Identical |
| Reaching the payload | OAuth2, refuses an unauthenticated dereference |
| Price, buyer, financing, contingencies, dates, status | Behind the payload link |
| The listing coordinate and participant identity rules | [Section 2.3](#section-23-offer-identity), [Section 2.4](#section-24-the-offer-resource) |
| Offer states and their Activity Streams mapping | [Section 2.7](#section-27-offer-states), [Section 2.8](#section-28-activity-streams-mapping) |
| Append-only submissions | [Section 2.9](#section-29-counter-offers) |
| Every certification check on offer content | [Section 3](#section-3-certification) |

### The consequence worth weighing

Public advertising discloses the **metadata of the negotiation** while protecting its **contents**. An observer learns that this actor made an offer on this listing at this time, that a counter followed an hour later, and that the thread went quiet after a third activity. The terms stay behind the token throughout.

For some participants that is the point: an open record that a listing is receiving activity, without exposing what anyone bid. For others the timing and the identities are themselves competitive information. Neither reading is wrong, which is why the specification carries both and neither is nominated as correct.

An implementation MAY use both models across different listings, or change between them, provided the rule in [Section 2.1](#section-21-participation-and-confidentiality) holds in each case: an activity carrying offer content is never addressed to the public collection.

<br />

## Design rationale

**Why ActivityPub rather than the Web API or an event feed.** An offer is a conversation with turns from both sides, and the transport has to carry that shape.

The Web API is request and response against a provider's own data. It serves a record well and it does not carry a negotiation between two providers, each of whom holds part of it and neither of whom is the other's server.

EntityEvent is one-directional by design, a stream of things that have happened. It can log a negotiation after the fact and it cannot conduct one, because there is no reply.

ActivityPub is bidirectional and threaded, which is what an offer actually is: a turn, an answer, another turn, each naming what it responds to. It also already carries listings under the [Listing Advertisement proposal](https://github.com/RESOStandards/transport/discussions/162), so an offer replies into a thread that exists rather than opening a parallel channel alongside it.

That division is why this specification requires nothing of an event feed. The negotiation must be complete from the thread and its payloads alone.

The two are complementary, in both directions. A provider already running an event stream MAY emit realtime notifications of offer events into it, or let a consumer subscribe to them there, and `HistoryTransactional` carries the field-level detail behind each record. That is a useful thing to have alongside the ActivityPub nodes and it is not a substitute for them, because an event feed cannot carry a reply. Equally, the nodes are not a substitute for the feed: an event feed serves a purpose this specification does not attempt, which is telling a subscriber quickly that something changed. Neither requires the other, and a provider MAY run either alone or both together.

**Why the data is not in the activity.** Putting offer terms in an ActivityPub object publishes them to every server the activity federates to, and federation is not revocable. An offer is confidential, so the activity carries a reference and the data stays behind an authenticated link the originator controls. This also keeps the vocabulary standard, since nothing offer-specific has to be expressed in JSON-LD.

**Why the identifier need not be meaningful.** A provider that must expose `OfferId` in an activity identifier discloses, to anyone who can see the thread, how many offers it has issued and in what order. Allowing an opaque identifier removes that disclosure without weakening the reference, because the payload behind the link resolves the record.

**Why submissions are append-only.** A negotiation is evidence. If a counter overwrites the offer it answers, the record of what was offered, when and by whom is lost, and the parties have no common account of what happened. Append-only keeps the sequence, and the sequence is what an offer is.

No party can verify that another party kept its own log intact, and this specification does not pretend otherwise. Append-only is a contract between the parties, resting on the same trust every RESO payload rests on. What certification can establish is that a candidate honors it in a controlled exchange, which is what [Section 3](#section-3-certification) tests. What a consumer can establish is that the submissions it holds are internally consistent. Neither is a guarantee about a counterparty, and an implementation SHOULD be built on the assumption that it is reading a record it did not write.

**Why two status lookups.** The submitting side and the receiving side observe different events. `Delivered` is knowable by the sender's system before the recipient has done anything, and `Received` is the recipient's statement. Collapsing them into one field would force one side to assert what the other side knows. Their values are close today and may diverge.

**Why an absent state is not an error.** Some systems will not publish offer states to a thread, for business reasons that are theirs to weigh. A specification that failed on absence would exclude those systems or push them to publish what they would rather not. Treating absence as a signal to dereference keeps them in the exchange and keeps the data where its owner wants it.

<br /><br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO Transport](mailto:transport@reso.org) with questions about this proposal, or [RESO developer support](mailto:dev@reso.org) with specific development questions.
