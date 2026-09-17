# RESO ULI Resolution Protocol Endorsement

| **RCP** | 54 |
| :--- | :--- |
| **Version** | **0.1.0** |
| **Authors** | [Joshua Darnell (RESO)](mailto:josh@reso.org)<br />RESO ULI Workgroup |
| **Status** | IN PROGRESS |
| **Date Ratified** | TBD |
| **Dependencies** | [Data Dictionary 2.0+](https://dd.reso.org/DD2.0/Member/) (Member Resource)<br />[RESO Common Format 1.7+](https://transport.reso.org/proposals/reso-common-format/) (RCP-25)<br />[Web API Core 2.1.0](https://transport.reso.org/proposals/web-api-core/), Section 2.9 (OAuth2 authorization)<br />[W3C ActivityPub](https://www.w3.org/TR/activitypub/)<br />[W3C Activity Streams 2.0](https://www.w3.org/TR/activitystreams-core/) and [Activity Vocabulary](https://www.w3.org/TR/activitystreams-vocabulary/) |
| **Related Links** | [ULI Resolution Protocol discussion](https://github.com/RESOStandards/transport/discussions/163)<br />[ActivityPub examples (Listing Advertisement discussion)](https://github.com/RESOStandards/transport/discussions/162)<br />[RCP-54 tracking issue](https://github.com/RESOStandards/transport/issues/221)<br />[RCP-55 Organization and System Identifiers](https://github.com/RESOStandards/transport/blob/46619fdd2d383938ef86c6efff0f01e82c19d6c0/proposals/org-system-identifiers.md)<br />RESO ActivityPub Subgroup, 2025-08-01 agenda |

<br />

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

The keywords "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY" and "OPTIONAL" in this document are to be interpreted as described in [RFC 2119](https://www.ietf.org/rfc/rfc2119.txt).

<br />

# Table of Contents
- [Summary of Changes](#summary-of-changes)
- [Introduction](#introduction)
- [Section 1: Purpose](#section-1-purpose)
- [Section 2: Specification](#section-2-specification)
  - [Section 2.1: Participation and Data Isolation](#section-21-participation-and-data-isolation)
  - [Section 2.2: ActivityPub Usage](#section-22-activitypub-usage)
  - [Section 2.3: ULI Fields and Payload](#section-23-uli-fields-and-payload)
  - [Section 2.4: Broadcast](#section-24-broadcast)
  - [Section 2.5: Resolution and Reply](#section-25-resolution-and-reply)
  - [Section 2.6: Consensus](#section-26-consensus)
  - [Section 2.7: ULI Creation and Lifecycle](#section-27-uli-creation-and-lifecycle)
  - [Section 2.8: Signing and Self-Verification](#section-28-signing-and-self-verification)
  - [Section 2.9: Matching Engine and Scoring](#section-29-matching-engine-and-scoring)
  - [Section 2.10: Resolver Discovery and the Governance Feed](#section-210-resolver-discovery-and-the-governance-feed)
  - [Section 2.11: Authentication and Authorization](#section-211-authentication-and-authorization)
  - [Section 2.12: Worked Examples](#section-212-worked-examples)
- [Section 3: Certification](#section-3-certification)
  - [Section 3.1: Onboarding Gate](#section-31-onboarding-gate)
  - [Section 3.2: Continuous Conformance](#section-32-continuous-conformance)
  - [Section 3.3: Reference Correctness](#section-33-reference-correctness)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: Appendices](#section-6-appendices)
- [Section 7: License](#section-7-license)

<br />

# Summary of Changes
* Establishes the **ULI Resolution Protocol**: a threaded, event-based resolution of the RESO **Unique Licensee Identifier (ULI)** over W3C ActivityPub, across a network of certified resolvers, with no licensee data stored on or shared through the network.
* Participation with prearranged authorization and no central store ([Section 2.1](#section-21-participation-and-data-isolation)); an identifiability threshold before broadcast ([Section 2.4](#section-24-broadcast)); consensus at `floor(2/3 × R)` with optional mandatory resolvers ([Section 2.6](#section-26-consensus)).
* A ULI lifecycle built from a small set of primitives – consensus-gated mint, silent local adopt, holder-initiated tombstone and redirect, merge and decline by rebroadcast – with self-healing correction ([Section 2.7](#section-27-uli-creation-and-lifecycle), [Section 2.12](#section-212-worked-examples)).
* **Self-verifying identifiers**: each ULI carries an Ed25519 signature by its minting organization, so it can be checked as network-minted offline, with no registry ([Section 2.8](#section-28-signing-and-self-verification)).
* **Credit-score-style weighted matching** with a network-adopted weight set and threshold ([Section 2.9](#section-29-matching-engine-and-scoring)).
* **Certification**: a sandbox onboarding gate (exhaustive scoring and search parity against the reference oracle) plus continuous conformance monitoring ([Section 3](#section-3-certification)).
* **Governance feed**: a public ActivityPub feed, free of personally identifiable information (PII), that every resolver follows, carrying membership and parameter events; `R` is the view of it that is certified, compliant and active ([Section 2.10](#section-210-resolver-discovery-and-the-governance-feed)).
* **Data Dictionary impact.** This is a new endorsement and deprecates nothing. The identifier it resolves is carried in the existing `UniqueLicenseeIdentifier` field of the Member Resource (Data Dictionary 2.0 and later); this proposal asks for that field's definition to be broadened, and adds no new field, lookup or resource ([Section 6](#section-6-appendices)).

<br />

# Introduction

The Unique Licensee Identifier (ULI) gives every licensed real estate professional a single identifier that links their records across MLSs and markets and deduplicates them. The ULI links established records rather than replacing their keys, and it is eventually consistent: a ULI may be superseded by a merge or redirect as the network resolves further, and the linked records repoint to the replacement rather than disappearing.

This proposal specifies a **resolution protocol**: how a participating system broadcasts a licensee, how a network of certified resolvers replies with a match or no match, how consensus mints or adopts a ULI and how the resolution is proven. Resolution runs over the W3C **ActivityPub** standard as a threaded, event-based conversation.

The protocol keeps each participant in control of its data: a provider searches its own data, replies match or no match and shares its licensee subset only through prearranged authorization to the resolvers it relies on ([Section 2.1](#section-21-participation-and-data-isolation)). The network itself stores nothing. Consensus over replies, rather than a central pool of data, is what resolves a licensee, which makes the network adoptable among competitors even though the ULI fields are neutral and widely available. Match strength is reported as a **credit-score-style score**: an arbitrary point scale with thresholds, deliberately not a probability.

The ULI is one of RESO's family of identifiers, alongside the Unique Organization Identifier (UOI) and Unique System Identifier (USI) of RCP-55, which this protocol uses to name the organization that mints each ULI.

<br />

# Section 1: Purpose

A licensed real estate professional has no industry-wide identifier. The same person appears as separate, unlinked records across MLSs, associations and systems, under different MLS IDs and different source systems, so the industry cannot reliably tell that two records are one person, follow a professional across markets or deduplicate counts. The nearest existing identifier is the National Association of REALTORS® (NAR) member ID carried in `MemberNationalAssociationId`. It covers only NAR members, so non-NAR licensees have none, and it carries duplication of its own; no existing identifier can serve as the universal one.

The **Unique Licensee Identifier (ULI)** fills that gap: a single identifier that links a licensed professional's records across markets and deduplicates them. Its primary audience is licensed agents and brokers, and it extends to any licensed practitioner in the transaction, for example appraisers and photographers. The ULI does not replace existing identifiers or record keys; it links established records, resolving across the NAR member ID, state license, name and office together as weighted inputs ([Section 2.9](#section-29-matching-engine-and-scoring)) so no single identifier has to be present or unique on its own.

A ULI is a linking identifier, not a primary key, and it is eventually consistent. As more of the network resolves a licensee, a ULI may be superseded by a merge or redirect; the linked records repoint to the replacement rather than disappearing, and a record always resolves to its current ULI by following the redirect chain.

This proposal specifies the **resolution protocol**: how a participating system broadcasts a licensee, how a network of certified resolvers replies with a match or no match, how consensus mints or adopts a ULI and how the resolution is proven ([Section 2](#section-2-specification)). It is built for three properties:

- **Decentralized and interoperable.** The matching backend, review user interface and resolver network are independent layers; any vendor may supply any piece.
- **Privacy-preserving.** No licensee data is stored on the network; a participant shares only the subset of PII-safe ULI fields it chooses, and only with the resolvers it authorizes ([Section 2.1](#section-21-participation-and-data-isolation), [Section 2.3](#section-23-uli-fields-and-payload)).
- **Self-correcting.** ULIs are created at consensus and reconciled afterward, so late information merges or redirects rather than fragments ([Section 2.7](#section-27-uli-creation-and-lifecycle)).

Resolved ULIs accumulate in a **ULI registry** that records each observed ULI and the markets it appears in; this protocol is how those ULIs are created and maintained over time.

<br />

# Section 2: Specification

The ULI Resolution Protocol uses W3C ActivityPub to make licensee resolution a threaded, event-based conversation across a network of certified resolvers. No licensee data is stored on the network; data is referenced by OAuth2-protected links to RESO Common Format (RCF) payloads. The protocol's three layers (matching backend, review user interface and resolver network) are independent: any conformant implementation of one MUST interoperate with any conformant implementation of another.

## Section 2.1: Participation and Data Isolation

A provider participates by searching its **own** data and replying with a match or no match. An originator references its shared ULI subset through an OAuth2-protected link ([Section 2.3](#section-23-uli-fields-and-payload)), and a resolver matches against the broadcast by dereferencing that link, which requires authorization. Access is therefore **prearranged**: the originator grants read permission to the resolvers it relies on, and that set MUST cover at least its mandatory resolvers plus enough additional resolvers to reach consensus ([Section 2.6](#section-26-consensus)). Authorizing the whole network is preferable but not required. The network itself MUST NOT store licensee data: the shared subset lives only behind the originator's OAuth2 links and is read only by authorized resolvers.

No provider is forced to open its data to the entire network or to any particular competitor. It controls which resolvers it authorizes, shares only the subset of ULI fields it chooses ([Section 2.3](#section-23-uli-fields-and-payload)) and grants no more access than resolution requires. Resolution is reached by consensus over replies ([Section 2.6](#section-26-consensus)), not by pooling data into a central store, so the protocol works among participants, competitors included, without anyone surrendering control of their data or trusting a shared repository.

## Section 2.2: ActivityPub Usage

Implementations MUST use the standard [Activity Streams 2.0 vocabulary](https://www.w3.org/TR/activitystreams-vocabulary/), for example [`Note`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-note), [`Create`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-create) and [`Tombstone`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-tombstone). Implementations MUST NOT extend [ActivityPub](https://www.w3.org/TR/activitypub/)'s JSON-LD with custom terms. Any licensee data or matching detail the protocol requires (for example a match score or a candidate record) MUST be carried in the RCF payload that the activity references, not in the ActivityPub object. A ULI is an identifier rather than data, so a ULI URN – including a tombstone's redirect target – MAY appear directly in the activity, as it does in a thread reference.

## Section 2.3: ULI Fields and Payload

The licensee data exchanged is drawn from a PII-safe set of ULI fields, all carried on the Member Resource. Each field links to its Data Dictionary definition; the *Role in resolution* column summarizes how the matching engine ([Section 2.9](#section-29-matching-engine-and-scoring)) uses it.

| Field | Data Dictionary 2.0 definition | Role in resolution | Markets (of 353) |
| :--- | :--- | :--- | ---: |
| [`MemberFullName`](https://dd.reso.org/DD2.0/Member/MemberFullName/) | The first, middle and last name of the member or an alternate full name. | Primary identifier (Full Name, 700). | 323 |
| [`MemberFirstName`](https://dd.reso.org/DD2.0/Member/MemberFirstName/) | The first name of the member. | Supporting term; component of Full Name. | 345 |
| [`MemberMiddleName`](https://dd.reso.org/DD2.0/Member/MemberMiddleName/) | The middle name of the member. | Supporting term. | 270 |
| [`MemberLastName`](https://dd.reso.org/DD2.0/Member/MemberLastName/) | The last name of the member. | Supporting term; component of Full Name and Nickname + Last Name. | 344 |
| [`MemberNickname`](https://dd.reso.org/DD2.0/Member/MemberNickname/) | An alternate name used by the member, usually as a substitute for the first name. | Primary identifier as Nickname + Last Name (650); also a supporting term. | 94 |
| [`MemberType`](https://dd.reso.org/DD2.0/Member/MemberType/) | The type of member (i.e., Agent, Broker, Office Manager, Appraiser, Photographer, Assistant, Mortgage Loan Originator, REALTOR, Association Staff, MLS Staff, etc.). | Context; not scored. | 307 |
| [`MemberNationalAssociationId`](https://dd.reso.org/DD2.0/Member/MemberNationalAssociationId/) | The national association ID of the member (e.g., in the U.S., this is the NRDS number). | Strongest primary identifier (800). | 319 |
| [`MemberStateLicense`](https://dd.reso.org/DD2.0/Member/MemberStateLicense/) | The license of the member. Multiple licenses should be separated by a comma and space. | Primary identifier with State (600), or with State + Type (625). | 323 |
| [`MemberStateLicenseState`](https://dd.reso.org/DD2.0/Member/MemberStateLicenseState/) | The state in which the member is licensed. | Pairs with the license number to form a primary identifier. | 52 |
| [`MemberStateLicenseType`](https://dd.reso.org/DD2.0/Member/MemberStateLicenseType/) | The license type of the member. | Strengthens a license match (Number + State + Type, 625); the type alone is a supporting term. | 9 |
| [`MemberMlsId`](https://dd.reso.org/DD2.0/Member/MemberMlsId/) | The local, well-known identifier for the member. This value may not be unique, specifically in the case of aggregation systems, and it should be the identifier from the original system. | Context; not a scored identifier. | 337 |
| [`OfficeMlsId`](https://dd.reso.org/DD2.0/Member/OfficeMlsId/) | The local, well-known identifier. This value may not be unique, specifically in the case of aggregation systems, and it should be the identifier from the original system. | Secondary identifier (Office ID, 100); scored once within the office cluster. | 329 |
| [`OfficeName`](https://dd.reso.org/DD2.0/Member/OfficeName/) | The legal name of the brokerage. | Secondary identifier (Office Name, 80); scored once within the office cluster. | 256 |
| [`SourceSystemID`](https://dd.reso.org/DD2.0/Member/SourceSystemID/) | The OUID Resource's OrganizationUniqueId of the source record provider. The source system is the system from which the record was directly received. In cases where the source system was not where the record originated (the authoritative system), see the Originating System fields. | Provenance. | 208 |
| [`SourceSystemName`](https://dd.reso.org/DD2.0/Member/SourceSystemName/) | The name of the immediate record provider. The system from which the record was directly received. The legal name of the company. | Provenance. | 144 |
| [`OriginatingSystemID`](https://dd.reso.org/DD2.0/Member/OriginatingSystemID/) | The OUID Resource's OrganizationUniqueId of the originating record provider. The originating system is the system with authoritative control over the record (e.g., the MLS where the member was input). In cases where the originating system was not where the record originated (the authoritative system), see the Originating System fields. | Provenance. | 253 |
| [`OriginatingSystemName`](https://dd.reso.org/DD2.0/Member/OriginatingSystemName/) | The name of the originating record provider, most commonly the name of the MLS. The place where the member is originally input by the member. The legal name of the company. | Provenance. | 323 |

The *Markets* column counts the reporting markets that publish each field, of 353 in the RESO Data Dictionary 1.7 Industry Aggregates (December 2025); adoption is uneven, from near-universal names and identifiers down to a rarely broken-out license state and type. `MemberStateLicenseType` is defined from Data Dictionary 2.0; it is not a Data Dictionary 1.7 field.

The resolved identifier itself is carried in the Member Resource's existing [`UniqueLicenseeIdentifier`](https://dd.reso.org/DD2.0/Member/UniqueLicenseeIdentifier/) field (Data Dictionary 2.0 and later), as the URN of [Section 2.8](#section-28-signing-and-self-verification).

A node MAY share any subset of these fields; the matching engine ([Section 2.9](#section-29-matching-engine-and-scoring)) scores on whatever factors are present, so withholding fields lowers a node's own match strength but does not break resolution. The shared subset MUST be expressed in RESO Common Format and MUST be retrieved over an OAuth2-protected link rather than embedded in the ActivityPub object. A ULI is a self-verifying URN of the form `urn:reso:uli:1.0:<uoi>:<nonce>:<signature>`, carrying a signature from its minting organization ([Section 2.8](#section-28-signing-and-self-verification)).

## Section 2.4: Broadcast

Before resolving, a node MUST search its own data first. **A node MUST NOT create a ULI on its own:** every ULI is created only after the network confirms, through broadcast and consensus, that no existing ULI matches the licensee ([Section 2.6](#section-26-consensus), [Section 2.7](#section-27-uli-creation-and-lifecycle)). Two cases drive a broadcast:

- **New licensee.** The node holds no ULI and its local search finds none. It broadcasts to resolve; on a no-match consensus it mints the ULI and broadcasts the creation.
- **Changed data.** The node already holds a ULI but ULI-relevant fields have changed. It reruns its local search against the new data and MUST broadcast the licensee request so the network resolves it again; a change can surface a match or a merge the old data did not.

A node holding a ULI MAY also rebroadcast at any time with the original criteria.

A node MUST NOT broadcast a record that does not clear the **identifiability threshold**: the record's own present factors MUST score highly enough that a full match could reach the match threshold ([Section 2.9](#section-29-matching-engine-and-scoring)). In practice this requires at least two primary identifiers, since a single primary identifier cannot reach the threshold alone. This keeps under-identified records (for example, a common name alone) off the broadcast path. A below-threshold match is handled out of band: a resolver MAY contact the originator privately with a direct message to its inbox rather than reply in the public thread. The coordinated-resolution behavior for below-threshold matches is reserved for a future revision of this proposal.

A broadcast is one ActivityPub `Note` per member, addressed to the shared certified-resolver collection (`to: https://reso.org/resolvers`, which every resolver follows, [Section 2.10](#section-210-resolver-discovery-and-the-governance-feed)), and referencing the shared ULI subset by an OAuth2-protected RCF link. Any resolver the originator requires (its mandatory list, [Section 2.6](#section-26-consensus)) is additionally named with an ActivityPub `Mention` – a `Mention` is what makes a resolver mandatory for that broadcast.

Each thread is a transaction for the licensee and is ephemeral. Resolvers SHOULD respond in a timely manner, because messages MAY be deleted at any time (an ActivityPub `Delete`, which also satisfies PII removal). A transaction MAY complete in seconds when nodes respond quickly, or over several minutes for a fuller conversation.

## Section 2.5: Resolution and Reply

An **active** resolver SHOULD reply in the thread with either a potential match, carrying an OAuth2-protected link to its candidate ULI's RCF data ([Section 2.11](#section-211-authentication-and-authorization)), or a no-match. A no-match is a real contribution: it is what lets consensus ([Section 2.6](#section-26-consensus)) complete, so silence and "no match" are not the same thing. A resolver named on the originator's mandatory list ([Section 2.6](#section-26-consensus)), addressed by an ActivityPub `Mention`, MUST reply. A resolver MUST NOT reply with a match unless the match score clears the match threshold ([Section 2.9](#section-29-matching-engine-and-scoring)). A match reply MUST include its score in the referenced RCF payload, so that consensus and later correction ([Section 2.7](#section-27-uli-creation-and-lifecycle)) can rank competing matches. A resolver that chronically fails to reply is not penalized as noncompliant; it simply trends to **inactive** ([Section 2.10](#section-210-resolver-discovery-and-the-governance-feed)) and drops out of `R`, so persistent silence lowers the bar rather than stalling it. Matching is performed by the shared matching engine ([Section 2.9](#section-29-matching-engine-and-scoring)).

## Section 2.6: Consensus

Consensus requires two independent conditions: at least `floor(2/3 × R)` resolvers have replied in the thread, **and** every resolver on the requesting node's mandatory list is among those replies. The mandatory resolvers are always a subset of `R` and count toward the `floor(2/3 × R)`; the remainder is filled by nonmandatory replies. The mandatory condition is absolute: reaching the count with nonmandatory replies alone is not consensus while a mandatory resolver has not replied. With no mandatory list, consensus is the simple count. The choice of `floor` over `ceiling` is deliberate; see the design rationale in [Section 6](#section-6-appendices).

Each node **chooses its own mandatory list** – the resolvers it names with a `Mention` ([Section 2.4](#section-24-broadcast)) – and is responsible for choosing them correctly. The network does not impose a mandatory set: an originator requires whatever resolvers it trusts to hold an authoritative record – an affiliated body, a system covering the licensee's market or a specialized verification service. Under-naming an authoritative resolver risks a duplicate that later self-heals, and is the kind of thing the audit ([Section 3](#section-3-certification)) surfaces.

`R` is the thread's resolver pool, derived from RESO's governance feed ([Section 2.10](#section-210-resolver-discovery-and-the-governance-feed)) and **pinned at broadcast** to the feed position the broadcast references, so every participant computes the same `R`. It counts all certified, compliant, active resolvers, online or offline. `R` is fixed for the thread except that it **shrinks** if a member is pruned (marked inactive or noncompliant) mid-thread, lowering the bar so a departed node cannot stall the thread. Because the mandatory list is a subset of `R`, a pruned member also leaves the mandatory requirement, so an inactive or noncompliant mandatory resolver cannot block a resolution.

Replies accumulate over time: online resolvers reply at once, and an offline resolver MUST, on resume, drain its inbox and reply to every open thread it was tagged in. There is no fixed timeout – a thread lives until consensus, after which resolvers MAY remove it. A resolver that holds a match for a thread already resolved and gone MUST rebroadcast the original criteria so the late match can still surface ([Section 2.7](#section-27-uli-creation-and-lifecycle)).

Consensus is participation-based: once the count is met with any mandatory sign-offs present, the originating node selects the outcome – adopt the highest-scoring match over threshold, else mint a new ULI ([Section 2.7](#section-27-uli-creation-and-lifecycle)) – and self-healing reconciles any later, better match.

## Section 2.7: ULI Creation and Lifecycle

### Mint

ULIs are created first, but never unilaterally: a new ULI is minted only on a no-match consensus ([Section 2.6](#section-26-consensus)), and the originating provider then posts the created ULI to the thread. While resolution is in flight a provider MAY tag its records with a stable local **placeholder** (a local handle, not a ULI) so that binding the resolved ULI updates one pointer rather than every record. The mint is the consensus event, not a local decision.

### Adopt

When consensus lands on an existing match instead, no ULI is minted: the originating provider adopts that ULI and binds its records to it **locally**. Adoption posts nothing further to the thread, because it changes no shared state – the ULI already exists, carrying its own minting signature ([Section 2.8](#section-28-signing-and-self-verification)), and the ULI registry ([Section 1](#section-1-purpose)) observes the ULI's new usage. As a general rule, a node posts an outcome to the thread only when that outcome changes shared ULI state: a mint, a tombstone or a merge.

### Tombstone and Redirect

A resolver MAY surface a better match – a higher score – after a ULI has been minted. This triggers a correction: the superseded ULI MUST be tombstoned (ActivityPub `Tombstone`) and redirected to the better one. A supersession does not need a fresh consensus round the way a mint does: a mint rests on an *absence* of matches, which only the network's replies can establish, whereas a better match is a **present, verifiable** fact – its score is independently recomputable by any participant from the published criteria ([Section 2.9](#section-29-matching-engine-and-scoring)). So the gate is lighter: the correction commits once a **holder** of the superseded ULI acknowledges a better match that clears the threshold, by issuing the tombstone. Any holder MAY initiate it; an offline holder neither blocks the redirect nor must ratify it on return – on return it simply follows the redirect, relinking its records to the successor.

Because the ULI is a linking identifier rather than a primary key, a tombstone never removes the underlying records: they relink to the surviving ULI, so resolution is eventually consistent and continuity is preserved by the redirect chain. A node that ignores a redirect it has seen creates only a local divergence, which heals on the next resolution touching that licensee.

### Merge

When one licensee is found under two ULIs, the two are collapsed into one. A merge is not a distinct operation but a **composition** of the primitives above: the originator broadcasts the **merged criteria with the two ULIs being retired excluded from matching** – asking whether any *other* resolver holds the person – and resolves it normally. A no-other-match consensus mints a neutral new ULI; an other-match adopts that existing ULI. The target is established **first**; then each holder tombstones its own retired ULI and redirects it to the target – retiring one's own identifier into an established target needs no separate round beyond the consensus that minted it. Both originals are excluded from matching, so a merge never keeps one of the two – it always lands on a new or third-party ULI. [Section 2.12.4](#section-2124-merge-of-two-existing-ulis) works through this sequence.

### Decline

A match may clear the score threshold yet be wrong – two different licensees who share enough identifying data, often because of a data error. The originator MAY decline a candidate on human review. A decline is **not** a separate activity: the originator rebroadcasts the **corrected criteria** into the same thread, which reopens resolution through the ordinary path. The outcome is whatever that resolution returns – a no-match consensus mints a fresh ULI, or a correct existing ULI is adopted – so the mint stays consensus-gated, never the human's unilateral act. Declining a false positive is a clean separation: the wrongly matched ULI is left untouched, not merged.

## Section 2.8: Signing and Self-Verification

A ULI is **self-verifying**: it carries, in the identifier itself, an Ed25519 signature from the organization that minted it. Anyone can confirm a ULI was created by a certified network participant – offline, with no registry and no call back to the minter – by checking that signature against the organization's published key. This catches the realistic failure mode: a fabricated or mistaken identifier that looks like a ULI but never came from the network, dropped into a data set by error or shortcut.

The signature proves **origin**, not consensus. It answers the question that keeps spurious identifiers out of the graph – "did a certified organization mint this?" – and nothing more. Whether a mint was correctly consensus-gated ([Section 2.6](#section-26-consensus)) is enforced separately, by certification and continuous monitoring ([Section 3](#section-3-certification)). The signature also binds nothing about the licensee: signing licensee data would tie the identifier to a value that changes over time and break its stability as a linking identifier.

### Identifier Form

A ULI is a URN of the form:

`urn:reso:uli:1.0:<uoi>:<nonce>:<signature>`

| Segment | Meaning |
| :--- | :--- |
| `urn:reso:uli:1.0` | The fixed scheme, namespace and version prefix. The `urn:` scheme and the `reso` namespace identifier compare case-insensitively under the URN syntax ([RFC 8141](https://www.rfc-editor.org/rfc/rfc8141)); this proposal treats the whole prefix as case-insensitive on comparison, and signing always uses its lowercase form (see *What Is Signed*). |
| `<uoi>` | The Unique Organization Identifier (UOI) of the **minting** organization, as the URN defined by [RCP-55 Organization and System Identifiers](https://github.com/RESOStandards/transport/pull/243): `urn:reso:uoi:1.0:<issuer>:<unique-identifier>`, for example `urn:reso:uoi:1.0:T00000012:M00000123` for a RESO-issued UOI. A UOI always has exactly six colon-separated segments (RCP-55 percent-encodes any colon inside `<unique-identifier>`), so it is carried whole. It identifies whose key signed the ULI and traces the ULI to its origin. It is the minting organization, **not** the licensee's current location – that is a resolution and registry question. |
| `<nonce>` | A short random value (base64url) that makes each ULI unique by construction, independent of the signature scheme. |
| `<signature>` | An Ed25519 signature ([RFC 8032](https://www.rfc-editor.org/rfc/rfc8032)) by the minting organization over the rest of the identifier, base64url-encoded without padding (86 characters). |

Everything after the version is **case-sensitive** and uses the base64url alphabet ([RFC 4648, Section 5](https://www.rfc-editor.org/rfc/rfc4648#section-5)), so a ULI MUST NOT be case-folded or otherwise normalized; doing so corrupts the signature. The colon is the segment delimiter and never occurs within a segment, so a ULI has exactly twelve segments: the four-segment `urn:reso:uli:1.0` prefix, the six-segment UOI, the nonce and the signature. A reader parses it by position – the last segment is the signature, the one before it the nonce, and the six between the prefix and the nonce are the UOI.

### What Is Signed

The minting organization signs the **US-ASCII bytes of the identifier up to, and excluding, the signature** – the exact string `urn:reso:uli:1.0:<uoi>:<nonce>`, with the scheme and version prefix in its canonical lowercase form – with its RESO-certified Ed25519 private key, then appends the signature as the final segment. The prefix compares case-insensitively, but signing and verification always take it lowercase, so the signed bytes are unambiguous across implementations.

Because every character of that string is in the URN-allowed set – the literal prefix, an alphanumeric UOI, a base64url nonce – it is pure US-ASCII: each character is a single byte, with no percent-encoding and no Unicode normalization, so the bytes to sign are canonical by construction. Signing the full prefix, including `uli:1.0`, binds the signature to this exact namespace and version, so it cannot be replayed under another protocol or a future version.

### Verifying

Given a ULI, a verifier:

1. splits off the final `<signature>` segment and reconstructs the signed string `urn:reso:uli:1.0:<uoi>:<nonce>`;
2. takes the six UOI segments between the prefix and the nonce and resolves that UOI to the minting organization's RESO-certified Ed25519 public key on the governance feed ([Section 2.10](#section-210-resolver-discovery-and-the-governance-feed));
3. checks the signature over the reconstructed bytes.

If it verifies, the ULI was minted by that certified organization; if not, it is not a genuine ULI. No secret and no online service are required – only the organization's public key, which a verifier syncs once from the feed and then uses offline. The feed retains the keys of organizations that have rotated, merged or left ([Section 2.10](#section-210-resolver-discovery-and-the-governance-feed)), so a ULI stays verifiable for the life of the identifier; where an organization has had more than one key, a signature matching any of its certified keys verifies.

The ULI registry ([Section 1](#section-1-purpose)) MAY also record which organization minted a ULI and the consensus behind it, but verification never depends on it: the proof rides in the identifier, the one thing that always travels with the data.

### Worked Example

A minting organization whose RESO-issued UOI is `urn:reso:uoi:1.0:T00000012:M00000123` creates a ULI. It draws a nonce, builds the prefix, signs the prefix's US-ASCII bytes with its Ed25519 key and appends the signature:

```
UOI           urn:reso:uoi:1.0:T00000012:M00000123
nonce         nyxKF6Gyw9Re
signed bytes  urn:reso:uli:1.0:urn:reso:uoi:1.0:T00000012:M00000123:nyxKF6Gyw9Re        (US-ASCII, 66 bytes)
signature     zByDTVoX3z7TNVgH3SVKVQCaA2RcUK7dAJK70Pu48rExxBiCGu5Sz6lhPAChBPEs-Gb3TXjEkJOspFZkDjiABg

ULI  urn:reso:uli:1.0:urn:reso:uoi:1.0:T00000012:M00000123:nyxKF6Gyw9Re:zByDTVoX3z7TNVgH3SVKVQCaA2RcUK7dAJK70Pu48rExxBiCGu5Sz6lhPAChBPEs-Gb3TXjEkJOspFZkDjiABg
```

To check it, a reader strips the signature, reconstructs `urn:reso:uli:1.0:urn:reso:uoi:1.0:T00000012:M00000123:nyxKF6Gyw9Re`, looks up the certified Ed25519 public key of `urn:reso:uoi:1.0:T00000012:M00000123` on the governance feed – here base64url `3F0yXARgFnOg1PnvBNs-PEsNA-I4VLafdjmlklecppw` – and verifies the signature over those bytes.

## Section 2.9: Matching Engine and Scoring

The same matching engine runs locally (a participant's private deduplication) and on the network (each resolver). A candidate is scored by summing the weights of the identifiers it matches. The score is a **credit-score-style** value: an arbitrary point scale with thresholds, deliberately not a probability. Primary identifiers are weighted an order of magnitude above secondary ones, so a ULI cannot be validated by secondary identifiers alone.

| Tier | Weight | Identifier |
| :--- | :--- | :--- |
| Primary | 800 | MemberNationalAssociationId (NRDS) |
| Primary | 700 | Full Name (or First Name + Last Name) |
| Primary | 650 | Full Nickname (or Nickname + Last Name) |
| Primary | 625 | License (Number + State + Type) |
| Primary | 600 | License (Number + State) |
| Secondary | 100 | Office ID |
| Secondary | 80 | Office Name |
| Other | 10 each | Last Name, First Name, Middle Name, Nickname, License Type |

A combined score at or above **1300** is a possible match; a score below **700** is rejected; scores between are inconclusive and SHOULD be routed to review. Correlated identifiers that express the same underlying fact – for example office ID and office name – MUST be scored as a single cluster (the strongest matched member), not summed, so that one fact is not counted several times.

Term weights derive from a published static frequency table (rarer values carry more weight), so common values such as a frequent surname cannot on their own clear the threshold. Participants MAY tune weights in their own sandbox, but the network MUST define an official adopted weight set **and** threshold; both are network parameters. When either changes, RESO posts the new set to the governance feed ([Section 2.10](#section-210-resolver-discovery-and-the-governance-feed)); each node MUST rerun matching locally, compute diffs and broadcast the results. The reference implementation computes relevance with a BM25-style score; see [Section 6](#section-6-appendices) for the rationale and references.

**Exact parity.** Two conforming engines MUST produce the same score for the same candidate, so the determinism-sensitive choices are pinned rather than left to the implementation. The adopted weight set, the threshold and the **versioned frequency table** the weights derive from are network parameters posted to the governance feed ([Section 2.10](#section-210-resolver-discovery-and-the-governance-feed)). String and value **normalization** (case folding, accent and whitespace handling, nickname expansion), **tie-breaking** between equal-scoring candidates and the **rounding** of the combined score are fixed by the reference harness and its published test vectors ([Section 3](#section-3-certification)), exactly as the signed-identifier bytes are ([Section 2.8](#section-28-signing-and-self-verification)): an engine conforms if and only if it reproduces the reference scores on those vectors. The Phase 2 global-IDF option ([Section 6](#section-6-appendices)) is a future refinement, not part of the parity baseline; until it is adopted the static frequency table is the sole weight source.

The network also leaves room for **specialized resolvers** to provide additional layers of identity verification using PII-based approaches, beyond the shared matching engine described here.

## Section 2.10: Resolver Discovery and the Governance Feed

All participants certified on the RESO network are ULI resolvers, continuously. A resolver's identity is its Unique Organization Identifier (UOI), confirmed with a hash of the URL of the node on which it was certified.

The certified ULI resolvers – the membership behind the `reso.org/resolvers` collection a broadcast addresses ([Section 2.4](#section-24-broadcast)) – are published through RESO's Organizations and Endorsements feed, queried by endorsement name:

**REQUEST**

```
GET https://services.reso.org/endorsements?endorsementName=ULI
  Accept: application/json
```

**RESPONSE** (the ULI-endorsed resolver organizations that form `R`; the shape and location are illustrative – the feed, its location and its field names `OrganizationId` and `SystemId` are defined by RCP-55)

```json
{
  "endorsementName": "ULI",
  "value": [
    {
      "OrganizationId": "<UOI>",
      "SystemId": "<USI>",
      "nodeUrlHash": "<SHA-256 of the certified node URL>",
      "status": "active"
    }
  ]
}
```

The endorsements feed is **public**, so it never publishes a resolver's node URL – only the **hash** (SHA-256) of that URL, alongside the resolver's Unique Organization Identifier (UOI) and Unique System Identifier (USI). The URL itself is exchanged later, through prearranged authorization ([Section 2.1](#section-21-participation-and-data-isolation)): when a node authenticates to a resolver and is handed that resolver's URL with its tokens, it hashes the URL and compares the result to the certified hash here. A match confirms the node is the one RESO certified – identity verified without the registry ever exposing a URL.

RESO publishes a **public governance feed** – an ordered ActivityPub stream every resolver follows. It is the resolver registry and the single channel for all network-level events: a resolver certified and joined (with its Unique Organization Identifier and RESO-certified Ed25519 public key, [Section 2.8](#section-28-signing-and-self-verification)), a status change (inactive, reactivated, noncompliant) and parameter changes (weights and thresholds, [Section 2.9](#section-29-matching-engine-and-scoring)). Every event is PII-free – organization identifiers, public keys, status and parameters only, never licensee data – so the feed is safe to be public and serves as a transparent, auditable membership-and-parameter log. A node builds its `R` from that endorsement set – equivalently, by replaying the feed – and follows the feed for live deltas; a feed position is the version a broadcast pins ([Section 2.6](#section-26-consensus)).

The resolver pool `R` is a filtered view of the feed: a node is in `R` if and only if it is **certified, compliant and active**. Two flags remove a node, neither deleting its record (so the ULIs it minted stay verifiable against its retained keys, [Section 2.8](#section-28-signing-and-self-verification)):

- **Inactive** – not seen for the staleness window (one month by default); soft and reversible. The node keeps its certification and rejoins `R` automatically when it returns and participates. Liveness is an active reachability heartbeat (RESO polls daily), independent of whether the node has matches to offer – a node that only ever replies "no match" is fully live.
- **Noncompliant** – failed continuous conformance ([Section 3.2](#section-32-continuous-conformance)); an immediate, hard removal. The node MUST recertify to return.

Only governance rides the public feed. The resolution conversations stay peer-to-peer and behind authentication ([Section 2.11](#section-211-authentication-and-authorization)); the public feed never carries a ULI thread or any licensee data.

## Section 2.11: Authentication and Authorization

Access to the network MUST be authenticated. Each participant node handles its own authorization from that point, using the existing RESO Web API Core and Data Dictionary endpoints or hosting RESO Common Format, with OAuth2 bearer tokens or client credentials as defined in [Web API Core 2.1.0, Section 2.9](https://transport.reso.org/proposals/web-api-core/). Records are carried only by OAuth2-protected URLs to external services ([Section 2.1](#section-21-participation-and-data-isolation)).

## Section 2.12: Worked Examples

These examples illustrate the core resolution flows, in increasing order of complexity: [Section 2.12.1](#section-2121-new-uli-no-match-found) a new ULI when no match is found, [Section 2.12.2](#section-2122-existing-uli-found-and-adopted) an existing ULI found and adopted, [Section 2.12.3](#section-2123-late-match-correction) a late-match self-healing correction, [Section 2.12.4](#section-2124-merge-of-two-existing-ulis) two existing ULIs merged and [Section 2.12.5](#section-2125-match-found-but-declined-on-review) a match found but declined on review. Each renders the threaded conversation as a step table, then shows the request and response activities.

The ActivityPub objects use only standard Activity Streams 2.0 vocabulary (no extensions, per [Section 2.2](#section-22-activitypub-usage)). The RCF payloads validate against the Data Dictionary schema (Member Resource, Data Dictionary 2.0, strict). Licensee data and matching detail an activity needs (a match score, a candidate record) ride in the referenced RCF payload, not in the ActivityPub object; a ULI URN, including a tombstone's redirect target, is an identifier and appears directly. Hosts are illustrative (`provider1.example.com` and so on). Provider 1 has the RESO-issued UOI `urn:reso:uoi:1.0:T00000012:M00000123`, Provider 5 `urn:reso:uoi:1.0:T00000012:M00000555` and Provider 8 `urn:reso:uoi:1.0:T00000012:M00000888` (RCP-55). ULIs are shown in the full form of [Section 2.8](#section-28-signing-and-self-verification) with a shortened nonce and the signature segment elided as `<signature>`; step tables abbreviate a ULI to the last characters of its nonce (`urn:…4a17`). Every `/rcf/...` link below is an OAuth2-protected URL ([Section 2.11](#section-211-authentication-and-authorization)) – the activities are public, the data behind the links is not. Throughout, `R` resolvers are on the network and consensus is `floor(2/3 × R)`.

### Section 2.12.1: New ULI, No Match Found

A provider resolves a licensee it holds no ULI for; the network confirms no match; a new ULI is minted at consensus and posted to the thread.

**Thread.** The conversation is a single broadcast `Note` with each resolver's reply threaded beneath it; the originator's `Create` closes the thread once consensus is reached. The table reads top to bottom as the thread does.

| # | Actor | Activity | Description |
| :--- | :--- | :--- | :--- |
| 1 | Provider 1 | Broadcast `Note` "ULI Resolution Request" | Fans out to the certified resolvers; the licensee subset stays behind the OAuth2 link, never embedded in the activity. |
| 2 | Other resolvers | No Match | Each resolver searches its own data and replies in the thread. Replies accumulate toward `floor(2/3 × R)`. |
| – | – | No-match consensus | Enough no-match replies are in to clear the bar; no resolver on the network holds this licensee. |
| 3 | Provider 1 | `Create` ULI `urn:…4a17` | Only now – never unilaterally – the originator mints the ULI and posts it as a thread message bound to the licensee record. |

**Broadcast.** The originator posts the request to its outbox; the server wraps it in a `Create`, assigns identifiers to the activity and the object, and federates it to the resolver collection. The `Note` is the thread root that replies reference.

**REQUEST**

```
POST https://provider1.example.com/actor/outbox
  Content-Type: application/ld+json; profile="https://www.w3.org/ns/activitystreams"
```

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Note",
  "to": "https://reso.org/resolvers",
  "content": "ULI Resolution Request",
  "url": { "type": "Link", "href": "https://provider1.example.com/rcf/member/licensee-123", "mediaType": "application/json" }
}
```

**RESPONSE**

```
HTTP/2 201 Created
  Location: https://provider1.example.com/activities/create-xyz
  Content-Type: application/ld+json; profile="https://www.w3.org/ns/activitystreams"
```

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Create",
  "id": "https://provider1.example.com/activities/create-xyz",
  "actor": "https://provider1.example.com/actor",
  "to": "https://reso.org/resolvers",
  "object": {
    "type": "Note",
    "id": "https://provider1.example.com/threads/xyz",
    "attributedTo": "https://provider1.example.com/actor",
    "to": "https://reso.org/resolvers",
    "content": "ULI Resolution Request",
    "url": { "type": "Link", "href": "https://provider1.example.com/rcf/member/licensee-123", "mediaType": "application/json" }
  }
}
```

**Licensee subset** behind the OAuth2-protected link (a Data Dictionary-validated Member record):

**REQUEST**

```
GET https://provider1.example.com/rcf/member/licensee-123
  Authorization: Bearer <token>
  Accept: application/json
```

**RESPONSE**

```
HTTP/2 200 OK
  Content-Type: application/json
```

```json
{
  "@reso.context": "urn:reso:metadata:2.0:resource:member",
  "value": [
    {
      "MemberFullName": "Jane A Smith",
      "MemberFirstName": "Jane",
      "MemberMiddleName": "A",
      "MemberLastName": "Smith",
      "MemberNickname": "Janie",
      "MemberType": "REALTOR Salesperson",
      "MemberNationalAssociationId": "999000111",
      "MemberStateLicense": "SL123456",
      "MemberStateLicenseType": "Salesperson",
      "MemberStateLicenseState": "CA",
      "MemberMlsId": "A0001",
      "OfficeName": "Smith Realty Group",
      "OfficeMlsId": "OFF001",
      "SourceSystemID": "M00000123",
      "SourceSystemName": "Source MLS",
      "OriginatingSystemID": "M00000123",
      "OriginatingSystemName": "Originating MLS"
    }
  ]
}
```

**No-match reply.** Each resolver that finds no match replies in the thread; no `url` is carried:

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Note",
  "attributedTo": "https://provider2.example.com/actor",
  "inReplyTo": "https://provider1.example.com/threads/xyz",
  "content": "No Match"
}
```

**ULI created.** At no-match consensus, the originator mints the ULI and posts the `Create` to the thread. The created `Note` carries its own identifier, which a later tombstone references ([Section 2.12.3](#section-2123-late-match-correction)):

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Create",
  "actor": "https://provider1.example.com/actor",
  "object": {
    "type": "Note",
    "id": "https://provider1.example.com/uli/4a17",
    "attributedTo": "https://provider1.example.com/actor",
    "inReplyTo": "https://provider1.example.com/threads/xyz",
    "content": "ULI Created",
    "url": { "type": "Link", "href": "https://provider1.example.com/rcf/uli/4a17", "mediaType": "application/json" }
  }
}
```

The new ULI's RCF payload is the licensee record bound to the minted URN, carried in the Member Resource's `UniqueLicenseeIdentifier` field:

```json
{
  "@reso.context": "urn:reso:metadata:2.0:resource:member",
  "value": [
    {
      "UniqueLicenseeIdentifier": "urn:reso:uli:1.0:urn:reso:uoi:1.0:T00000012:M00000123:4a178d3c4e92:<signature>",
      "MemberFullName": "Jane A Smith",
      "MemberFirstName": "Jane",
      "MemberMiddleName": "A",
      "MemberLastName": "Smith",
      "MemberNickname": "Janie",
      "MemberType": "REALTOR Salesperson",
      "MemberNationalAssociationId": "999000111",
      "MemberStateLicense": "SL123456",
      "MemberStateLicenseType": "Salesperson",
      "MemberStateLicenseState": "CA",
      "MemberMlsId": "A0001",
      "OfficeName": "Smith Realty Group",
      "OfficeMlsId": "OFF001",
      "SourceSystemID": "M00000123",
      "SourceSystemName": "Source MLS",
      "OriginatingSystemID": "M00000123",
      "OriginatingSystemName": "Originating MLS"
    }
  ]
}
```

### Section 2.12.2: Existing ULI Found and Adopted

The network returns a match; the originator adopts the existing ULI rather than minting a new one. The broadcast is identical to [Section 2.12.1](#section-2121-new-uli-no-match-found).

**Thread.** A resolver that already holds the licensee replies with a match that links to the existing ULI. The originator adopts that ULI **locally**, posting nothing further to the thread.

| # | Actor | Activity | Description |
| :--- | :--- | :--- | :--- |
| 1 | Provider 1 | Broadcast `Note` "ULI Resolution Request" | Identical to the [Section 2.12.1](#section-2121-new-uli-no-match-found) broadcast; the originator does not yet know whether a ULI exists for this licensee. |
| 2 | Provider 8 | Matches Found → `urn:…9f2c` (score 1450) | Provider 8 already holds this licensee under an existing ULI. The score (1450, above the 1300 bar) rides in the linked payload, not in the activity itself. |
| 3 | Other resolvers | No Match | The other resolvers do not hold the licensee; their replies still count toward consensus. |
| – | – | Match consensus | Enough resolvers have replied, and a match is among them. |
| – | Provider 1 | Adopt `urn:…9f2c` – local, no thread message | The originator binds the existing ULI to its record. Adoption is the point of the network – one licensee, one ULI, no matter how many systems hold a record for them. |

**Match reply.** A resolver that holds a match replies in the thread; the `url` is OAuth2-protected ([Section 2.11](#section-211-authentication-and-authorization)) and resolves, for an authorized reader, to a **resolution payload** carrying the ULI, the match score and the candidate Member record (the Member part is Data Dictionary-validated).

The reply activity:

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Note",
  "attributedTo": "https://provider8.example.com/actor",
  "inReplyTo": "https://provider1.example.com/threads/xyz",
  "content": "Matches Found",
  "url": { "type": "Link", "href": "https://provider8.example.com/rcf/uli/9f2c", "mediaType": "application/json" }
}
```

The resolution payload, dereferenced from the link:

```json
{
  "uli": "urn:reso:uli:1.0:urn:reso:uoi:1.0:T00000012:M00000888:9f2c2b6e4f1a:<signature>",
  "score": 1450,
  "licensee": {
    "@reso.context": "urn:reso:metadata:2.0:resource:member",
    "value": [
      {
        "UniqueLicenseeIdentifier": "urn:reso:uli:1.0:urn:reso:uoi:1.0:T00000012:M00000888:9f2c2b6e4f1a:<signature>",
        "MemberFullName": "Jane Smith",
        "MemberFirstName": "Jane",
        "MemberMiddleName": "A",
        "MemberLastName": "Smith",
        "MemberNickname": "Janie",
        "MemberType": "REALTOR Salesperson",
        "MemberNationalAssociationId": "999000111",
        "MemberStateLicense": "SL123456",
        "MemberStateLicenseType": "Salesperson",
        "MemberStateLicenseState": "CA",
        "MemberMlsId": "B7788",
        "OfficeName": "Smith Realty Group",
        "OfficeMlsId": "OFF042",
        "SourceSystemID": "M00000888",
        "SourceSystemName": "Resolver 8 MLS",
        "OriginatingSystemID": "M00000888",
        "OriginatingSystemName": "Resolver 8 MLS"
      }
    ]
  }
}
```

**Adoption.** On match consensus the originator binds `urn:…9f2c` to its record and stops – no `Create`, and no further activity at all. Adoption changes no shared state, so the thread needs no closing message: the existing ULI already carries its own minting signature ([Section 2.8](#section-28-signing-and-self-verification)) from when it was created, and the ULI registry independently records that the ULI is now in use in the originator's market.

This is the general rule, and it is why the other examples *do* post a closing activity while this one does not: **a node posts an outcome to the thread only when that outcome changes shared ULI state** – a mint, a tombstone, a merge. Reusing an existing ULI is none of those.

### Section 2.12.3: Late-Match Correction

A ULI is minted at consensus, and then a resolver that was slow or briefly offline replies with a *better* match. The network corrects itself: the just-minted ULI is retired in favor of the better one, and anything pointing at the retired ULI is redirected. No record is lost – only the identifier they share changes.

**Thread.** The late reply lands in the same thread, after the `Create`. The originator answers it with a `Tombstone` that redirects the retired ULI to the better one.

| # | Actor | Activity | Description |
| :--- | :--- | :--- | :--- |
| 1 | Provider 1 | Broadcast `Note` "ULI Resolution Request" | The originator holds no ULI for the licensee and asks the network. |
| 2 | Other resolvers | No Match | Replies accumulate to `floor(2/3 × R)`. |
| – | – | No-match consensus | Enough no-match replies to clear the bar; no resolver holds the licensee. |
| 3 | Provider 1 | `Create` ULI `urn:…4a17` | Minted at no-match consensus, without waiting for every resolver. The `Create` is itself a thread message. |
| 4 | Provider 8 | Matches Found → `urn:…9f2c` (late) | A resolver that was slow or offline replies after the mint, with a match that scores above the threshold. |
| 5 | Provider 1 | `Tombstone` `urn:…4a17` → redirect `urn:…9f2c` | Provider 1 holds the just-minted `urn:…4a17`, so it issues the correction. The better match is verifiable – its score is recomputable from the published criteria ([Section 2.9](#section-29-matching-engine-and-scoring)) – so the holder retires and redirects without a fresh consensus round ([Section 2.7](#section-27-uli-creation-and-lifecycle)). Records bound to `urn:…4a17` now resolve to `urn:…9f2c`. Resolvers may keep replying afterward; the thread MAY be removed once consensus is reached ([Section 2.6](#section-26-consensus)). |

Why this is safe to do after the fact:

- The ULI is created at consensus (#3) without waiting for everyone, so the network stays responsive. The trade-off is that a better match can still arrive (#4).
- When it does, that better match MUST trigger tombstone-and-redirect (#5): the correction is mandatory, not optional. Because the match is verifiable ([Section 2.9](#section-29-matching-engine-and-scoring)), the holder applies it directly – no fresh consensus round, unlike the mint at #3 ([Section 2.7](#section-27-uli-creation-and-lifecycle)).
- A tombstone never deletes data. It marks the old ULI as superseded and points to the survivor, so links heal instead of breaking. Each ULI's own signature outlives the thread itself, so it stays verifiable afterward ([Section 2.8](#section-28-signing-and-self-verification)).

The correction (#5) tombstones the `Note` that created the minted ULI and redirects it to the better match. The tombstone's `id` is the created `Note`'s identifier; the retired ULI appears in its `content`, and the redirect target is the `url`:

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Tombstone",
  "id": "https://provider1.example.com/uli/4a17",
  "formerType": "Note",
  "deleted": "2026-03-02T15:04:05Z",
  "attributedTo": "https://provider1.example.com/actor",
  "inReplyTo": "https://provider1.example.com/threads/xyz",
  "content": "ULI Retired: urn:reso:uli:1.0:urn:reso:uoi:1.0:T00000012:M00000123:4a178d3c4e92:<signature>",
  "url": { "type": "Link", "href": "urn:reso:uli:1.0:urn:reso:uoi:1.0:T00000012:M00000888:9f2c2b6e4f1a:<signature>" }
}
```

### Section 2.12.4: Merge of Two Existing ULIs

Sometimes one licensee already carries **two different ULIs** – usually because two systems minted one independently before they ever talked. A merge collapses them into one. It is **not a new operation**: it is an ordinary resolution of the **merged criteria** with the two ULIs being retired **excluded from matching**, followed by retiring both into whatever that resolution returns.

The exclusion is the subtle part. If the two were left in, the merged criteria would simply match them again – they carry that data – and there would be no way to tell whether a clean target exists. So the broadcast asks: *setting aside the two being retired, does any other resolver hold this person?*

- **No other holder** (this example) → mint a neutral third ULI; both originals redirect into it.
- **Some other holder** → adopt that existing ULI as the target instead; both originals redirect to it, nothing new is minted.

Either way **neither original survives** – both are excluded from the match, so a merge never keeps one of the two; it always lands on a new or third-party ULI.

**Thread.** The originator has already learned (from an earlier resolution) that one licensee is held under `urn:…a1b2` (Provider 5) and `urn:…9f2c` (Provider 8). It broadcasts the merged criteria with both excluded, mints the target once no other holder is found, then each holder retires its own ULI into the target. Each retirement is the holder's own act over its own identifier, so no separate consensus ratifies it beyond the no-other-match round that minted the target. Order matters: the target must exist before anything can redirect to it.

| # | Actor | Activity | Description |
| :--- | :--- | :--- | :--- |
| 1 | Provider 1 | Broadcast `Note` – merged criteria, excluding `urn:…a1b2` + `urn:…9f2c` | The originator already knows both are duplicates for this licensee; it asks whether any *other* resolver holds the person. |
| 2 | Other resolvers | No Match | No third party holds the licensee. (Providers 5 and 8 are excluded from this broadcast – they hold the two ULIs being retired.) Replies accumulate to `floor(2/3 × R)`. |
| – | – | No-other-match consensus | The two being retired are the only ULIs for this person – so the target must be freshly minted, not adopted. |
| 3 | Provider 1 | `Create` neutral ULI `urn:…c3d4` | Minted at consensus – the target the originals will redirect to. (Had a third holder matched, the originator would adopt theirs instead of minting.) |
| 4 | Provider 5 | `Tombstone` `urn:…a1b2` → `urn:…c3d4` | The holder of ULI-A retires its own ULI into the target, now that it exists. |
| 5 | Provider 8 | `Tombstone` `urn:…9f2c` → `urn:…c3d4` | The holder of ULI-B does the same – both records relink to `urn:…c3d4`. |

The merged criteria behind the broadcast link are an ordinary licensee subset (as in [Section 2.12.1](#section-2121-new-uli-no-match-found)) – the combined record of the person held under both ULIs:

```json
{
  "@reso.context": "urn:reso:metadata:2.0:resource:member",
  "value": [
    {
      "MemberFullName": "Jane Smith",
      "MemberFirstName": "Jane",
      "MemberMiddleName": "A",
      "MemberLastName": "Smith",
      "MemberType": "REALTOR Salesperson",
      "MemberNationalAssociationId": "999000111",
      "MemberStateLicense": "SL123456",
      "MemberStateLicenseType": "Salesperson",
      "MemberStateLicenseState": "CA",
      "MemberMlsId": "C9012",
      "OfficeName": "Smith Realty Group",
      "OfficeMlsId": "OFF099",
      "SourceSystemID": "M00000123",
      "SourceSystemName": "Source MLS",
      "OriginatingSystemID": "M00000123",
      "OriginatingSystemName": "Originating MLS"
    }
  ]
}
```

The target is minted first; then each holder tombstones its own ULI and redirects it to the target. The `Create`:

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Create",
  "actor": "https://provider1.example.com/actor",
  "object": {
    "type": "Note",
    "id": "https://provider1.example.com/uli/c3d4",
    "attributedTo": "https://provider1.example.com/actor",
    "inReplyTo": "https://provider1.example.com/threads/merge-1",
    "content": "ULI Created (merge target for urn:…a1b2 and urn:…9f2c)",
    "url": { "type": "Link", "href": "https://provider1.example.com/rcf/uli/c3d4", "mediaType": "application/json" }
  }
}
```

Provider 5 retires `urn:…a1b2` and Provider 8 retires `urn:…9f2c`, each tombstoning the `Note` that created its ULI and redirecting to the target:

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Tombstone",
  "id": "https://provider5.example.com/uli/a1b2",
  "formerType": "Note",
  "deleted": "2026-03-02T15:10:00Z",
  "attributedTo": "https://provider5.example.com/actor",
  "inReplyTo": "https://provider1.example.com/threads/merge-1",
  "content": "ULI Retired: urn:reso:uli:1.0:urn:reso:uoi:1.0:T00000012:M00000555:a1b27c5d4a8e:<signature>",
  "url": { "type": "Link", "href": "urn:reso:uli:1.0:urn:reso:uoi:1.0:T00000012:M00000123:c3d46f9a4b3c:<signature>" }
}
```

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Tombstone",
  "id": "https://provider8.example.com/uli/9f2c",
  "formerType": "Note",
  "deleted": "2026-03-02T15:11:00Z",
  "attributedTo": "https://provider8.example.com/actor",
  "inReplyTo": "https://provider1.example.com/threads/merge-1",
  "content": "ULI Retired: urn:reso:uli:1.0:urn:reso:uoi:1.0:T00000012:M00000888:9f2c2b6e4f1a:<signature>",
  "url": { "type": "Link", "href": "urn:reso:uli:1.0:urn:reso:uoi:1.0:T00000012:M00000123:c3d46f9a4b3c:<signature>" }
}
```

### Section 2.12.5: Match Found but Declined on Review

This is the trickiest case, because the network and a human disagree. The math returns a match above the threshold, but a person at the originator recognizes it is the *wrong* person. Rather than adopt it, the originator **declines by rebroadcasting corrected criteria into the same thread**, and the network resolves again – landing on a new ULI (this example) or, if a correct one exists, on that.

The trigger here is a data error, the most common source of a false positive. An MLS onboards a brand-new agent, **Robert Chen Jr.**, by cloning his father **Robert Chen Sr.**'s record as a template – and two fields are never corrected, so Jr.'s record carries Sr.'s **license number and NRDS ID**. Two *primary* identifiers now collide between two genuinely different people, so the match scores well over the bar. Only a human, who knows Jr. is a distinct new licensee, can catch it.

Two things make this case worth spelling out:

- **The decline is a rebroadcast, not a new verb.** Activity Streams 2.0 has no "reject" for this purpose, and this proposal adds none. The originator broadcasts the *corrected* criteria again, in the same thread; that rebroadcast **is** the decline, and it sends resolution back through the ordinary path.
- **Resolution decides the outcome, not the human.** The human can reject the one candidate, but the originator does not mint on its own say-so – the corrected criteria are resolved by the network, and only a no-match consensus mints the new ULI. (Had a *correct* existing ULI surfaced instead, the originator would adopt it.)

**Thread.** The erroneous broadcast draws a high match; the originator declines by rebroadcasting Jr.'s corrected criteria; that resolves to no-match; a fresh ULI is minted – all in the one thread. Sr.'s ULI is never touched: Jr. and Sr. are different people, so this is a clean separation, not a merge.

| # | Actor | Activity | Description |
| :--- | :--- | :--- | :--- |
| 1 | Provider 1 | Broadcast `Note` – Jr.'s criteria, carrying Sr.'s license + NRDS | The MLS resolves its new agent; the record unknowingly holds two of Sr.'s primary identifiers. |
| 2 | Provider 8 | Matches Found → `urn:…9f2c` (Sr.'s ULI, score 1380) | Provider 8 holds Sr. under `urn:…9f2c`; the two shared primaries push the score over 1300. Same reply shape as [Section 2.12.2](#section-2122-existing-uli-found-and-adopted). |
| 3 | Other resolvers | No Match | They do not hold the person; replies still count toward consensus. |
| – | – | Match consensus → candidate `urn:…9f2c` | The network's answer, purely on the numbers. |
| – | Provider 1 (human) | Reviews the candidate and rejects it | Off the wire. Jr. is a distinct, newly licensed agent; the matching license and NRDS are a clone artifact – the false positive the math could not catch. |
| 4 | Provider 1 | **Rebroadcast** – Jr.'s *corrected* criteria, same thread | The decline itself: reposting corrected criteria reopens resolution. The visible signal, no separate verb. |
| 5 | Other resolvers | No Match | Against the corrected criteria, nobody holds Jr. |
| – | – | No-match consensus | The corrected criteria match no one; the licensee is genuinely new. |
| 6 | Provider 1 | `Create` new ULI `urn:…b8d6` (Jr.) | Minted at no-match consensus – properly gated, not on the human's say-so. Sr.'s `urn:…9f2c` is left untouched. |

The first broadcast carries Jr.'s record as onboarded – note the two inherited primaries (`MemberNationalAssociationId` and `MemberStateLicense` are Sr.'s):

```json
{
  "@reso.context": "urn:reso:metadata:2.0:resource:member",
  "value": [
    {
      "MemberFullName": "Robert Chen Jr.",
      "MemberFirstName": "Robert",
      "MemberLastName": "Chen",
      "MemberType": "REALTOR Salesperson",
      "MemberNationalAssociationId": "555000222",
      "MemberStateLicense": "SL778899",
      "MemberStateLicenseType": "Salesperson",
      "MemberStateLicenseState": "CA",
      "MemberMlsId": "J2001",
      "OfficeName": "Chen Realty",
      "OfficeMlsId": "OFF300",
      "SourceSystemID": "M00000123",
      "SourceSystemName": "Source MLS",
      "OriginatingSystemID": "M00000123",
      "OriginatingSystemName": "Originating MLS"
    }
  ]
}
```

The decline rebroadcasts the **corrected** criteria – the same record with Jr.'s own identifiers (`MemberNationalAssociationId` and `MemberStateLicense` now his):

```json
{
  "@reso.context": "urn:reso:metadata:2.0:resource:member",
  "value": [
    {
      "MemberFullName": "Robert Chen Jr.",
      "MemberFirstName": "Robert",
      "MemberLastName": "Chen",
      "MemberType": "REALTOR Salesperson",
      "MemberNationalAssociationId": "555000999",
      "MemberStateLicense": "SL445566",
      "MemberStateLicenseType": "Salesperson",
      "MemberStateLicenseState": "CA",
      "MemberMlsId": "J2001",
      "OfficeName": "Chen Realty",
      "OfficeMlsId": "OFF300",
      "SourceSystemID": "M00000123",
      "SourceSystemName": "Source MLS",
      "OriginatingSystemID": "M00000123",
      "OriginatingSystemName": "Originating MLS"
    }
  ]
}
```

Resolving the corrected criteria yields no match, so the originator mints Jr.'s own ULI:

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Create",
  "actor": "https://provider1.example.com/actor",
  "object": {
    "type": "Note",
    "id": "https://provider1.example.com/uli/b8d6",
    "attributedTo": "https://provider1.example.com/actor",
    "inReplyTo": "https://provider1.example.com/threads/xyz",
    "content": "ULI Created",
    "url": { "type": "Link", "href": "https://provider1.example.com/rcf/uli/b8d6", "mediaType": "application/json" }
  }
}
```

<br />

# Section 3: Certification

Certification proves three things – identity, protocol and matching conformance, and the trust guarantees – in two phases: a one-time **onboarding gate** ([Section 3.1](#section-31-onboarding-gate)) and **continuous conformance** ([Section 3.2](#section-32-continuous-conformance)). The rules derive from the Section 2 requirements. Certification is per-component: each of the three layers (matching backend, review user interface, resolver network) is certified against its interface, so any vendor may supply any piece.

RESO will validate the following during certification:

**Identity and signing**
* The candidate MUST register its Unique Organization Identifier and Ed25519 public key and sign a RESO-issued challenge that verifies against that key ([Section 2.8](#section-28-signing-and-self-verification), [Section 2.10](#section-210-resolver-discovery-and-the-governance-feed)).
* A ULI the candidate mints MUST carry a signature that verifies against its registered key over the exact bytes defined in [Section 2.8](#section-28-signing-and-self-verification), with the prefix in lowercase.
* A ULI the candidate stores, transmits or redirects to MUST be byte-identical to the ULI as minted; a case-folded or otherwise normalized ULI fails ([Section 2.8](#section-28-signing-and-self-verification)).

**Protocol**
* Every activity the candidate posts MUST use only Activity Streams 2.0 vocabulary and MUST NOT carry custom JSON-LD terms ([Section 2.2](#section-22-activitypub-usage)).
* An activity the candidate posts MUST NOT carry licensee data or matching detail; that content MUST be reachable only through the OAuth2-protected RCF link the activity references, and the payload behind the link MUST validate as RESO Common Format against the Member Resource of the declared Data Dictionary version ([Section 2.2](#section-22-activitypub-usage), [Section 2.3](#section-23-uli-fields-and-payload)).
* The candidate MUST complete the sequences of [Section 2.12](#section-212-worked-examples) against the sandbox resolver: broadcast, no-match reply, match reply, create, tombstone and redirect, merge and decline ([Section 2.4](#section-24-broadcast) through [Section 2.7](#section-27-uli-creation-and-lifecycle)).
* The candidate MUST NOT post a `Create` for a new ULI before a no-match consensus is reached in the thread, and MUST NOT broadcast a record whose present factors cannot reach the match threshold ([Section 2.4](#section-24-broadcast), [Section 2.7](#section-27-uli-creation-and-lifecycle)).
* When ULI-relevant fields of a record holding a ULI change, the candidate MUST rebroadcast that record ([Section 2.4](#section-24-broadcast)).
* When addressed by a `Mention`, the candidate MUST reply; it MUST NOT reply with a match whose score is below the match threshold; and every match reply MUST carry its score in the referenced RCF payload ([Section 2.5](#section-25-resolution-and-reply)).
* On resume after being offline, the candidate MUST reply to every open thread it was tagged in, and MUST rebroadcast the original criteria when it holds a match for a thread that has already been resolved and removed ([Section 2.6](#section-26-consensus)).
* When a better match than a ULI the candidate holds is surfaced, the candidate MUST either tombstone that ULI and redirect it to the better one or, where another holder has already issued the tombstone, follow that redirect and relink its records ([Section 2.7](#section-27-uli-creation-and-lifecycle)).

**Matching**
* The candidate's engine MUST reproduce the reference oracle's score exactly on every published test vector, scoring correlated identifiers as a single cluster; its search is measured for recall and precision across the sandbox data set ([Section 2.9](#section-29-matching-engine-and-scoring), [Section 3.1](#section-31-onboarding-gate)).
* When a weight-set or threshold change is posted to the governance feed, the candidate MUST rerun matching locally and broadcast the resulting differences ([Section 2.9](#section-29-matching-engine-and-scoring)).

**Data isolation and authorization**
* Access to the candidate's node MUST be authenticated, and every RCF link the candidate publishes MUST refuse an unauthenticated dereference ([Section 2.3](#section-23-uli-fields-and-payload), [Section 2.11](#section-211-authentication-and-authorization)).
* The set of resolvers the candidate authorizes MUST include every resolver on its mandatory list plus enough additional resolvers to reach `floor(2/3 × R)` ([Section 2.1](#section-21-participation-and-data-isolation)).

## Section 3.1: Onboarding Gate

A node MUST pass the onboarding gate before joining the production network. The gate runs in a sandbox – the RESO ULI reference server and resolver node – which serves as both the scoring **oracle** and the protocol **peer**. In the sandbox, and only there, the candidate grants RESO access to its data set for certification; this access is scoped to the certification phase and ends at the production boundary, where data isolation ([Section 2.1](#section-21-participation-and-data-isolation)) applies in full.

The gate checks:

- **Identity** – the candidate registers its Unique Organization Identifier and Ed25519 public key, signs a challenge, and RESO verifies it. This is the binding that ULI signing ([Section 2.8](#section-28-signing-and-self-verification)) depends on.
- **Protocol conformance** – the candidate completes the [Section 2.12](#section-212-worked-examples) sequence against the sandbox resolver (broadcast, no-match reply, match reply, create, tombstone and redirect, merge and decline), using standard ActivityPub with data behind OAuth2-protected RCF links.
- **Scoring and search parity** – RESO exhaustively exercises the candidate's engine: every record is scored against the reference oracle (scoring), and the candidate's index is queried across the data set (search, measuring recall and precision). Scoring is deterministic under the adopted weight set, so parity MUST be exact; any divergence is a defect to resolve before joining.
- **Signing** – the candidate mints a ULI and the signature it carries verifies against the candidate's registered Ed25519 key ([Section 2.8](#section-28-signing-and-self-verification)).

The certification harness – sampling records, feeding them to the reference engine and diffing the results – is provided in RESO Tools. Convergence across independent implementations is itself a test of this specification's precision.

## Section 3.2: Continuous Conformance

Once on the network, a node is monitored continuously rather than recertified periodically; certification is an ongoing state. The check rides the daily liveness heartbeat: when RESO pings a node that has had activity since the last check, it also samples up to five of the most recent threads the node participated in (by its handle) and verifies it **participated correctly** – well-formed activities, valid signatures, correct consensus and lifecycle behavior. The sample is behavioral and observable from the public threads without data access; deeper checks may run when something looks off. The private below-threshold direct-message path ([Section 2.4](#section-24-broadcast)) is not passively auditable and is covered by the sandbox gate rather than production sampling.

A conformance failure removes the node **immediately**: RESO marks it noncompliant on the governance feed ([Section 2.10](#section-210-resolver-discovery-and-the-governance-feed)), dropping it from every node's `R` on the next delta. The node's specific violation – what it failed and how to remediate – is delivered privately to its inbox (a direct message); the public feed carries only the status, the direct message carries the detail.

Marking a node noncompliant invalidates its **pending** sign-offs: each open thread is tallied again over currently compliant sign-offs – the originator tallies its own affected threads again on the decertification event and posts an in-thread follow-up where a second resolution results – and any thread that falls below `floor(2/3 × R)` resolves again through self-healing. Already-minted ULIs are immutable, each carrying its minter's signature; the mint is the finalization boundary.

## Section 3.3: Reference Correctness

The reference engine is the certification oracle, so it MUST be validated before it certifies any node: the reference MUST pass its own self-test (the reference engine against known-correct resolutions), so that a defect in the oracle cannot silently pass a nonconforming node or fail a conforming one.

<br />

# Section 4: Contributors

This document was written by [Joshua Darnell](mailto:josh@reso.org).

The ULI Resolution Protocol was developed by the RESO ULI Workgroup and its ActivityPub Subgroup. Additional contributors will be acknowledged as the proposal advances.

<br />

# Section 5: References

Please see the following references for more information regarding topics covered in this document:
* [W3C ActivityPub](https://www.w3.org/TR/activitypub/)
* [W3C Activity Streams 2.0 (Core)](https://www.w3.org/TR/activitystreams-core/)
* [W3C Activity Vocabulary](https://www.w3.org/TR/activitystreams-vocabulary/)
* [RESO Common Format (RCP-25)](https://transport.reso.org/proposals/reso-common-format/)
* [RESO Web API Core 2.1.0, Section 2.9 Security](https://transport.reso.org/proposals/web-api-core/)
* [RESO Data Dictionary 2.0 – Member Resource](https://dd.reso.org/DD2.0/Member/)
* [RESO Data Dictionary 2.0 – UniqueLicenseeIdentifier](https://dd.reso.org/DD2.0/Member/UniqueLicenseeIdentifier/)
* [RESO Organization and System Identifiers (RCP-55)](https://github.com/RESOStandards/transport/blob/46619fdd2d383938ef86c6efff0f01e82c19d6c0/proposals/org-system-identifiers.md)
* [RESO ULI Resolution Protocol discussion](https://github.com/RESOStandards/transport/discussions/163)
* [RESO Listing Advertisement discussion (ActivityPub examples)](https://github.com/RESOStandards/transport/discussions/162)
* RESO ActivityPub Subgroup, 2025-08-01 agenda (consensus mechanism and worked example)
* [RFC 8032 – Edwards-Curve Digital Signature Algorithm (EdDSA)](https://www.rfc-editor.org/rfc/rfc8032)
* [RFC 4648, Section 5 – Base 64 Encoding with URL and Filename Safe Alphabet](https://www.rfc-editor.org/rfc/rfc4648#section-5)
* [RFC 8141 – Uniform Resource Names (URNs)](https://www.rfc-editor.org/rfc/rfc8141)
* [FIPS PUB 180-4 – Secure Hash Standard (SHA-256)](https://doi.org/10.6028/NIST.FIPS.180-4)
* [RFC 6962 – Certificate Transparency (Merkle tree construction)](https://www.rfc-editor.org/rfc/rfc6962)
* Technical and design-rationale references: see [Section 6](#section-6-appendices).

<br />

# Section 6: Appendices

## Design rationale

**The score is a credit-score-style value, not a probability.** Match strength is an arbitrary integer on a fixed scale with threshold bands (reject, review, possible match), like a credit score. This is deliberate: a 0–100 or percentage scale would invite reading the number as a calibrated probability, which it is not. A true probability would require calibration against labeled resolutions; until that data exists, the number is a defensible *threshold*, not a chance.

**The weights are bits of identifying information.** Each factor's weight reflects how much it narrows the population – formally, the self-information `log₂(1/frequency)` of the matched value. A national identifier narrows to one person (many bits); a common surname narrows very little (few bits). This is the same quantity as the inverse document frequency (IDF) the matching engine already uses. Two consequences fall out rather than being bolted on: common values such as "John Smith" cannot clear the threshold on their own, and the threshold reads as "enough bits to single out one licensee from the population."

**Correlated identifiers are scored once.** Office ID and office name are two views of one fact ("same brokerage"). Summing them double-counts one signal, so a cluster is scored as its strongest matched member, not the sum ([Section 2.9](#section-29-matching-engine-and-scoring)).

**`floor`, not `ceiling`.** The consensus count is `floor(2/3 × R)` ([Section 2.6](#section-26-consensus)). The Workgroup adopted the more lenient bar; the 2025-08-01 subgroup proposal used `ceiling`.

**Where the scoring goes next – count-based axes.** Counting occurrences turns flat weights into measured bits along several independent axes: value rarity within a field; how many *providers* (not records) carry a value; and co-occurrence rarity of combinations. Bits add cleanly only across *independent* axes – an axis earns its place only if it carries signal the others do not (measurable via the co-occurrence axis). Term statistics can be shared as aggregate counts without exposing records; a small-count floor (or noise) is required, because the rarest values carry the most information and are therefore the most identifying when their counts are published.

**Shared term statistics.** The initial weight source is a published static frequency table (public name-frequency data and the standard vocabularies), which makes scores comparable across nodes by construction and requires no cross-organization exchange. A Phase 2 option computes a global IDF from per-term document-frequency counts published by each node (documents stay local; only counts are shared) – more population-accurate, at the cost of a coordination and small-count-leakage surface.

**Self-verifying identifiers, not a separate proof.** A ULI carries an Ed25519 signature by its minting organization over the identifier itself, so it verifies as network-minted from the identifier alone – offline, against the organization's certified public key, with no registry and no call back to the minter. The proof rides *in* the identifier because that is the one thing that always travels: a bare ULI copied downstream arrives with no RCF payload, and a registry or minter lookup may be unreachable, so anything kept alongside the identifier rather than inside it cannot be relied on at verification time. The signature proves origin only – that a certified organization minted the identifier – which is what keeps fabricated or mistaken ULIs out of the graph; consensus compliance and key compromise are handled by certification and monitoring ([Section 3](#section-3-certification)), not by the identifier. A heavier alternative – per-organization signed attestations committed in an [RFC 6962](https://www.rfc-editor.org/rfc/rfc6962) Merkle tree over a keyed licensee reference – would prove the full consensus rather than just origin, but on a closed, certified network with a trusted governance anchor a single minter signature delivers the property that matters, anti-fabrication, at a fraction of the implementation cost; the lighter construction is chosen here.

## Design rationale references

- Okapi BM25 – https://en.wikipedia.org/wiki/Okapi_BM25 ; Robertson & Zaragoza, "The Probabilistic Relevance Framework: BM25 and Beyond" (2009)
- tf–idf – https://en.wikipedia.org/wiki/Tf%E2%80%93idf
- Information content (self-information) – https://en.wikipedia.org/wiki/Information_content
- Entropy (information theory) – https://en.wikipedia.org/wiki/Entropy_(information_theory)
- Shannon, "A Mathematical Theory of Communication" (1948); Cover & Thomas, *Elements of Information Theory*
- Name-frequency data – U.S. Census Bureau, "Frequently Occurring Surnames"; U.S. Social Security Administration given-name frequency data
- Distributed information retrieval / global IDF – Callan, "Distributed Information Retrieval" (2000)

## Proposed Data Dictionary elements

This proposal introduces no new field, lookup or resource. The identifier it resolves is carried in the Member Resource's existing `UniqueLicenseeIdentifier` field (Data Dictionary 2.0 and later), whose current definition limits it to "a licensed real estate agent." This proposal asks for that definition to be broadened to the scope of [Section 1](#section-1-purpose):

| Resource | Field | Simple Data Type | Nullable | Lookup | Definition |
| :--- | :--- | :--- | :--- | :--- | :--- |
| Member | UniqueLicenseeIdentifier | String | Yes | | The Unique Licensee Identifier (ULI) of the member, a RESO identifier that links the records of one licensed real estate professional across systems and markets. |

The seventeen matching-input fields of [Section 2.3](#section-23-uli-fields-and-payload) are existing Data Dictionary 2.0 fields and are not changed.

| Lookup | Lookup Value | Definition |
| :--- | :--- | :--- |
| None | | |

<br />

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO Transport](mailto:transport@reso.org) with questions about this proposal, or [RESO developer support](mailto:dev@reso.org) with specific development questions.
