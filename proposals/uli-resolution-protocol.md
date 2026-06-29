# RESO ULI Resolution Protocol

| **RCP**           | 54                                                                  |
| :---------------- | :------------------------------------------------------------------ |
| **Version**       | **0.1.0 (draft)**                                                   |
| **Authors**       | [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org)) <br /> RESO ULI Workgroup |
| **Specification** | [**LINK TO RCP**](#) (ticket pending)                               |
| **Status**        | IN PROGRESS                                                         |
| **Date Ratified** | TBD                                                                 |
| **Dependencies**  | [W3C ActivityPub](https://www.w3.org/TR/activitypub/) <br /> [RESO Common Format](./reso-common-format.md) <br /> RESO Data Dictionary (ULI fields) <br /> OAuth2 |
| **Related Links** | [ULI Resolution Protocol discussion](https://github.com/RESOStandards/transport/discussions/163) <br /> [ActivityPub examples](https://github.com/RESOStandards/transport/discussions/162) <br /> RESO ActivityPub Subgroup, 2025-08-01 agenda |

The keywords "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY" and "OPTIONAL" in this document are to be interpreted as described in [RFC 2119](https://www.ietf.org/rfc/rfc2119.txt).

<br />

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

<br />

# Table of Contents

- [Summary of Changes](#summary-of-changes)
- [Introduction](#introduction)
- [Section 1: Purpose](#section-1-purpose)
- [Section 2: Specification](#section-2-specification)
- [Section 3: Certification](#section-3-certification)
- [Section 4: Contributors](#section-4-contributors)
- [Section 5: References](#section-5-references)
- [Section 6: Appendices](#section-6-appendices)
- [Section 7: License](#section-7-license)

<br />

# Summary of Changes

Initial draft. Establishes the ULI Resolution Protocol: a threaded, event-based resolution over W3C ActivityPub across a network of certified resolvers, with no licensee data stored on or shared through the network. Significant items:

- Participation with pre-arranged authorization and no central store (§2.1).
- Identifiability gate before broadcast (§2.4).
- Consensus at `floor(2/3 × R)` with optional mandatory resolvers (§2.6).
- A ULI lifecycle built from a small set of primitives – consensus-gated mint, silent local adopt, holder-initiated tombstone/redirect, merge and decline-by-re-broadcast – with self-healing correction (§2.7, §2.12).
- Self-verifying identifiers: each ULI carries an Ed25519 signature by its minting org, so it can be checked as network-minted offline, with no registry (§2.8).
- Credit-score-style weighted matching with a network-adopted weight set and threshold (§2.9).
- Certification: a rigorous sandbox onboarding gate (exhaustive scoring and search parity against the reference oracle) plus continuous conformance monitoring (§3).
- Governance feed: a public, PII-free ActivityPub feed every resolver follows, carrying membership and parameter events; `R` is the certified ∩ compliant ∩ active view of it (§2.10).

<br />

# Introduction

The Unique Licensee Identifier (ULI) gives every licensed real estate professional a single identifier that links their records across MLSs and markets and de-duplicates them. The ULI links established records rather than replacing their keys, and it is eventually consistent: a ULI may be superseded by a merge or redirect as the network resolves further, and the linked records re-point to the replacement rather than disappearing.

This proposal specifies a **resolution protocol**: how a participating system broadcasts a licensee, how a network of certified resolvers replies with a match or no match, how consensus mints or adopts a ULI and how the resolution is proven. Resolution runs over the W3C **ActivityPub** standard as a threaded, event-based conversation.

The protocol keeps each participant in control of its data: a provider searches its own data, replies match or no match and shares its licensee subset only through pre-arranged authorization to the resolvers it relies on (§2.1). The network itself stores nothing. Consensus over replies, rather than a central pool of data, is what resolves a licensee, which makes the network adoptable among competitors even though the ULI fields are neutral and widely available. Match strength is reported as a **credit-score-style score**: an arbitrary point scale with thresholds, deliberately not a probability.

# Section 1: Purpose

A licensed real estate professional has no industry-wide identifier. The same person appears as separate, unlinked records across MLSs, associations and systems, under different MLS IDs and different source systems, so the industry cannot reliably tell that two records are one person, follow a professional across markets or de-duplicate counts. The nearest existing identifier is the National Association of REALTORS® (NAR) member ID carried in `MemberNationalAssociationId`. It covers only NAR members, so non-NAR licensees have none, and it carries duplication of its own; no existing identifier can serve as the universal one.

The **Unique Licensee Identifier (ULI)** fills that gap: a single identifier that links a licensed professional's records across markets and de-duplicates them. Its primary audience is licensed agents and brokers, and it extends to any licensed practitioner in the transaction, for example appraisers and photographers. The ULI does not replace existing identifiers or record keys; it links established records, resolving across the NAR member ID, state license, name and office together as weighted inputs (§2.9) so no single identifier has to be present or unique on its own.

A ULI is a linking identifier, not a primary key, and it is eventually consistent. As more of the network resolves a licensee, a ULI may be superseded by a merge or redirect; the linked records re-point to the replacement rather than disappearing, and a record always resolves to its current ULI by following the redirect chain.

This proposal specifies the **resolution protocol**: how a participating system broadcasts a licensee, how a network of certified resolvers replies with a match or no match, how consensus mints or adopts a ULI and how the resolution is proven (§2). It is built for three properties:

- **Decentralized and interoperable.** The matching backend, review UI and resolver network are independent layers; any vendor may supply any piece.
- **Privacy-preserving.** No licensee data is stored on the network; a participant shares only the subset of PII-safe ULI fields it chooses, and only with the resolvers it authorizes (§2.1, §2.3).
- **Self-correcting.** ULIs are created at consensus and reconciled afterward, so late information merges or redirects rather than fragments (§2.7).

Resolved ULIs accumulate in a **ULI registry** that records each observed ULI and the markets it appears in; this protocol is how those ULIs are created and maintained over time.

# Section 2: Specification

The ULI Resolution Protocol uses W3C ActivityPub to make licensee resolution a threaded, event-based conversation across a network of certified resolvers. No licensee data is stored on the network; data is referenced by OAuth2-protected links to RESO Common Format (RCF) payloads. The protocol's three layers (matching backend, review UI and resolver network) are independent: any conformant implementation of one MUST interoperate with any conformant implementation of another.

## 2.1: Participation and data isolation
A provider participates by searching its **own** data and replying with a match or no match. An originator references its shared ULI subset through an OAuth2-protected link (§2.3), and a resolver matches against the broadcast by dereferencing that link, which requires authorization. Access is therefore **pre-arranged**: the originator grants read permission to the resolvers it relies on, and that set MUST cover at least its mandatory resolvers plus enough additional resolvers to reach consensus (§2.6). Authorizing the whole network is preferable but not required. The network itself MUST NOT store licensee data: the shared subset lives only behind the originator's OAuth2 links and is read only by authorized resolvers.

No provider is forced to open its data to the entire network or to any particular competitor. It controls which resolvers it authorizes, shares only the subset of ULI fields it chooses (§2.3) and grants no more access than resolution requires. Resolution is reached by consensus over replies (§2.6), not by pooling data into a central store, so the protocol works among participants, competitors included, without anyone surrendering control of their data or trusting a shared repository.

## 2.2: ActivityPub usage
Implementations MUST use the standard [ActivityStreams 2.0 vocabulary](https://www.w3.org/TR/activitystreams-vocabulary/), for example [`Note`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-note), [`Create`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-create) and [`Tombstone`](https://www.w3.org/TR/activitystreams-vocabulary/#dfn-tombstone). Implementations MUST NOT extend [ActivityPub](https://www.w3.org/TR/activitypub/)'s JSON-LD with custom terms. Any licensee data or matching detail the protocol requires (for example a match score or a candidate record) MUST be carried in the RCF payload that the activity references, not in the ActivityPub object. A ULI is an identifier rather than data, so a ULI URN – including a tombstone's redirect target – MAY appear directly in the activity, as it does in a thread reference.

## 2.3: ULI fields and payload
The licensee data exchanged is drawn from a PII-safe set of ULI fields, all carried on the Member resource. Each field links to its Data Dictionary definition; the *Role* column summarizes how the matching engine (§2.9) uses it.

| Field | Definition | Role in resolution | Markets (of 353) |
| :--- | :--- | :--- | ---: |
| [`MemberFullName`](https://dd.reso.org/DD2.0/Member/MemberFullName/) | The first, middle and last name of the member, or an alternate full name. | Primary identifier (Full Name, 700). | 323 |
| [`MemberFirstName`](https://dd.reso.org/DD2.0/Member/MemberFirstName/) | The member's first name. | Supporting term; component of Full Name. | 345 |
| [`MemberMiddleName`](https://dd.reso.org/DD2.0/Member/MemberMiddleName/) | The member's middle name. | Supporting term. | 270 |
| [`MemberLastName`](https://dd.reso.org/DD2.0/Member/MemberLastName/) | The member's last name. | Supporting term; component of Full Name and Nickname + Last. | 344 |
| [`MemberNickname`](https://dd.reso.org/DD2.0/Member/MemberNickname/) | An alternate name the member uses, usually for the first name. | Primary identifier as Nickname + Last Name (650); also a supporting term. | 94 |
| [`MemberType`](https://dd.reso.org/DD2.0/Member/MemberType/) | The type of member (Agent, Broker, Appraiser, and so on). | Context; not scored. | 307 |
| [`MemberNationalAssociationId`](https://dd.reso.org/DD2.0/Member/MemberNationalAssociationId/) | The member's national association ID (in the U.S., the NRDS number). | Strongest primary identifier (800). | 319 |
| [`MemberStateLicense`](https://dd.reso.org/DD2.0/Member/MemberStateLicense/) | The member's license number, or numbers separated by a comma. | Primary identifier with State (600), or with State + Type (625). | 323 |
| [`MemberStateLicenseState`](https://dd.reso.org/DD2.0/Member/MemberStateLicenseState/) | The state in which the member is licensed. | Pairs with the license number to form a primary identifier. | 52 |
| [`MemberStateLicenseType`](https://dd.reso.org/DD2.0/Member/MemberStateLicenseType/) | The member's license type. | Strengthens a license match (Number + State + Type, 625); the sub-type alone is a supporting term. | 9 |
| [`MemberMlsId`](https://dd.reso.org/DD2.0/Member/MemberMlsId/) | The local, well-known member identifier; may not be unique across systems. | Context; not a scored identifier. | 337 |
| [`OfficeMlsId`](https://dd.reso.org/DD2.0/Member/OfficeMlsId/) | The local, well-known office identifier; may not be unique across systems. | Secondary identifier (Office ID, 100); scored once within the office cluster. | 329 |
| [`OfficeName`](https://dd.reso.org/DD2.0/Member/OfficeName/) | The legal name of the brokerage. | Secondary identifier (Office Name, 80); scored once within the office cluster. | 256 |
| [`SourceSystemID`](https://dd.reso.org/DD2.0/Member/SourceSystemID/) | OUID of the immediate (source) record provider. | Provenance. | 208 |
| [`SourceSystemName`](https://dd.reso.org/DD2.0/Member/SourceSystemName/) | Name of the immediate record provider. | Provenance. | 144 |
| [`OriginatingSystemID`](https://dd.reso.org/DD2.0/Member/OriginatingSystemID/) | OUID of the authoritative originating system. | Provenance. | 253 |
| [`OriginatingSystemName`](https://dd.reso.org/DD2.0/Member/OriginatingSystemName/) | Name of the authoritative originating system, usually the MLS. | Provenance. | 323 |

The *Markets* column counts the reporting markets that publish each field, of 353 in the RESO Data Dictionary 1.7 Industry Aggregates (December 2025); adoption is uneven, from near-universal names and identifiers down to a rarely broken-out license state and type.

A node MAY share any subset of these fields; the matching engine (§2.9) scores on whatever factors are present, so withholding fields lowers a node's own match strength but does not break resolution. The shared subset MUST be expressed in RESO Common Format and MUST be retrieved over an OAuth2-protected link rather than embedded in the ActivityPub object. A ULI is a self-verifying URN of the form `urn:reso:uli:1.0:<uoi>:<nonce>:<signature>`, carrying a signature from its minting org (§2.8).

## 2.4: Broadcast
Before resolving, a node MUST search its own data first. **A node MUST NOT create a ULI on its own:** every ULI is created only after the network confirms, through broadcast and consensus, that no existing ULI matches the licensee (§2.6, §2.7). Two cases drive a broadcast:

- **New licensee.** The node holds no ULI and its local search finds none. It broadcasts to resolve; on a no-match consensus it mints the ULI and broadcasts the creation.
- **Changed data.** The node already holds a ULI but ULI-relevant fields have changed. It re-runs its local search against the new data and MUST broadcast the licensee request so the network re-resolves; a change can surface a match or a merge the old data did not.

A node holding a ULI MAY also rebroadcast at any time with the original criteria.

A node MUST NOT broadcast a record that does not clear the **identifiability threshold**: the record's own present factors MUST score highly enough that a full match could reach the match threshold (§2.9) – in practice, at least two primary identifiers, since a single primary cannot reach the threshold alone. This keeps under-identified records (for example, a common name alone) off the broadcast path. A below-threshold match is handled out of band: a resolver MAY contact the originator privately with a direct message to its inbox rather than reply in the public thread. The detailed coordinated-resolution behavior is a placeholder, not yet specified.

A broadcast is one ActivityPub `Note` per member, addressed to the shared certified-resolver collection (`to: https://reso.org/resolvers`, which every resolver follows, §2.10), and referencing the shared ULI subset by an OAuth2-protected RCF link. Any resolver the originator requires (its mandatory list, §2.6) is additionally named with an ActivityPub `Mention` – a `Mention` is what makes a resolver mandatory for that broadcast.

Each thread is a transaction for the licensee and is ephemeral. Resolvers SHOULD respond in a timely manner, because messages MAY be deleted at any time (an ActivityPub `Delete`, which also satisfies PII removal). A transaction MAY complete in seconds when nodes respond quickly, or over several minutes for a fuller conversation.

## 2.5: Resolution and reply
An **active** resolver SHOULD reply in the thread with either a potential match, carrying an OAuth2-protected link to its candidate ULI's RCF data (§2.11), or a no-match. A no-match is a real contribution: it is what lets consensus (§2.6) complete, so silence and "no match" are not the same thing. A resolver named on the originator's mandatory list (§2.6), addressed by an ActivityPub `Mention`, MUST reply. A resolver MUST NOT reply with a match unless the match score clears the match threshold (§2.9). A match reply MUST include its score in the referenced RCF payload, so that consensus and later correction (§2.7) can rank competing matches. A resolver that chronically fails to reply is not penalized as noncompliant; it simply trends to **inactive** (§2.10) and drops out of `R`, so persistent silence lowers the bar rather than stalling it. Matching is performed by the shared matching engine (§2.9).

## 2.6: Consensus
Consensus requires two independent conditions: at least `floor(2/3 × R)` resolvers have replied in the thread, **and** every resolver on the requesting node's mandatory list is among those replies. The mandatory resolvers are always a subset of `R` and count toward the `floor(2/3 × R)`; the remainder is filled by non-mandatory replies. The mandatory condition is absolute: reaching the count with non-mandatory replies alone is not consensus while a mandatory resolver has not replied. With no mandatory list, consensus is the simple count. (`floor`, not `ceiling`, is deliberate: the WG adopted the more lenient bar; the 2025-08-01 subgroup proposal used `ceiling`.)

Each node **chooses its own mandatory list** – the resolvers it names with a `Mention` (§2.4) – and is responsible for choosing them correctly. The network does not impose a mandatory set: an originator requires whatever resolvers it trusts to hold an authoritative record – an affiliated body, a system covering the licensee's market or a specialized verification service. Under-naming an authoritative resolver risks a duplicate that later self-heals, and is the kind of thing the audit (§3) surfaces.

`R` is the thread's resolver pool, derived from RESO's governance feed (§2.10) and **pinned at broadcast** to the feed position the broadcast references, so every participant computes the same `R`. It counts all certified, compliant, active resolvers, online or offline. `R` is fixed for the thread except that it **shrinks** if a member is pruned (marked inactive or noncompliant) mid-thread, lowering the bar so a departed node cannot stall the thread. Because the mandatory list is a subset of `R`, a pruned member also leaves the mandatory requirement, so an inactive or noncompliant mandatory resolver cannot block a resolution.

Replies accumulate over time: online resolvers reply at once, and an offline resolver MUST, on resume, drain its inbox and reply to every open thread it was tagged in. There is no fixed timeout – a thread lives until consensus, after which resolvers MAY remove it. A resolver that holds a match for a thread already resolved and gone MUST rebroadcast the original criteria so the late match can still surface (§2.7).

Consensus is participation-based: once the count is met with any mandatory sign-offs present, the originating node selects the outcome – adopt the highest-scoring match over threshold, else mint a new ULI (§2.7) – and self-healing reconciles any later, better match.

## 2.7: ULI creation and lifecycle
### Mint

ULIs are created first, but never unilaterally: a new ULI is minted only on a no-match consensus (§2.6), and the originating provider then posts the created ULI to the thread. While resolution is in flight a provider MAY tag its records with a stable local **placeholder** (a local handle, not a ULI) so that binding the resolved ULI updates one pointer rather than every record. The mint is the consensus event, not a local decision.

### Adopt

When consensus lands on an existing match instead, no ULI is minted: the originating provider adopts that ULI and binds its records to it **locally**. Adoption posts nothing further to the thread, because it changes no shared state – the ULI already exists, carrying its own minting signature (§2.8), and the resolver registry (§2.10) observes the ULI's new usage. As a general rule, a node posts an outcome to the thread only when that outcome changes shared ULI state: a mint, a tombstone or a merge.

### Tombstone and redirect

A resolver MAY surface a better match – a higher score – after a ULI has been minted. This triggers a correction: the superseded ULI MUST be tombstoned (ActivityPub `Tombstone`) and redirected to the better one. A supersession does not need a fresh consensus round the way a mint does: a mint rests on an *absence* of matches, which only the network's replies can establish, whereas a better match is a **present, verifiable** fact – its score is independently recomputable by any participant from the published criteria (§2.9). So the gate is lighter: the correction commits once a **holder** of the superseded ULI acknowledges a better match that clears the threshold, by issuing the tombstone. Any holder MAY initiate it; an offline holder neither blocks the redirect nor must ratify it on return – on return it simply follows the redirect, relinking its records to the successor.

Because the ULI is a linking identifier rather than a primary key, a tombstone never removes the underlying records: they relink to the surviving ULI, so resolution is eventually consistent and continuity is preserved by the redirect chain. A node that ignores a redirect it has seen creates only a local divergence, which heals on the next resolution touching that licensee.

### Merge

When one licensee is found under two ULIs, the two are collapsed into one. A merge is not a distinct operation but a **composition** of the primitives above: the originator broadcasts the **merged criteria with the two ULIs being retired excluded from matching** – asking whether any *other* resolver holds the person – and resolves it normally. A no-other-match consensus mints a neutral new ULI; an other-match adopts that existing ULI. The target is established **first**; then each holder tombstones its own retired ULI and redirects it to the target – retiring one's own identifier into an established target needs no separate round beyond the consensus that minted it. Both originals are excluded from matching, so a merge never keeps one of the two – it always lands on a new or third-party ULI. §2.12 works through this sequence.

### Decline

A match may clear the score threshold yet be wrong – two different licensees who share enough identifying data, often because of a data error. The originator MAY decline a candidate on human review. A decline is **not** a separate activity: the originator re-broadcasts the **corrected criteria** into the same thread, which re-opens resolution through the ordinary path. The outcome is whatever that resolution returns – a no-match consensus mints a fresh ULI, or a correct existing ULI is adopted – so the mint stays consensus-gated, never the human's unilateral act. Declining a false positive is a clean separation: the wrongly-matched ULI is left untouched, not merged.

## 2.8: Signing and self-verification

A ULI is **self-verifying**: it carries, in the identifier itself, an Ed25519 signature from the org that minted it. Anyone can confirm a ULI was created by a certified network participant – offline, with no registry and no call back to the minter – by checking that signature against the org's published key. This catches the realistic failure mode: a fabricated or mistaken identifier that looks like a ULI but never came from the network, dropped into a data set by error or shortcut.

The signature proves **origin**, not consensus. It answers the question that keeps spurious identifiers out of the graph – "did a certified org mint this?" – and nothing more. Whether a mint was correctly consensus-gated (§2.6) is enforced separately, by certification and continuous monitoring (§3). The signature also binds nothing about the licensee: signing licensee data would tie the identifier to a value that changes over time and break its stability as a linking identifier.

### Identifier form

A ULI is a URN of the form:

`urn:reso:uli:1.0:<uoi>:<nonce>:<signature>`

| Segment | Meaning |
| :--- | :--- |
| `urn:reso:uli:1.0` | The fixed scheme and version prefix; case-insensitive, per URN rules. |
| `<uoi>` | The Unique Org Identifier of the **minting** org. It identifies whose key signed the ULI and traces the ULI to its origin. It is the minting org, **not** the licensee's current location – that is a resolution and registry question. |
| `<nonce>` | A short random value (base64url) that makes each ULI unique by construction, independent of the signature scheme. |
| `<signature>` | An Ed25519 signature (§5) by the minting org over the rest of the identifier, base64url-encoded without padding (86 characters). |

Everything after the version is **case-sensitive** and uses the base64url alphabet, so a ULI MUST NOT be case-folded or otherwise normalized; doing so corrupts the signature. The colon is the segment delimiter and never occurs within a segment.

### What is signed

The minting org signs the **US-ASCII bytes of the identifier up to, and excluding, the signature** – the exact string `urn:reso:uli:1.0:<uoi>:<nonce>`, with the scheme and version prefix in its canonical lowercase form – with its RESO-certified Ed25519 private key, then appends the signature as the final segment. The prefix compares case-insensitively under URN rules, but signing and verification always take it lowercase, so the signed bytes are unambiguous across implementations.

Because every character of that string is in the URN-allowed set – the literal prefix, an alphanumeric UOI, a base64url nonce – it is pure US-ASCII: each character is a single byte, with no percent-encoding and no Unicode normalization, so the bytes to sign are canonical by construction. Signing the full prefix, including `uli:1.0`, binds the signature to this exact namespace and version, so it cannot be replayed under another protocol or a future version.

### Verifying

Given a ULI, a verifier:

1. splits off the final `<signature>` segment and reconstructs the signed string `urn:reso:uli:1.0:<uoi>:<nonce>`;
2. resolves `<uoi>` to the minting org's RESO-certified Ed25519 public key on the governance feed (§2.10);
3. checks the signature over the reconstructed bytes.

If it verifies, the ULI was minted by that certified org; if not, it is not a genuine ULI. No secret and no online service are required – only the org's public key, which a verifier syncs once from the feed and then uses offline. The feed retains the keys of orgs that have rotated, merged or left (§2.10), so a ULI stays verifiable for the life of the identifier; where an org has had more than one key, a signature matching any of its certified keys verifies.

A resolver registry (§2.10) MAY also record which org minted a ULI and the consensus behind it, but verification never depends on it: the proof rides in the identifier, the one thing that always travels with the data.

### Worked example

A minting org with UOI `M00000123` creates a ULI. It draws a nonce, builds the prefix, signs the prefix's US-ASCII bytes with its Ed25519 key and appends the signature:

```
UOI           M00000123
nonce         nyxKF6Gyw9Re
signed bytes  urn:reso:uli:1.0:M00000123:nyxKF6Gyw9Re        (US-ASCII, 39 bytes)
signature     TswgDS6_ggEsPXIDIM6ZO5WsR29AqRDi02IE5REqAty6habgWemveWSAgRHeKY07Tx1hy1dzE3mtpdP6EFdXCA

ULI  urn:reso:uli:1.0:M00000123:nyxKF6Gyw9Re:TswgDS6_ggEsPXIDIM6ZO5WsR29AqRDi02IE5REqAty6habgWemveWSAgRHeKY07Tx1hy1dzE3mtpdP6EFdXCA
```

To check it, a reader strips the signature, reconstructs `urn:reso:uli:1.0:M00000123:nyxKF6Gyw9Re`, looks up `M00000123`'s certified Ed25519 public key on the governance feed – here base64url `jf6SCe-7_mn3Nl5hXnRTyCP3Z9S8z8cT-ku_KqGJXEw` – and verifies the signature over those bytes.

## 2.9: Matching engine and scoring
The same matching engine runs locally (a participant's private de-duplication) and on the network (each resolver). A candidate is scored by summing the weights of the identifiers it matches. The score is a **credit-score-style** value: an arbitrary point scale with thresholds, deliberately not a probability. Primary identifiers are weighted an order of magnitude above secondary ones, so a ULI cannot be validated by secondary identifiers alone.

| Tier | Weight | Identifier |
| :--- | :--- | :--- |
| Primary | 800 | MemberNationalAssociationId (NRDS) |
| Primary | 700 | Full Name (or First Name + Last Name) |
| Primary | 650 | Full Nickname (or Nickname + Last Name) |
| Primary | 625 | License (Number + State + Type) |
| Primary | 600 | License (Number + State) |
| Secondary | 100 | Office ID |
| Secondary | 80 | Office Name |
| Other | 10 each | Last Name, First Name, Middle Name, Nickname, License Sub-Type |

A combined score at or above **1300** is a possible match; a score below **700** is rejected; scores between are inconclusive and SHOULD be routed to review. Correlated identifiers that express the same underlying fact – for example office id and office name – MUST be scored as a single cluster (the strongest matched member), not summed, so that one fact is not counted several times.

Term weights derive from a published static frequency table (rarer values carry more weight), so common values such as a frequent surname cannot on their own clear the threshold. Participants MAY tune weights in their own sandbox, but the network MUST define an official adopted weight set **and** threshold; both are network parameters. When either changes, RESO posts the new set to the governance feed (§2.10); each node MUST re-run matching locally, compute diffs and broadcast the results. The reference implementation computes relevance with a BM25-style score; see §6 for the rationale and references.

**Exact parity.** Two conforming engines MUST produce the same score for the same candidate, so the determinism-sensitive choices are pinned rather than left to the implementation. The adopted weight set, the threshold and the **versioned frequency table** the weights derive from are network parameters posted to the governance feed (§2.10). String and value **normalization** (case folding, accent and whitespace handling, nickname expansion), **tie-breaking** between equal-scoring candidates and the **rounding** of the combined score are fixed by the reference harness and its published test vectors (§3), exactly as the signed-identifier bytes are (§2.8): an engine conforms iff it reproduces the reference scores on those vectors. The Phase-2 global-IDF option (§6) is a future refinement, not part of the parity baseline; until it is adopted the static frequency table is the sole weight source.

The network also leaves room for **specialized resolvers** to provide additional layers of identity verification using PII-based approaches, beyond the shared matching engine described here.

## 2.10: Resolver discovery and the governance feed
All participants certified on the RESO network are ULI resolvers, continuously. A resolver's identity is its Unique Org Identifier, confirmed with a hash of the URL of the node on which it was certified.

The certified ULI resolvers – the membership behind the `reso.org/resolvers` collection a broadcast addresses (§2.4) – are published through RESO's endorsements service, queried by endorsement name:

*REQUEST*

```
GET https://services.reso.org/endorsements?endorsementName=ULI
  Accept: application/json
```

*RESPONSE* (the ULI-endorsed resolver orgs that form `R`; shape illustrative)

```json
{
  "endorsementName": "ULI",
  "value": [
    {
      "OrganizationUniqueId": "<UOI>",
      "ServiceUniqueId": "<USI>",
      "nodeUrlHash": "<SHA-256 of the certified node URL>",
      "status": "active"
    }
  ]
}
```

The endorsements service is **public**, so it never publishes a resolver's node URL – only the **hash** of that URL, alongside the resolver's Unique Org Identifier (UOI) and Unique Service Identifier (USI). The URL itself is exchanged later, through pre-arranged authorization (§2.1): when a node authenticates to a resolver and is handed that resolver's URL with its tokens, it hashes the URL and compares the result to the certified hash here. A match confirms the node is the one RESO certified – identity verified without the registry ever exposing a URL.

RESO publishes a **public governance feed** – an ordered ActivityPub stream every resolver follows. It is the resolver registry and the single channel for all network-level events: a resolver certified and joined (with its Unique Org Identifier and RESO-certified Ed25519 public key, §2.8), a status change (inactive, reactivated, noncompliant) and parameter changes (weights and thresholds, §2.9). Every event is PII-free – org IDs, public keys, status and parameters only, never licensee data – so the feed is safe to be public and serves as a transparent, auditable membership-and-parameter log. A node builds its `R` from that endorsement set – equivalently, by replaying the feed – and follows the feed for live deltas; a feed position is the version a broadcast pins (§2.6).

The resolver pool `R` is a filtered view of the feed: a node is in `R` iff it is **certified, compliant and active**. Two flags remove a node, neither deleting its record (so the ULIs it minted stay verifiable against its retained keys, §2.8):

- **Inactive** – not seen for the staleness window (default ~1 month); soft and reversible. The node keeps its certification and auto-rejoins `R` when it returns and participates. Liveness is an active reachability heartbeat (RESO polls daily), independent of whether the node has matches to offer – a node that only ever replies "no match" is fully live.
- **Noncompliant** – failed continuous conformance (§3.2); an immediate, hard removal. The node must re-certify to return.

Only governance rides the public feed. The resolution conversations stay peer-to-peer and behind authentication (§2.11); the public feed never carries a ULI thread or any licensee data.

## 2.11: Authentication and authorization
Access to the network MUST be authenticated. Each participant node handles its own authorization from that point, using the existing RESO Web API Core and Data Dictionary endpoints or hosting RESO Common Format, with OAuth2 bearer tokens or client credentials. Records are carried only by OAuth2-protected URLs to external services (§2.1).

## 2.12: Worked examples

These examples illustrate the core resolution flows, in increasing order of complexity: **2.12.1** a new ULI when no match is found, **2.12.2** an existing ULI found and adopted, **2.12.3** a late-match self-healing correction, **2.12.4** two existing ULIs merged and **2.12.5** a match found but declined on review. Each renders the threaded conversation as a plain-text thread and a step table, then shows the request and response activities.

The ActivityPub objects use only standard ActivityStreams 2.0 vocabulary (no extensions, per §2.2). The RCF payloads validate against the DD schema (Member resource, DD 2.0, strict). Licensee data and matching detail an activity needs (a match score, a candidate record) ride in the referenced RCF payload, not in the ActivityPub object; a ULI URN, including a tombstone's redirect target, is an identifier and appears directly. ULIs are shown abbreviated for readability; a full ULI is the self-verifying form `urn:reso:uli:1.0:<uoi>:<nonce>:<signature>` of §2.8. Every `/rcf/...` link below is an OAuth2-protected URL (§2.11) – the activities are public, the data behind the links is not. Throughout, `R` resolvers are on the network and consensus is `floor(2/3 × R)`.

### 2.12.1: New ULI, no match found (happy path)

A provider resolves a licensee it holds no ULI for; the network confirms no match; a new ULI is minted at consensus and posted to the thread.

**Thread.** The conversation is a single broadcast `Note` with each resolver's reply threaded beneath it; the originator's `Create` closes the thread once consensus is reached. The table reads top to bottom as the thread does.

| # | Actor | Activity | Description |
| :--- | :--- | :--- | :--- |
| 1 | Provider 1 | Broadcast `Note` "ULI Resolution Request" | Fans out to the certified resolvers; the licensee subset stays behind the OAuth2 link, never embedded in the activity. |
| 2 | Other resolvers | No Match | Each resolver searches its own data and replies in the thread. Replies accumulate toward `floor(2/3 × R)`. |
| – | – | No-match consensus | Enough no-match replies are in to clear the bar; no resolver on the network holds this licensee. |
| 3 | Provider 1 | `Create` ULI `urn:…4a17` | Only now – never unilaterally – the originator mints the ULI and posts it as a thread message bound to the licensee record. |

**Broadcast.** The originator posts the request to its outbox; the server wraps it in a `Create` and federates it to the resolver collection.

*REQUEST*

```
POST https://provider1.example/actor/outbox
  Content-Type: application/activity+json
```

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Note",
  "to": "https://reso.org/resolvers",
  "content": "ULI Resolution Request",
  "url": { "type": "Link", "href": "https://provider1.example/rcf/member/licensee-123", "mediaType": "application/json" }
}
```

*RESPONSE*

```
HTTP/2 201 Created
  Location: https://provider1.example/threads/xyz
  Content-Type: application/activity+json
```

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Create",
  "id": "https://provider1.example/threads/xyz",
  "actor": "https://provider1.example/actor",
  "to": "https://reso.org/resolvers",
  "object": {
    "type": "Note",
    "content": "ULI Resolution Request",
    "url": { "type": "Link", "href": "https://provider1.example/rcf/member/licensee-123", "mediaType": "application/json" }
  }
}
```

**Licensee subset** behind the OAuth2-protected link (a DD-validated Member record):

*REQUEST*

```
GET https://provider1.example/rcf/member/licensee-123
  Authorization: Bearer <token>
  Accept: application/json
```

*RESPONSE*

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
      "MemberType": "Affiliate",
      "MemberNationalAssociationId": "999000111",
      "MemberStateLicense": "SL123456",
      "MemberStateLicenseType": "Salesperson",
      "MemberStateLicenseState": "CA",
      "MemberMlsId": "A0001",
      "OfficeName": "Smith Realty Group",
      "OfficeMlsId": "OFF001",
      "SourceSystemID": "src-sys-01",
      "SourceSystemName": "Source MLS",
      "OriginatingSystemID": "orig-sys-01",
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
  "attributedTo": "https://provider2.example/actor",
  "inReplyTo": "https://provider1.example/threads/xyz",
  "content": "No Match"
}
```

**ULI created.** At no-match consensus, the originator mints the ULI and posts the `Create` to the thread. The new ULI's RCF is the licensee record bound to the minted URN:

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Create",
  "actor": "https://provider1.example/actor",
  "inReplyTo": "https://provider1.example/threads/xyz",
  "object": {
    "type": "Note",
    "content": "ULI Created",
    "url": { "type": "Link", "href": "https://provider1.example/rcf/uli/4a17", "mediaType": "application/json" }
  }
}
```

### 2.12.2: Existing ULI found and adopted

The network returns a match; the originator adopts the existing ULI rather than minting a new one. The broadcast is identical to 2.12.1.

**Thread.** A resolver that already holds the licensee replies with a match that links to the existing ULI. The originator adopts that ULI **locally**, posting nothing further to the thread.

| # | Actor | Activity | Description |
| :--- | :--- | :--- | :--- |
| 1 | Provider 1 | Broadcast `Note` "ULI Resolution Request" | Identical to the 2.12.1 broadcast; the originator does not yet know whether a ULI exists for this licensee. |
| 2 | Provider 8 | Matches Found → `urn:…9f2c` (score 1450) | Provider 8 already holds this licensee under an existing ULI. The score (1450, well above the 1300 bar) rides in the linked payload, not in the activity itself. |
| 3 | Other resolvers | No Match | The other resolvers don't hold the licensee; their replies still count toward consensus. |
| – | – | Match consensus | Enough resolvers have replied, and a match is among them. |
| – | Provider 1 | Adopt `urn:…9f2c` – local, no thread message | The originator binds the existing ULI to its record. Adoption is the whole point of the network – one licensee, one ULI, no matter how many systems hold a record for them. |

**Match reply.** A resolver that holds a match replies; the `url` is OAuth2-protected (§2.11) and resolves, for an authorized reader, to a **resolution payload** carrying the ULI, the match score and the candidate Member record (the Member part is DD-validated):

*REQUEST*

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Note",
  "attributedTo": "https://provider8.example/actor",
  "inReplyTo": "https://provider1.example/threads/xyz",
  "content": "Matches Found",
  "url": { "type": "Link", "href": "https://provider8.example/rcf/uli/9f2c", "mediaType": "application/json" }
}
```

*RESPONSE* (the dereferenced resolution payload)

```json
{
  "uli": "urn:reso:uli:1.0:9f2c-2b6e-4f1a-bd33",
  "score": 1450,
  "licensee": {
    "@reso.context": "urn:reso:metadata:2.0:resource:member",
    "value": [
      {
        "MemberFullName": "Jane Smith",
        "MemberFirstName": "Jane",
        "MemberMiddleName": "A",
        "MemberLastName": "Smith",
        "MemberNickname": "Janie",
        "MemberType": "Affiliate",
        "MemberNationalAssociationId": "999000111",
        "MemberStateLicense": "SL123456",
        "MemberStateLicenseType": "Salesperson",
        "MemberStateLicenseState": "CA",
        "MemberMlsId": "B7788",
        "OfficeName": "Smith Realty Group",
        "OfficeMlsId": "OFF042",
        "SourceSystemID": "resolver8-src",
        "SourceSystemName": "Resolver 8 MLS",
        "OriginatingSystemID": "resolver8-orig",
        "OriginatingSystemName": "Resolver 8 MLS"
      }
    ]
  }
}
```

**Adoption.** On match consensus the originator binds `urn:…9f2c` to its record and stops – no `Create`, and no further activity at all. Adoption changes no shared state, so the thread needs no closing message: the existing ULI already carries its own minting signature (§2.8) from when it was created, and the registry independently records that the ULI is now in use in the originator's market.

This is the general rule, and it is why the other examples *do* post a closing activity while this one does not: **a node posts an outcome to the thread only when that outcome changes shared ULI state** – a mint, a tombstone, a merge. Reusing an existing ULI is none of those.

### 2.12.3: Late-match correction (self-healing)

A ULI is minted at consensus, and then a resolver that was slow or briefly offline replies with a *better* match. The network corrects itself: the just-minted ULI is retired in favor of the better one, and anything pointing at the retired ULI is redirected. No record is lost – only the identifier they share changes.

**Thread.** The late reply lands in the same thread, after the `Create`. The originator answers it with a `Tombstone` that redirects the retired ULI to the better one.

| # | Actor | Activity | Description |
| :--- | :--- | :--- | :--- |
| 1 | Provider 1 | Broadcast `Note` "ULI Resolution Request" | The originator holds no ULI for the licensee and asks the network. |
| 2 | Other resolvers | No Match | Replies accumulate to `floor(2/3 × R)`. |
| – | – | No-match consensus | Enough no-match replies to clear the bar; no resolver holds the licensee. |
| 3 | Provider 1 | `Create` ULI `urn:…4a17` | Minted at no-match consensus, without waiting for every resolver. The `Create` is itself a thread message. |
| 4 | Provider 8 | Matches Found → `urn:…9f2c` (late) | A resolver that was slow or offline replies after the mint, with a match that scores above the threshold. |
| 5 | Provider 1 | `Tombstone` `urn:…4a17` → redirect `urn:…9f2c` | Provider 1 holds the just-minted `urn:…4a17`, so it issues the correction. The better match is verifiable – its score is recomputable from the published criteria (§2.9) – so the holder retires and redirects without a fresh consensus round (§2.7). Records bound to `urn:…4a17` now resolve to `urn:…9f2c`. Resolvers may keep replying afterward; the thread is torn down once the well-known resolvers have responded. |

Why this is safe to do after the fact:

- The ULI is created at consensus (#3) without waiting for everyone, so the network stays responsive. The trade-off is that a better match can still arrive (#4).
- When it does, that better match MUST trigger tombstone-and-redirect (#5): the correction is mandatory, not optional. Because the match is verifiable (§2.9), the holder applies it directly – no fresh consensus round, unlike the mint at #3 (§2.7).
- A tombstone never deletes data. It marks the old ULI as superseded and points to the survivor, so links heal instead of breaking. Each ULI's own signature outlives the thread itself, so it stays verifiable afterward (§2.8).

The correction (#5) tombstones the minted ULI and redirects it to the better match:

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Tombstone",
  "formerType": "Note",
  "id": "urn:reso:uli:1.0:4a17-8d3c-4e92-a1f0",
  "url": { "type": "Link", "href": "urn:reso:uli:1.0:9f2c-2b6e-4f1a-bd33" }
}
```

### 2.12.4: Merge – two existing ULIs

Sometimes one licensee already carries **two different ULIs** – usually because two systems minted one independently before they ever talked. A merge collapses them into one. It is **not a new operation**: it is an ordinary resolution of the **merged criteria** with the two ULIs being retired **excluded from matching**, followed by retiring both into whatever that resolution returns.

The exclusion is the subtle part. If the two were left in, the merged criteria would just re-match them – they carry that data – and there would be no way to tell whether a clean target exists. So the broadcast really asks: *"setting aside the two I'm retiring, does anyone **else** hold this person?"*

- **No other holder** (this example) → mint a neutral third ULI; both originals redirect into it.
- **Some other holder** → adopt that existing ULI as the target instead; both originals redirect to it, nothing new is minted.

Either way **neither original survives** – both are excluded from the match, so a merge never keeps one of the two; it always lands on a new or third-party ULI.

**Thread.** The originator has already learned (from an earlier resolution) that one licensee is held under `urn:…a1b2` (Provider 5) and `urn:…9f2c` (Provider 8). It broadcasts the merged criteria with both excluded, mints the target once no other holder is found, then each holder retires its own ULI into the target. Each retirement is the holder's own act over its own identifier, so no separate consensus ratifies it beyond the no-other-match round (#–) that minted the target. Order matters: the target must exist before anything can redirect to it.

| # | Actor | Activity | Description |
| :--- | :--- | :--- | :--- |
| 1 | Provider 1 | Broadcast `Note` – merged criteria, excluding `urn:…a1b2` + `urn:…9f2c` | The originator already knows both are dups for this licensee; it asks whether any *other* resolver holds the person. |
| 2 | Other resolvers | No Match | No third party holds the licensee. (Providers 5 and 8 are excluded from this broadcast – they hold the two ULIs being retired.) Replies accumulate to `floor(2/3 × R)`. |
| – | – | No-other-match consensus | The two being retired are the only ULIs for this person – so the target must be freshly minted, not adopted. |
| 3 | Provider 1 | `Create` neutral ULI `urn:…c3d4` | Minted at consensus – the target the originals will redirect to. (Had a third holder matched, the originator would adopt theirs instead of minting.) |
| 4 | Provider 5 | `Tombstone` `urn:…a1b2` → `urn:…c3d4` | The holder of ULI-A retires its own ULI into the target, now that it exists. |
| 5 | Provider 8 | `Tombstone` `urn:…9f2c` → `urn:…c3d4` | The holder of ULI-B does the same – both records relink to `urn:…c3d4`. |

The merged criteria behind the broadcast link are an ordinary licensee subset (as in 2.12.1) – the combined record of the person held under both ULIs:

```json
{
  "@reso.context": "urn:reso:metadata:2.0:resource:member",
  "value": [
    {
      "MemberFullName": "Jane Smith",
      "MemberFirstName": "Jane",
      "MemberMiddleName": "A",
      "MemberLastName": "Smith",
      "MemberType": "Affiliate",
      "MemberNationalAssociationId": "999000111",
      "MemberStateLicense": "SL123456",
      "MemberStateLicenseType": "Salesperson",
      "MemberStateLicenseState": "CA",
      "MemberMlsId": "C9012",
      "OfficeName": "Smith Realty Group",
      "OfficeMlsId": "OFF099",
      "SourceSystemID": "src-sys-01",
      "SourceSystemName": "Source MLS",
      "OriginatingSystemID": "orig-sys-01",
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
  "actor": "https://provider1.example/actor",
  "inReplyTo": "https://provider1.example/threads/merge-1",
  "object": {
    "type": "Note",
    "content": "ULI Created (merge target for urn:…a1b2 and urn:…9f2c)",
    "url": { "type": "Link", "href": "https://provider1.example/rcf/uli/c3d4", "mediaType": "application/json" }
  }
}
```

Provider 5 retires `urn:…a1b2` and Provider 8 retires `urn:…9f2c`, each redirecting to the target:

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Tombstone",
  "formerType": "Note",
  "id": "urn:reso:uli:1.0:a1b2-7c5d-4a8e-9b21",
  "url": { "type": "Link", "href": "urn:reso:uli:1.0:c3d4-6f9a-4b3c-8e17" }
}
```

```json
{
  "@context": "https://www.w3.org/ns/activitystreams",
  "type": "Tombstone",
  "formerType": "Note",
  "id": "urn:reso:uli:1.0:9f2c-2b6e-4f1a-bd33",
  "url": { "type": "Link", "href": "urn:reso:uli:1.0:c3d4-6f9a-4b3c-8e17" }
}
```

### 2.12.5: Match found but declined on review

This is the trickiest case, because the network and a human disagree. The math returns a match above the threshold, but a person at the originator recognizes it is the *wrong* person. Rather than adopt it, the originator **declines by re-broadcasting corrected criteria into the same thread**, and the network resolves again – landing on a new ULI (this example) or, if a correct one exists, on that.

The trigger here is a data error, the most common source of a false positive. An MLS onboards a brand-new agent, **Robert Chen Jr.**, by cloning his father **Robert Chen Sr.**'s record as a template – and two fields are never corrected, so Jr.'s record carries Sr.'s **license number and NRDS ID**. Two *primary* identifiers now collide between two genuinely different people, so the match scores well over the bar. Only a human, who knows Jr. is a distinct new licensee, can catch it.

Two things make this case worth spelling out:

- **The decline is a re-broadcast, not a new verb.** ActivityStreams 2.0 has no "reject," and we add none. The originator broadcasts the *corrected* criteria again, in the same thread; that re-broadcast **is** the decline, and it sends resolution back through the ordinary path.
- **Resolution decides the outcome, not the human.** The human can reject the one candidate, but the originator does not mint on its own say-so – the corrected criteria are resolved by the network, and only a no-match consensus mints the new ULI. (Had a *correct* existing ULI surfaced instead, the originator would adopt it.)

**Thread.** The erroneous broadcast draws a high match; the originator declines by re-broadcasting Jr.'s corrected criteria; that resolves to no-match; a fresh ULI is minted – all in the one thread. Sr.'s ULI is never touched: Jr. and Sr. are different people, so this is a clean separation, not a merge.

| # | Actor | Activity | Description |
| :--- | :--- | :--- | :--- |
| 1 | Provider 1 | Broadcast `Note` – Jr.'s criteria, carrying Sr.'s license + NRDS | The MLS resolves its new agent; the record unknowingly holds two of Sr.'s primary identifiers. |
| 2 | Provider 8 | Matches Found → `urn:…9f2c` (Sr.'s ULI, score 1380) | Provider 8 holds Sr. under `urn:…9f2c`; the two shared primaries push the score over 1300. Same reply shape as 2.12.2. |
| 3 | Other resolvers | No Match | They don't hold the person; replies still count toward consensus. |
| – | – | Match consensus → candidate `urn:…9f2c` | The network's answer, purely on the numbers. |
| – | Provider 1 (human) | Reviews the candidate and rejects it | Off the wire. Jr. is a distinct, newly-licensed agent; the matching license and NRDS are a clone artifact – the false positive the math could not catch. |
| 4 | Provider 1 | **Re-broadcast** – Jr.'s *corrected* criteria, same thread | The decline itself: re-posting corrected criteria re-opens resolution. The visible signal, no separate verb. |
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
      "MemberType": "Affiliate",
      "MemberNationalAssociationId": "555000222",
      "MemberStateLicense": "SL778899",
      "MemberStateLicenseType": "Salesperson",
      "MemberStateLicenseState": "CA",
      "MemberMlsId": "J2001",
      "OfficeName": "Chen Realty",
      "OfficeMlsId": "OFF300",
      "SourceSystemID": "src-sys-09",
      "SourceSystemName": "Source MLS",
      "OriginatingSystemID": "orig-sys-09",
      "OriginatingSystemName": "Originating MLS"
    }
  ]
}
```

The decline re-broadcasts the **corrected** criteria – the same record with Jr.'s own identifiers (`MemberNationalAssociationId` and `MemberStateLicense` now his):

```json
{
  "@reso.context": "urn:reso:metadata:2.0:resource:member",
  "value": [
    {
      "MemberFullName": "Robert Chen Jr.",
      "MemberFirstName": "Robert",
      "MemberLastName": "Chen",
      "MemberType": "Affiliate",
      "MemberNationalAssociationId": "555000999",
      "MemberStateLicense": "SL445566",
      "MemberStateLicenseType": "Salesperson",
      "MemberStateLicenseState": "CA",
      "MemberMlsId": "J2001",
      "OfficeName": "Chen Realty",
      "OfficeMlsId": "OFF300",
      "SourceSystemID": "src-sys-09",
      "SourceSystemName": "Source MLS",
      "OriginatingSystemID": "orig-sys-09",
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
  "actor": "https://provider1.example/actor",
  "inReplyTo": "https://provider1.example/threads/xyz",
  "object": {
    "type": "Note",
    "content": "ULI Created",
    "url": { "type": "Link", "href": "https://provider1.example/rcf/uli/b8d6", "mediaType": "application/json" }
  }
}
```

# Section 3: Certification

Certification proves three things – identity, protocol and matching conformance, and the trust guarantees – in two phases: a one-time **onboarding gate** and **continuous conformance**. The rules derive from the Section 2 requirements. Certification is per-component: each of the three layers (matching backend, review UI, resolver network) is certified against its interface, so any vendor may supply any piece.

## 3.1: Onboarding gate (sandbox)
A node MUST pass the onboarding gate before joining the production network. The gate runs in a sandbox – the RESO ULI reference server and resolver node – which serves as both the scoring **oracle** and the protocol **peer**. In the sandbox, and only there, the candidate grants RESO access to its data set for certification; this access is scoped to the cert phase and ends at the production boundary, where data isolation (§2.1) applies in full.

The gate checks:

- **Identity** – the candidate registers its Unique Org Identifier and Ed25519 public key, signs a challenge, and RESO verifies it. This is the binding that ULI signing (§2.8) depends on.
- **Protocol conformance** – the candidate completes the §2.12 sequence against the sandbox resolver (broadcast, reply, create, tombstone and redirect), using vanilla ActivityPub with data behind OAuth2-protected RCF links.
- **Scoring and search parity** – RESO exhaustively exercises the candidate's engine: every record is scored against the reference oracle (scoring), and the candidate's index is queried across the data set (search, measuring recall and precision). Scoring is deterministic under the adopted weight set, so parity MUST be exact; any divergence is a defect to resolve before joining.
- **Signing** – the candidate mints a ULI and the signature it carries verifies against the candidate's registered Ed25519 key (§2.8).

The certification harness – sampling records, feeding them to the reference engine and diffing the results – is provided in RESO Tools. The reference engine and node are themselves built **spec-first** from this RCP, using a constitution and a feature specification to generate the reference server and client; convergence across independent, multi-language implementations is itself a test of this specification's precision. See [RESOStandards/reso-tools#224](https://github.com/RESOStandards/reso-tools/issues/224).

## 3.2: Continuous conformance
Once on the network, a node is monitored continuously rather than re-certified periodically; certification is an ongoing state. The check rides the daily liveness heartbeat: when RESO pings a node that has had activity since the last check, it also samples the last ≤5 threads the node participated in (by its handle) and verifies it **participated correctly** – well-formed activities, valid signatures, correct consensus and lifecycle behavior. The sample is behavioral and observable from the public threads without data access; deeper checks may run when something looks off. The private below-threshold direct-message (DM) path (§2.4) is not passively auditable and is covered by the sandbox gate rather than production sampling.

A conformance failure removes the node **immediately**: RESO marks it noncompliant on the governance feed (§2.10), dropping it from every node's `R` on the next delta. The node's specific violation – what it failed and how to remediate – is delivered privately to its inbox (a DM); the public feed carries only the status, the DM carries the detail.

Marking a node noncompliant invalidates its **pending** sign-offs: each open thread is re-tallied over currently-compliant sign-offs – the originator re-tallies its own affected threads on the decert event and posts an in-thread follow-up where a re-resolution results – and any thread that falls below `floor(2/3 × R)` re-resolves via self-healing. Already-minted ULIs are immutable, each carrying its minter's signature; the mint is the finalization boundary.

## 3.3: Reference correctness
The reference engine is the certification oracle, so it MUST be validated before it certifies any node: the reference MUST pass its own self-test (the reference engine against known-correct resolutions), so that a defect in the oracle cannot silently pass a nonconforming node or fail a conforming one.

# Section 4: Contributors

The ULI Resolution Protocol was developed by the RESO ULI Workgroup and its ActivityPub Subgroup. Lead author: [Joshua Darnell](https://github.com/darnjo) ([RESO](mailto:josh@reso.org)). Additional contributors will be acknowledged as the proposal advances.

# Section 5: References

- W3C ActivityPub: https://www.w3.org/TR/activitypub/
- RESO Common Format: https://github.com/RESOStandards/transport/blob/main/proposals/reso-common-format.md
- RESO ULI Resolution Protocol discussion: https://github.com/RESOStandards/transport/discussions/163
- RESO Listing Advertisement Proposal (ActivityPub examples): https://github.com/RESOStandards/transport/discussions/162
- RESO ActivityPub Subgroup, 2025-08-01 agenda (consensus mechanism and worked example)
- Ed25519 signature scheme – [RFC 8032](https://www.rfc-editor.org/rfc/rfc8032)
- base64url encoding – [RFC 4648 §5](https://www.rfc-editor.org/rfc/rfc4648#section-5)
- Technical and design-rationale references: see §6.

# Section 6: Appendices

## Design rationale

**The score is a credit-score-style value, not a probability.** Match strength is an arbitrary integer on a fixed scale with threshold bands (reject / review / possible match), like a credit score. This is deliberate: a 0–100 or percentage scale would invite reading the number as a calibrated probability, which it is not. A true probability would require calibration against labeled resolutions; until that data exists, the number is a defensible *threshold*, not a chance.

**The weights are bits of identifying information.** Each factor's weight reflects how much it narrows the population – formally, the self-information `log₂(1/frequency)` of the matched value. A national identifier narrows to one person (many bits); a common surname narrows very little (few bits). This is the same quantity as the inverse document frequency (IDF) the matching engine already uses. Two consequences fall out rather than being bolted on: common values such as "John Smith" cannot clear the threshold on their own, and the threshold reads as "enough bits to single out one licensee from the population."

**Correlated identifiers are scored once.** Office id and office name are two views of one fact ("same brokerage"). Summing them double-counts one signal, so a cluster is scored as its strongest matched member, not the sum (§2.9).

**Where the scoring goes next – count-based axes.** Counting occurrences turns flat weights into measured bits along several independent axes: value rarity within a field; how many *providers* (not records) carry a value; and co-occurrence rarity of combinations. Bits add cleanly only across *independent* axes – an axis earns its place only if it carries signal the others do not (measurable via the co-occurrence axis). Term statistics can be shared as aggregate counts without exposing records; a small-count floor (or noise) is required, because the rarest values carry the most information and are therefore the most identifying when their counts are published.

**Shared term statistics.** The initial weight source is a published static frequency table (public name-frequency data and the standard vocabularies), which makes scores comparable across nodes by construction and requires no cross-org exchange. A Phase-2 option computes a global IDF from per-term document-frequency counts published by each node (documents stay local; only counts are shared) – more population-accurate, at the cost of a coordination and small-count-leakage surface.

**Self-verifying identifiers, not a separate proof.** A ULI carries an Ed25519 signature by its minting org over the identifier itself, so it verifies as network-minted from the identifier alone – offline, against the org's certified public key, with no registry and no call back to the minter. The proof rides *in* the identifier because that is the one thing that always travels: a bare ULI copied downstream arrives with no RCF payload, and a registry or minter lookup may be unreachable, so anything kept alongside the identifier rather than inside it cannot be relied on at verification time. The signature proves origin only – that a certified org minted the identifier – which is what keeps fabricated or mistaken ULIs out of the graph; consensus compliance and key compromise are handled by certification and monitoring (§3), not by the identifier. A heavier alternative – per-org signed attestations committed in an RFC 6962 Merkle tree over a keyed licensee reference – would prove the full consensus rather than just origin, but on a closed, certified network with a trusted governance anchor a single minter signature delivers the property that actually matters, anti-fabrication, at a fraction of the implementation cost; the lighter construction is chosen here.

## References

- Okapi BM25 – https://en.wikipedia.org/wiki/Okapi_BM25 ; Robertson & Zaragoza, "The Probabilistic Relevance Framework: BM25 and Beyond" (2009)
- tf–idf – https://en.wikipedia.org/wiki/Tf%E2%80%93idf
- Information content (self-information) – https://en.wikipedia.org/wiki/Information_content
- Entropy (information theory) – https://en.wikipedia.org/wiki/Entropy_(information_theory)
- Shannon, "A Mathematical Theory of Communication" (1948); Cover & Thomas, *Elements of Information Theory*
- Name-frequency data – U.S. Census Bureau, "Frequently Occurring Surnames"; U.S. SSA given-name frequency data
- Distributed information retrieval / global IDF – Callan, "Distributed Information Retrieval" (2000)

# Section 7: License
This document is covered by the [RESO EULA](https://www.reso.org/eula/).

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

Please [contact RESO Transport](mailto:transport@reso.org) if you have any questions.
