# RCP-57 Offer Management – Reference Implementation

Offer exchange over ActivityPub, built against
[`proposals/offer-management.md`](../../../proposals/offer-management.md).

> ## ⚠️ Work in Progress – Not for Production
>
> **The proposal this implements is not ratified.** RCP-57 is a draft before the
> RESO workgroups, its Section 6 records seven questions that are open rather
> than settled, and its Data Dictionary elements are not confirmed. Anything here
> can change.
>
> **This is not a product and has not been certified.** It exists to test whether
> the specification can be implemented, and it is published so that others can
> check that claim rather than take it on trust. It has no operational hardening,
> no deployment story and no support. Do not put it in front of real offers.
>
> **It has known limits as software.** A pre-publish audit of this package found
> four exploitable defects in the payload server, every one of which had survived
> a green suite of 122 tests: an unauthenticated request could crash the process,
> a submitting party could forge the status and identifiers the server states,
> one organization could overwrite another's property address, and personal data
> withheld from the terms was still served inside the property group. All four
> are fixed and each carries a regression test. They are listed here rather than
> quietly closed, because the fact that a well-tested reference had them is the
> most useful thing it can tell an implementer.
>
> Read it as evidence about the specification, not as software to adopt.

## Why It Exists Before the Specification Is Finished

A reference implementation is a test of the specification. A rule that cannot be
built is not a rule, and a rule nobody has built is only a sentence that reads
well.

Building this found five defects, two of them in the specification itself, in a
document that had already been reviewed line by line the same day:

1. **A worked example relied on a relationship no field table declared.** An
   `OfferPropertyGroup` was nested inside an `OfferSubmission` while neither
   resource declared any link to the other.
2. **Two payload members were undeclared.** `Submissions` and `PropertyGroup`
   appeared in examples while no field table defined them, so the check requiring
   a produced payload to validate as RESO Common Format would have failed against
   the specification's own example. Both are now declared as expansions.
3. **The listing coordinate accepted a form that fails certification.** An offer
   carrying only an organization identifier passed, when the rule requires an
   originating system name or identifier.
4. **The obvious ActivityPub library makes a conformant implementation look
   non-conformant.** See below.
5. **A contradiction inside one section.** Section 2.9 called the submission
   history "ordered by timestamp" four paragraphs before requiring
   `OfferSubmissionSequence` and forbidding reliance on the timestamp.

All five are fixed upstream. That is the argument for implementing before
publishing.

## Quick Start

Requires Node 24 or later, for `node:sqlite`. No database to install, no services, nothing to configure.

```bash
npm install
npm test          # 141 tests
npm run lint      # typecheck, source and tests
npm run log       # regenerate CERTIFICATION-LOG.md from the suite's results
```

The specification is read from `proposals/offer-management.md` in this
repository. Set `RCP57_SPEC` to point at a draft elsewhere.

## What Works, and What Does Not

[`CERTIFICATION-LOG.md`](CERTIFICATION-LOG.md) is generated and reports the
current answer. At the time of writing, **21 of the specification's 38
certification checks are verified**.

| Section | Verified | Total | |
| :--- | ---: | ---: | :--- |
| 2.9 Counter offers | 5 | 5 | sequence, ordering, immutability |
| 2.11 Authorization | 5 | 5 | over a real HTTP server |
| 2.4 The Offer resource | 4 | 7 | |
| 2.5 The OfferSubmission resource | 3 | 4 | |
| 2.7 Offer states | 3 | 4 | |
| 2.6 The OfferPropertyGroup resource | 1 | 2 | |
| 2.1 Participation | 0 | 2 | needs the activity-posting path |
| 2.2 ActivityPub usage | 0 | 5 | needs the activity-posting path |
| 2.3 Offer identity | 0 | 1 | |
| 2.8 Activity Streams mapping | 0 | 2 | |

The log distinguishes *not verified* from *fails*. Nothing here is a claim that a
candidate would fail a check, only that no scenario observes it yet. A
certification tool that tests a subset without saying so reads as a passing
grade, which is the failure this log exists to prevent.

Seven rules stated in Section 2 have no check in Section 3 at all, so
certification could never fail an implementation that ignored them. They are
listed in the log rather than quietly fixed, because an unenforceable requirement
is a finding about the specification.

## The Specification Is the Source of Truth

Nothing about the model is transcribed into code. Two generators read the
specification directly:

- **The metadata report** is generated from the field tables of Sections 2.4
  through 2.6, in the same shape as Data Dictionary reference metadata, so
  changing a field's type or nullability changes what validates with no second
  copy to keep in step.
- **The certification checks** are extracted from Section 3. A scenario declares
  which check it covers and carries that check's text as it stood when the
  scenario was written, so a reworded check is reported as drift rather than
  silently testing something else.

Scenarios bind to checks by a distinctive phrase rather than by check number,
because Section 3's numbering is positional. Adding two checks in one pass
renumbered twenty-four of them.

Coverage comes from the test suite's own results, not from the scenario list. A
scenario declared with no passing test behind it is reported as claimed-but-unrun
rather than counted.

## Notes for Anyone Else Implementing This

**The activities are conformant, and the obvious library makes that look
otherwise.** Section 2.2 says an implementation MUST NOT extend ActivityPub's
JSON-LD with custom terms. Fedify attaches four contexts to every activity,
including a vendor namespace, so a conformance test reading the literal
`@context` rejects it. Expanded, every property resolves under
`https://www.w3.org/ns/activitystreams#` and zero properties fall outside
Activity Streams, across `Offer`, `Accept`, `Reject` and `Note`.

> Test the absence of non-Activity-Streams terms in the **expanded** form, never
> the literal `@context` value. The literal test rejects conformant
> implementations built on the most widely used library.

**Fedify does not assign activity identifiers or persist an outbox.** It provides
routing and authorization; the rest is application-defined. Section 2.2 says the
server assigns the `id`, so an implementation owns that either way.

**Naming differs from the wire.** Fedify calls Activity Streams `inReplyTo`
`replyTarget`, and its vocabulary lives on the `@fedify/fedify/vocab` subpath
rather than the package root. The emitted JSON is standard.

**The dependency is pinned.** `@fedify/fedify` 2.3.8, MIT, pure JavaScript with
no native modules. Client-to-server support is young, which is why the version is
pinned rather than ranged. Its OpenTelemetry chain carried four moderate
advisories at install, which an `overrides` entry for `@opentelemetry/core`
clears to zero. Re-check on any version bump.

## Layout

| Path | What it holds |
| :--- | :--- |
| `src/store.ts` | the store interface, where the shape enforces the rules |
| `src/store-sqlite.ts` | SQLite via `node:sqlite`, so persistence costs no dependency |
| `src/hub.ts` | the operations, and which act may follow which |
| `src/auth.ts` | the two authorization decisions of Section 2.11, kept separate |
| `src/constants.ts` | protocol identifiers the specification fixes |
| `src/index.ts` | the public surface |
| `src/server.ts` | the payload server, and every refusal path |
| `src/read-payload.ts` | reads a submission in either of the forms Section 2.5 permits |
| `src/metadata.ts` | the metadata report, generated from the specification |
| `src/checks.ts` | the certification checks, extracted from the specification |
| `src/activities.ts` | the Activity Streams activities |

Two rules are enforced by the store's shape rather than by remembering them.
There is no `updateSubmission`, so append-only is structural. `setStatusOnCurrent`
takes no submission key, so it can only reach the current turn, and takes a side,
so no caller can write the counterparty's status.

## License

Apache 2.0, as the rest of this repository.
