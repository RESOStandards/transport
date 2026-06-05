# CLAUDE.md — transport

This repo hosts the **RESO Data Dictionary reference sheets**, the [transport.reso.org](https://transport.reso.org) site, and Replication Cookbook Process (RCP) drafts. It is **public**.

## What lives here

- `references/dd/RESODataDictionary-{1.7,2.0,2.1}.xlsx` — canonical DD reference sheets, the source of truth for DD reference metadata.
- `references/dd/CHANGELOG.md` — durable audit trail of every DD sheet revision, with row-level deltas linked to transport tickets.
- `artifacts/` — RCP grammars and working artifacts.
- `.github/pages/` — content for the transport.reso.org site.

## DD XLSX discipline

The DD sheets are the **upstream source of truth** for the entire RESO cert reference data pipeline (reso-tools generator → cert-backend Lambda layer → live cert services). An error here cascades to every downstream consumer.

When mutating any `references/dd/RESODataDictionary-*.xlsx`:

- **openpyxl only.** Never use SheetJS (or any JavaScript XLSX library) to write a DD sheet back to disk. SheetJS write-back strips cell formatting, comments, and merged-cell layout and roughly doubles file size. openpyxl preserves all of that and trims unused metadata. Linter script: [`reso-tools/reso-certification/utils/lint-dd-sheet.py`](https://github.com/RESOStandards/reso-tools/blob/main/reso-certification/utils/lint-dd-sheet.py).
- **Lint after every edit.** The linter rewrites the canonical `dd.reso.org` hyperlinks deterministically. Post-lint file size is typically 0.81–0.84× the publisher's original; meaningfully larger sizes indicate non-openpyxl rewriting and should be investigated.
- **Generate diffs at the end of the cycle.** After the lint pass, the JSON regeneration, and the adversarial review checks have all passed cleanly, generate the row-level diffs as the *final* artifact of the cycle:
  - XLSX diff via [`reso-tools/reso-certification/utils/diff-dd-sheet.py`](https://github.com/RESOStandards/reso-tools/blob/main/reso-certification/utils/diff-dd-sheet.py) — emits markdown suitable for the PR comment AND for the `CHANGELOG.md` entry (`--changelog` flag).
  - JSON diff between the regenerated JSON and the prior published JSON — the projection delta. Useful as a sanity check that the JSON matches the XLSX edits faithfully (the JSON delta should be a clean projection of the XLSX delta).

  The ordering is load-bearing: diffs come *after* adversarial review passes, not before. A diff produced from a state that fails review is misleading — it captures content that may not survive the cycle. The diff is the artifact of a *clean* cycle, not a working-state preview. Generating it last makes the diff itself an audit trail that the review steps before it passed.

  Until the GH Actions automation lands (transport [#208](https://github.com/RESOStandards/transport/issues/208)), this step is manual: after the cycle passes review, run `diff-dd-sheet.py` locally and post the markdown output as a comment on the PR so reviewers see the row-level deltas without having to read the binary XLSX diff.

- **CHANGELOG.md entry on every revision.** Each XLSX change gets a row-level breakdown in `references/dd/CHANGELOG.md`, linked to the originating transport ticket. The CHANGELOG entry is generated from the same diff tool's `--changelog` output during the end-of-cycle diff step. Silent edits without a changelog entry mislead downstream consumers about coverage.

The full set of fitness principles the downstream JSON projection must satisfy lives at [`reso-tools/reso-certification/docs/dd-reference-fitness-principles.md`](https://github.com/RESOStandards/reso-tools/blob/main/reso-certification/docs/dd-reference-fitness-principles.md). Read it before authoring an XLSX edit that touches structural columns (`SourceResource`, `LookupName`, `SimpleDataType`, `LookupStatus`).

## Pre-publish review

Non-trivial PRs against `references/dd/` are reviewed against codified invariants before they reach the cert backend's publish gate. The review applies a high-precision discipline against a private review framework — the framework itself is RESO review IP and lives in a separate private location, so this CLAUDE.md does not reproduce it. PR authors don't need to invoke the review themselves; it runs on the maintainer side before merge.

If you author a DD sheet PR, the **most useful upstream signal** for the review is a clear PR body that:

- Links the originating transport ticket(s) (`#N`).
- Lists the row-level deltas you're introducing (added / removed / modified per tab).
- Notes any documented version-rename or resource-consolidation that explains a cross-version asymmetry.
- States which version files are touched (1.7, 2.0, 2.1).

A PR body in that shape lets the pre-publish review focus its attention precisely; vague PR bodies force the review to reconstruct intent from the diff alone, which can cost reviewer attention on legitimate work.

## Don't

- Don't hand-edit `WikiPageUrl` cells. They are deterministically rewritten by the lint script; manual edits create drift the linter would correct anyway.
- Don't bypass the lint pass before commit. Even small XLSX edits should go through `lint-dd-sheet.py` so the resulting file shape is canonical.
- Don't update `references/dd/CHANGELOG.md` manually with hand-written deltas when the diff tool can generate them; the tool's output is consistent and reviewable.
- Don't add a new lookup or field row without confirming the corresponding canonical URL exists (or will exist) at [dd.reso.org](https://dd.reso.org). Broken canonical URLs ripple through every consumer.

## Related

- [`reso-tools`](https://github.com/RESOStandards/reso-tools) — DD reference metadata generator, fitness principles, linter, diff tool.
- [`reso-tools/CLAUDE.md`](https://github.com/RESOStandards/reso-tools/blob/main/CLAUDE.md) — full DD reference-metadata regeneration workflow.
- [`dd.reso.org`](https://dd.reso.org) — authoritative published DD content, derived from these sheets.
