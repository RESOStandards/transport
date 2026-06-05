# DD Reference Sheet Change Log

Tracks revisions to the DD reference XLSX files in `references/dd/`. Entries are reverse-chronological. Each entry summarizes row-level changes; the full Fields/Lookups diff for a given change set lives on the linked transport ticket.

Generated entries use [`diff-dd-sheet.py --changelog`](https://github.com/RESOStandards/reso-tools/blob/main/reso-certification/utils/diff-dd-sheet.py) (`RESOStandards/reso-tools`). The diff and audit tooling is part of the reso-tools DD pipeline.

---

## 2026-06-04 — Lookup Resource StandardLookupValue Field + DD 1.7 reference integrity fixes

Bundles Jason Darrough's [`#203`](https://github.com/RESOStandards/transport/issues/203) / [`#204`](https://github.com/RESOStandards/transport/issues/204) / [`#205`](https://github.com/RESOStandards/transport/issues/205) sheet updates for DD 1.7, 2.0, and 2.1. The 1.7 sheet additionally receives 17 `SourceResource` populations and one `LookupName` correction surfaced by the cert-backend dd-reference data-consistency tests.

The reference integrity fixes are upstream cleanup, not behavior changes — they bring DD 1.7 in line with the FK navigation targets already used in DD 2.0 / 2.1. Without them, expansion fields like `OpenHouse.Listing` were resolving to non-existent types (`org.reso.metadata.Listing` rather than `Property`).

Closes [`#201`](https://github.com/RESOStandards/transport/issues/201), [`#202`](https://github.com/RESOStandards/transport/issues/202), [`#203`](https://github.com/RESOStandards/transport/issues/203), [`#204`](https://github.com/RESOStandards/transport/issues/204), [`#205`](https://github.com/RESOStandards/transport/issues/205).

### DD 2.1

**Ticket**: [#203](https://github.com/RESOStandards/transport/issues/203)
**Tab changes** (added / removed / modified): Fields 0 / 0 / 2

**Fields — modified (2)**:
- `Lookup::StandardLookupValue` — DisplayName, Definition
- `Property::CapRate` — PropertyTypes

### DD 2.0

**Ticket**: [#204](https://github.com/RESOStandards/transport/issues/204)
**Tab changes** (added / removed / modified): Fields 0 / 0 / 1

**Fields — modified (1)**:
- `Lookup::StandardLookupValue` — Definition, DisplayName

### DD 1.7

**Ticket**: [#205](https://github.com/RESOStandards/transport/issues/205) (+ integrity fixes outside ticket scope)
**Tab changes** (added / removed / modified): Fields 0 / 0 / 19

**Jason's changes (1)**:
- `Lookup::StandardLookupValue` — Definition, DisplayName

**Integrity fixes — `SourceResource` populated (17)**:

FK-navigation fields (target ≠ field name):
- `HistoryTransactional::ChangedByMember` → `Member`
- `HistoryTransactional::OriginatingSystem` → `OUID`
- `HistoryTransactional::SourceSystem` → `OUID`
- `OpenHouse::Listing` → `Property`
- `OpenHouse::OriginatingSystem` → `OUID`
- `OpenHouse::ShowingAgent` → `Member`
- `OpenHouse::SourceSystem` → `OUID`
- `Prospecting::Contact` → `Contacts`
- `Prospecting::OwnerMember` → `Member`
- `Queue::OriginatingSystem` → `OUID`
- `Queue::SourceSystem` → `OUID`

Self-named expansions (target = field name, populated explicitly per XLSX-as-source-of-truth):
- `OpenHouse::HistoryTransactional` → `HistoryTransactional`
- `OpenHouse::Media` → `Media`
- `OpenHouse::SocialMedia` → `SocialMedia`
- `Prospecting::HistoryTransactional` → `HistoryTransactional`
- `Prospecting::Media` → `Media`
- `Prospecting::SavedSearch` → `SavedSearch`

**Integrity fix — `LookupName` correction (1)**:
- `PropertyUnitTypes::UnitTypeFurnished` — `LookupName` `UnitTypeFurnished` → `Furnished` (no `UnitTypeFurnished` lookup exists; `Furnished` is the canonical enum and matches DD 2.0 / 2.1)

**Cross-verification**: All 17 `SourceResource` targets were cross-referenced against DD 2.0 and 2.1 — both versions agree on every target. The `Furnished` `LookupName` fix matches DD 2.0 / 2.1 convention.

**Downstream contract gate**: [`reso-certification-backend` PR #118](https://github.com/RESOStandards/reso-certification-backend/pull/118) runs a six-test battery on the regenerated `dd-{ver}.json` before each layer publish. All 34 tests pass across the three versions after this revision.
