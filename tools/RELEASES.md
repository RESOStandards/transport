# RESO Transport Tools — Release Notes

---

## v0.0.22 — 2026-03-05

### SQLite Data Access Layer Backend (#3)

Added SQLite as a third database backend alongside PostgreSQL and MongoDB.
Lightweight option requiring no external database — ideal for local development
and testing.

**New files:**
- `filter-to-sqlite.ts` — OData $filter to SQLite SQL translator (32 tests)
- `sqlite-schema-generator.ts` — Edm type to SQLite type mapping + DDL (14 tests)
- `sqlite-dal.ts` — Full `DataAccessLayer` implementation with CTE + LEFT JOIN
  for `$expand`, `json_each()` for collection lambda expressions, `strftime()` for
  date functions, and `REGEXP` for `matchesPattern()`
- `sqlite-pool.ts` — Database handle factory with WAL journal mode

**Configuration:**
- `DB_BACKEND=sqlite` selects the SQLite backend
- `SQLITE_DB_PATH` sets the database file path (default: `./reso_reference.db`)

**Docker Compose:**
- `docker compose --profile sqlite up -d` — starts server + UI with SQLite
- `docker compose --profile sqlite --profile seed-sqlite up` — seeds test data
- Compliance profiles: `compliance-dd-sqlite`, `compliance-core-sqlite`

**Compliance:** Web API Core 2.0.0 — 42/42 passed, 3 skipped (identical to
PostgreSQL and MongoDB).

---

## v0.0.21 — 2026-03-04

### Lookup Resource + Human-Friendly String Enumerations (#29)

The server now exposes a **Lookup Resource** per DD 2.0 Section 2.2 and uses
human-friendly `StandardName` display values for string enumerations. This is
required for DD 2.0 compliance when using `Edm.String` enumeration mode.

**Lookup Resource:**

- New `Lookup` entity set with 6 fields: `LookupKey`, `LookupName`,
  `LookupValue`, `StandardLookupValue`, `LegacyODataValue`,
  `ModificationTimestamp`
- `LookupKey` is a deterministic SHA-3 256 hash of `{LookupName}:{LookupValue}`
- Auto-seeded from `server-metadata.json` at startup (3,611 records)
- Read-only: GET collection + GET by key with full OData query support
  (`$filter`, `$top`, `$skip`, `$count`, `$select`, `$orderby`)
- POST/PATCH/DELETE routes not registered (404)

**Enumeration mode (`ENUM_MODE`):**

- New `ENUM_MODE` environment variable (`string` | `enum-type`, default `string`)
- **String mode** (default): `Edm.String` types with `LookupName` annotations,
  Lookup Resource exposed, human-friendly values in data (e.g.,
  `"Active Under Contract"` instead of `"ActiveUnderContract"`)
- **EnumType mode**: future ticket (#30)

**Data generator:**

- New `transformLookupsForHumanFriendly()` transforms lookup maps so all
  downstream generators produce human-friendly values without code changes
- `getLookupDisplayValue()` resolves `StandardName` annotation when available,
  falls back to `lookupValue` for the 23 lookups without annotations

**MongoDB filter fixes:**

- `$ne` operator now uses `$nin: [value, null]` to exclude null/missing
  documents, matching SQL three-valued NULL logic
- `all()` lambda now uses `$not: { $elemMatch: { $ne: value } }` for correct
  "every element matches" semantics (was using `$all` which is set containment)

**RESOScript generator improvements:**

- Decimal and date fields use median of distinct values (prevents `gt`/`lt`
  tests from failing when the first value is an extreme)
- Multi-value lookup `MultipleLookupValue1` prefers single-element collection
  records so `all()` tests find matching data

**UI:**

- Lookup added to resource list for browsing/searching
- Add/Edit/Delete buttons hidden for read-only resources (Lookup)
- Nginx proxy allowlist updated

**Compliance results (both PostgreSQL and MongoDB):**

- Web API Core 2.0.0: **42 passed, 0 failed, 3 skipped**

---

## v0.0.20 — 2026-03-04

### Web API Core 2.0.0 Compliance — All Tests Passing (#19)

The reference server now passes all applicable Web API Core 2.0.0 compliance
tests: **42 passed, 0 failed, 3 skipped** (the 3 skipped are `has` operator
tests, not applicable when using string enumerations).

**Server fixes:**

- **PostgreSQL numeric coercion** — `pg` driver returns BIGINT/NUMERIC as
  strings; `deserializeValue()` now coerces Edm.Int* to JavaScript integers
  and Edm.Decimal/Double/Single to JavaScript decimals
- **Edm.Date truncation** — Edm.Date fields now return ISO 8601 date-only
  (`YYYY-MM-DD`), not full timestamps, in API responses
- **Lambda `any()`/`all()` support** — OData collection lambda expressions
  translated to PostgreSQL JSONB containment (`@>`, `<@`) and MongoDB native
  array operators (`$in`, `$all`), with 6 new tests
- **Service document endpoint** — `GET /` returns the OData service document

**RESOScript generator improvements:**

- Date values truncated to ISO 8601 date-only format
- Timestamp field selection prefers fully-populated DateTimeOffset fields with
  fallback to ModificationTimestamp (always populated per RESO rules)
- Integer field selection uses median of distinct values (ensures `lt`/`gt`
  tests find matching records)
- Multi-value lookup fields correctly populated for `any()`/`all()` tests

**Filter parser:**

- Rebuilt `dist/` with fractional seconds support in DateTimeOffset lexer

---

## v0.0.19 — 2026-03-04

### UI: Expansion Cards for Navigation Properties (#25)

The detail page now displays expanded navigation property data in a collapsible
"Related Records" section. Each expansion renders as an inset card with a
two-column, vertically scrollable field list (max 4 visible rows).

- **To-one expansions** (ListAgent, ListOffice, etc.): single card with
  alphabetized fields and "View" link to navigate to the related entity
- **To-many expansions** (Media, OpenHouse, Showing, etc.): card with
  back/next pagination to browse individual records
- Expansion fields are filtered from regular field lists (no more "[object Object]")
- Existing `MediaCarousel` (photo/video display) unchanged

### Bug Fix: nginx proxy missing Teams, TeamMembers, OUID

The nginx reverse proxy allowlist in `nginx.conf.template` was missing three
resources added in v0.0.18 (`Teams`, `TeamMembers`, `OUID`). API requests for
these resources were served the SPA HTML instead of being proxied to the backend,
causing "not valid JSON" errors.

---

## v0.0.18 — 2026-03-04

### Data Generator: Referentially Correct Multi-Resource Seed Data (#24)

The data generator now produces records with valid cross-resource FK linkages so
`$expand` works for all to-one navigation properties (e.g., Property→ListAgent,
Member→Office, Office→OfficeBroker).

**FK resolver (`fk-resolver.ts`):**

- Discovers to-one FK relationships from metadata navigation bindings
- Builds a dependency graph and topologically sorts resources
- Detects and breaks the Office ↔ Member circular dependency via deferred back-fill
- 28 tests covering FK discovery, dependency graph, topo sort, and plan building

**Multi-resource orchestrator (`index.ts`):**

- New `generateWithDependencies()` creates resources in dependency order with FK injection
- Maintains a key pool per resource; injects valid FK values from previously created records
- Three output modes: HTTP POST (with server back-fill via `dal.update()`), JSON files, curl scripts
- Supports `resolveDependencies` flag in `SeedOptions`

**PATCH support (`client.ts`):**

- `patchRecordsViaHttp()` for HTTP back-fill of deferred FKs
- `updateJsonRecords()` for read-modify-write of JSON output files
- Curl script generation extended with PATCH commands

**CLI improvements:**

- `--deps` flag (default true) enables dependency resolution
- Interactive mode shows plan summary: "Property (10) requires: Office (4), Member (10), ..."

**Server endpoint:**

- `POST /admin/data-generator` accepts `resolveDependencies` in request body
- When true, builds a multi-resource plan and executes phases with `dal.update()` back-fill

**Docker seed:**

- Single seed call with `resolveDependencies: true` replaces separate per-resource calls
- Verified: PostgreSQL (892 records, 0 errors) and MongoDB (892 records, 0 errors)

**Bug fixes during testing:**

- TaxAssessedValue: Edm.Int64 but generated as decimal — fixed with `Math.round()`
- PostgreSQL CTE $expand JOIN: parent-fk strategy used raw column name instead of CTE alias
- PostgreSQL CTE $expand: FK columns now included when `$select` is used with `$expand`
- PostgreSQL $count with $top=0: window function returns nothing — added fallback `SELECT COUNT(*)`
- Removed hardcoded `OfficeBrokerKey = 'BRK0001'` from office generator
- UI data generator: added `resolveDependencies: true` to API call
- Added Teams, TeamMembers, OUID to TARGET_RESOURCES in server and UI

**Files changed:** 20 files, ~760 insertions, ~130 deletions

---

## v0.0.17 — 2026-03-04

### UI: Pinned Search Toolbar and Detail Header (#23)

On search pages, the resource title, action buttons, search bar, advanced search
panel, sort buttons, and result count now stay pinned at the top — only the result
cards scroll. On detail pages, the back link, title, and Edit/Delete buttons stay
pinned while summary, media, and field groups scroll beneath.

**Layout change:** `<main>` is now `overflow-hidden` and each page manages its own
scroll container. This enables per-page pinning without `position: sticky` hacks.

**Affected files:**

- `ui/src/components/layout.tsx` — removed padding and scroll from `<main>`
- `ui/src/pages/search-page.tsx` — flex layout: pinned toolbar + scrollable results
- `ui/src/components/results-list.tsx` — count display moved to search page pinned area
- `ui/src/pages/detail-page.tsx` — flex layout: pinned header + scrollable content
- `ui/src/pages/add-page.tsx`, `edit-page.tsx`, `delete-page.tsx`,
  `not-found-page.tsx`, `admin/admin-layout.tsx` — scroll wrappers with padding

---

## v0.0.16 — 2026-03-04

### Validation Improvements, Tax/Expense Data, and UI Fixes

**Validation: Pattern-based field rules and negative coordinate fix (#22):**

- Added `fieldPattern` to `FieldRule` for regex-based field matching — expense,
  fee, and amount fields (matching `/(?:Expense|Amount|Fee\d?)$/`) now validated
  against a $0–$10,000 range without enumerating all 25+ field names
- Price fields (ListPrice, OriginalListPrice, etc.) now require values > 0
  (previously accepted 0)
- Latitude and Longitude exempt from the "must be >= 0" rule — negative
  coordinates are valid for Western/Southern hemispheres
- 11 new validation tests (expense patterns, negative coordinates)

**Data generator: Realistic tax and expense fields:**

- Added US state effective property tax rates (2024 ACS data, all 50 states)
- `TaxAnnualAmount` calculated from ListPrice × state rate with ±10% randomization
- `TaxAssessedValue` generated at 70–95% of ListPrice
- `TaxYear` set to current or previous year
- 10 expense fields generated with realistic ranges: AssociationFee, InsuranceExpense,
  ElectricExpense, WaterSewerExpense, TrashExpense, CableTvExpense, MaintenanceExpense,
  OperatingExpense, OtherExpense, AssociationFee2
- 5 new data generator tests

**UI: Auto-expand field groups with validation errors (#22):**

- `FieldGroupSection` now accepts an `errorCount` prop — groups auto-expand
  when errors are present, with a red error count badge on the section header
  and a red border highlight
- `RecordForm` computes per-group error counts via `useMemo` and passes them
  to each `FieldGroupSection`
- Only affects resources with field groups (e.g., Property)

**UI: Dark mode persistence fix:**

- Dark mode preference now persists via `localStorage` instead of URL query
  params, which were lost on React Router navigation
- URL `?theme=dark|light` still works as a one-time override

**Affected files:**

- `validation/src/business-rules/types.ts` — `fieldPattern` on `FieldRule`
- `validation/src/business-rules/property-rules.ts` — expense pattern rule, price min > 0
- `validation/src/business-rules/index.ts` — pattern rule matching logic
- `validation/src/metadata/validate.ts` — `ALLOW_NEGATIVE_FIELDS` set
- `validation/tests/business-rules.test.ts` — 9 new expense tests
- `validation/tests/validate.test.ts` — 2 new coordinate tests
- `data-generator/src/generators/property.ts` — state tax rates, expense generation
- `data-generator/tests/generators.test.ts` — 5 new tests
- `ui/src/components/field-group-section.tsx` — errorCount prop, auto-expand
- `ui/src/components/record-form.tsx` — per-group error counts
- `ui/src/hooks/use-dark-mode.ts` — localStorage persistence

### Test Summary

| Package | Tests |
|---------|------:|
| `@reso/validation` | 103 |
| `@reso/odata-filter-parser` | 152 |
| `@reso/odata-client` | 101 |
| `@reso/data-generator` | 76 |
| `@reso/reference-server` | 137 |
| `@reso/certification-add-edit` | 49 |
| **Total** | **618** |

---

## v0.0.15 — 2026-03-05

### DD 2.0 Compliance and Expansion Field Fixes

Achieved full RESO Data Dictionary 2.0 compliance (928 passed, 676 skipped, 0
variations, 0 schema validation errors) and fixed several OData spec issues.

**Metadata source:**

- Replaced `server-metadata.json` with the `reso-certification-etl` DD 2.0
  reference (`metadata-report.json` dated 2024-10-15), fixing 6 field name
  casing issues (`ID` → `Id`) that caused compliance variations
- Same 1,727 fields; 3,611 lookups (355 more than previous source)
- Users can still pass their own metadata via the `METADATA_PATH` env var

**Expansion field lazy loading:**

- Server no longer returns expansion fields (e.g., HistoryTransactional,
  SocialMedia) in responses unless explicitly requested via `$expand`
- Fixed in both PostgreSQL and MongoDB DALs at all levels: parent queries,
  readByKey, and navigation property sub-queries
- 7 new tests verifying expansion field filtering behavior

**Data generator DD 2.0 compliance:**

- Removed `MimeType` from Media generator (not a DD 2.0 field)
- Removed `ResourceName`/`ResourceRecordKey` from OpenHouse and Showing
  generators (those resources use `ListingKey` for parent FK)
- Fixed Showing field names: `ShowingStartTime` → `ShowingStartTimestamp`,
  `ShowingEndTime` → `ShowingEndTimestamp`, removed `ShowingDate` and
  `ShowingInstructions` (not in DD 2.0)
- Added `isExpansion` flag to `ResoField` type; field generator skips
  expansion fields

**OData filter parser:**

- Fixed DateTimeOffset lexer regex to handle fractional seconds
  (e.g., `2026-03-04T13:02:21.582Z`) — previously only matched whole seconds

**Other fixes:**

- EDMX generator filters out expansion fields from NavigationProperty output
- Mock OAuth endpoint now parses `application/x-www-form-urlencoded` bodies
- OData-Version response header added to all responses
- Docker Compose: added 4 compliance services (DD + Core × Postgres + MongoDB)
  with profiles and result volumes

**Compliance infrastructure (new `compliance/` directory):**

- `Dockerfile.dd` — reso-certification-utils v3.0.0 (multi-stage build)
- `Dockerfile.core` — web-api-commander (Gradle build)
- `dd-config.json` — DD 2.0 config with bearer token and client credentials
- `entrypoint-dd.sh` — wait for server, run DD tests (supports `RECORD_LIMIT`)
- `entrypoint-core.sh` — wait for server, generate RESOScripts, run Core tests

### Test Summary

| Package | Tests |
|---------|------:|
| `@reso/validation` | 91 |
| `@reso/odata-filter-parser` | 152 |
| `@reso/odata-client` | 101 |
| `@reso/data-generator` | 73 |
| `@reso/reference-server` | 137 |
| `@reso/certification-add-edit` | 49 |
| **Total** | **603** |

---

## v0.0.14 — 2026-03-04

### EDMX Metadata: EntityContainer and Nullable Fixes

Fixed two OData spec compliance issues in the generated EDMX XML metadata
(`/$metadata` endpoint).

**EntityContainer with EntitySets:**

- Added `<EntityContainer Name="Default">` with an `<EntitySet>` element for
  each target resource, as required by the OData CSDL specification
- EntitySets include `<NavigationPropertyBinding>` entries linking navigation
  properties to their target EntitySets (only for targets included in the
  resource list)

**Nullable attribute corrections:**

- Removed `Nullable="true"` from all properties — this is the OData default
  for non-collection properties and should not be emitted
- Collection properties now always emit `Nullable="false"` since they return
  the empty list `[]` instead of `null`

**Modified files:**

- `edmx-generator.ts` — EntityContainer generation, Nullable logic
- `edmx-generator.test.ts` — 4 new tests (EntityContainer, EntitySet,
  NavigationPropertyBinding, Nullable)

### Test Summary

| Package | Tests |
|---------|------:|
| `@reso/validation` | 91 |
| `@reso/odata-filter-parser` | 151 |
| `@reso/odata-client` | 101 |
| `@reso/data-generator` | 71 |
| `@reso/reference-server` | 130 |
| `@reso/certification-add-edit` | 49 |
| **Total** | **593** |

---

## v0.0.13 — 2026-03-04

### OData $filter Two-Way Sync and Validation

Added two-way synchronization between the search bar and advanced search form,
client and server-side filter validation, and an AST serializer for the filter
parser.

**AST serializer (`astToFilterString`):**

- New `serializer.ts` in `@reso/odata-filter-parser` — walks the AST and
  produces a canonical OData `$filter` string
- Handles all node types: comparison, logical, not, arithmetic, function,
  lambda, literal, property, collection
- 54 round-trip tests (parse → serialize → compare)

**Filter sync utility (`filter-sync.ts`):**

- `parseFilterToEntries()` — parses an OData filter string into form-compatible
  entries using the AST, detecting simple comparisons, `contains()`, lambda
  `any`/`all` patterns, and flagging unrepresentable expressions
- `buildFilterString()` — moved from advanced-search, builds OData filter from
  form entries

**Search page state management:**

- `draftFilter` state lifted to SearchPage as single source of truth
- URL `$filter` param synced on browser back/forward
- Client-side validation via `parseFilter()` blocks invalid filters with
  error display before API calls
- SearchBar is now a fully controlled component with validation error display
  and copy-to-clipboard for long filters

**Advanced search two-way sync:**

- Form state derived from incoming filter string via `parseFilterToEntries()`
- `lastEmittedRef` prevents infinite update loops in bidirectional sync
- Live sync: field changes rebuild filter string and update search bar
- Amber info bar when filter contains expressions the form can't represent
- Expansion fields disabled with "filtering not yet supported" message

**Server-side validation:**

- `collectionHandler` returns 400 (not 500) for `ParseError`, `LexerError`,
  and filter-related errors with OData error format

**Modified files:**

- `odata-filter-parser/src/serializer.ts` — New
- `odata-filter-parser/src/index.ts` — Added export
- `odata-filter-parser/tests/serializer.test.ts` — New (54 tests)
- `ui/package.json` — Added `@reso/odata-filter-parser` dependency
- `ui/Dockerfile` — Copy parser into Docker build
- `ui/src/utils/filter-sync.ts` — New
- `ui/src/pages/search-page.tsx` — Lifted state, validation
- `ui/src/components/search-bar.tsx` — Controlled component rewrite
- `ui/src/components/advanced-search.tsx` — Two-way sync
- `server/src/odata/handlers.ts` — 400 for invalid filters

### Test Summary

| Package | Tests |
|---------|------:|
| `@reso/validation` | 91 |
| `@reso/odata-filter-parser` | 151 |
| `@reso/odata-client` | 101 |
| `@reso/data-generator` | 71 |
| `@reso/reference-server` | 126 |
| `@reso/certification-add-edit` | 49 |
| **Total** | **589** |

---

## v0.0.12 — 2026-03-04

### $expand: Property Child Resources and Three FK Strategies

Extended `$expand` support from 3 resources (Media, OpenHouse, Showing) to all 7
Property child resources, and refactored navigation property discovery to support
three distinct foreign key strategies.

**New child resources:** PropertyRooms, PropertyGreenVerification,
PropertyPowerProduction, PropertyUnitTypes — all linked to Property via
`ListingKey` (direct FK strategy).

**Three FK strategies in `buildNavigationBindings`:**

- **resource-record-key** — polymorphic FK via `ResourceName` +
  `ResourceRecordKey` columns (Media on any parent resource)
- **direct** — child has the parent's key field directly (OpenHouse, Showing,
  PropertyRooms, PropertyGreenVerification, PropertyPowerProduction,
  PropertyUnitTypes all have `ListingKey`)
- **parent-fk** — parent entity holds a FK to the target (to-one nav props like
  BuyerAgent, ListAgent, BuyerOffice, ListOffice on Property → Member/Office)

**OpenAPI and EDMX metadata:**

- OpenAPI: GET collection endpoints now include `$expand` query parameter with
  valid navigation property names listed in the enum
- EDMX: `NavigationProperty` elements generated for all expansion fields with
  correct `Type` (collection vs singleton) and `Partner` attributes

**Modified files:**

- `router.ts` — Three-strategy `buildNavigationBindings`, exported for testing
- `openapi-generator.ts` — `$expand` query parameter on GET collection endpoints
- `edmx-generator.ts` — `NavigationProperty` elements from expansion metadata
- `types.ts` — Extended `TARGET_RESOURCES` with 4 new child resources,
  added `FieldMetadata.isExpansion` support
- `postgres-dal.ts` — Updated expand query for direct FK strategy
- `mongo-dal.ts` — Updated expand query for direct FK strategy
- `data-access.ts` — Generalized `ExpandBinding` type for all FK strategies

### Data Generator: Property Child Resources

Added generators for 4 new child resources and made the seed plan dynamic.

- `property-child.ts` — Generic generator for PropertyRooms,
  PropertyGreenVerification, PropertyPowerProduction, PropertyUnitTypes
- `plan.ts` — `getRelatedResources()` discovers valid child resources per parent
  using FK field analysis; `getDefaultRelatedCount()` returns sensible defaults
- `open-house.ts`, `showing.ts` — Added `ListingKey` to generated records
- Docker Compose seed scripts updated with all 7 child resources

### UI: Data Generator Improvements

- Related Records section now shows only valid expansions for the selected parent
  resource (driven by server metadata, not hardcoded)
- Short display names for child resources (PropertyRooms → "Rooms", etc.)
- Two-column responsive layout (resource/count on left, related records on right)
- Wider container (`max-w-5xl`) for better use of horizontal space
- Plan summary computes total related records dynamically

**Modified files:**

- `admin-client.ts` — Added `RelatedResourceInfo` type and `relatedResources`
  field to `ResourceStatus`
- `data-generator.ts` (server) — Status endpoint returns `relatedResources`
  per resource using `getRelatedResources()` from `@reso/data-generator`
- `data-generator-page.tsx` — Context-sensitive related records UI

### nginx SPA Routing Fix

Fixed "Cannot GET /admin/data-generator" error on browser refresh. The nginx
config previously proxied all `/admin/*` paths to the backend. Split into a
dedicated `/admin/` location block that proxies POST and `/status` to the API
while serving the SPA for page routes. Also added the 4 new child resources
to the OData entity and collection regex patterns.

### Test Summary

17 new tests (navigation property discovery + EDMX generation).

| Package | Tests |
|---------|------:|
| `@reso/validation` | 91 |
| `@reso/odata-filter-parser` | 97 |
| `@reso/odata-client` | 101 |
| `@reso/data-generator` | 71 |
| `@reso/reference-server` | 126 |
| `@reso/certification-add-edit` | 49 |
| **Total** | **535** |

---

## v0.0.11 — 2026-03-03

### UI: Detail Page Layout and Field Grouping Improvements

Redesigned the detail page layout and improved field grouping behavior across
all UI views.

**Detail page layout (#15):**

- Media carousel now sits on the right (half-width) with a summary pane on the
  left, side-by-side on desktop, stacked on mobile
- For Property: left pane shows formatted address, key, timestamp, and
  configured summary fields (from `ui-config.json`); grouped field sections
  appear below in collapsible panels
- For resources without groupings: all fields display alphabetically in a
  two-column layout beside the carousel

**Remove "Other" group for ungrouped resources (#14):**

- Resources without field groupings (Member, Office, Media, OpenHouse, Showing)
  now display fields as a flat alphabetical list instead of wrapping them in a
  collapsible "Other" section
- Applies to detail page, record form, and advanced search

**Fixed header/sidebar and request URI fix (#16):**

- Header and left navigation sidebar now stay fixed; only the main content
  area scrolls
- Fixed "Request URI too large" error by omitting `$select` for resources
  with `summaryFields: "__all__"` (server returns all fields by default)
- Increased nginx `large_client_header_buffers` to `4 32k` for complex
  OData queries

**Modified files:**

- `layout.tsx` — Fixed header/sidebar with scrollable content area
- `search-page.tsx` — Omit `$select` for `__all__` resources
- `nginx.conf.template` — Increased header buffer limit
- `detail-page.tsx` — Summary pane + carousel layout, summary field extraction
- `record-form.tsx` — Flat field grid when no groups exist
- `advanced-search.tsx` — Flat field rows when no groups exist

---

## v0.0.10 — 2026-03-03

### MongoDB Document Store Backend

Added MongoDB as a fully functional alternative database backend, selectable via
the `DB_BACKEND` environment variable.

**New files:**

- `filter-to-mongo.ts` — OData `$filter` AST to MongoDB query translator with
  two modes: native query operators (index-friendly) for simple comparisons, and
  `$expr` aggregation expressions for functions and arithmetic
- `filter-to-mongo.test.ts` — 33 tests covering all comparison, logical, string,
  date, math, and arithmetic operators
- `mongo-dal.ts` — Production MongoDB `DataAccessLayer` adapter with cursor
  pagination and batch `$in` queries for `$expand` resolution
- `mongo-init.ts` — Collection and index setup (unique PK index per resource,
  compound FK index for child collections)

**Modified files:**

- `config.ts` — Added `DB_BACKEND` (`postgres` | `mongodb`) and `MONGODB_URL`
  environment variables
- `index.ts` — Conditional DAL instantiation via dynamic imports (mongodb package
  only loaded when `DB_BACKEND=mongodb`)
- `docker-compose.yml` — Added `mongodb` profile with `mongodb`, `server-mongo`,
  `ui-mongo`, and `seed-mongo` services

**Deleted:** `mongo-dal.example.ts` — superseded by production `mongo-dal.ts`

**Docker usage:**

- PostgreSQL (unchanged): `docker compose up -d`
- MongoDB: `docker compose --profile mongodb up -d mongodb server-mongo ui-mongo`
- MongoDB seed: `docker compose --profile seed-mongo up seed-mongo`

### Documentation

- Updated server README with MongoDB backend docs (env vars, project structure,
  DAL adapter comparison, filter translation, MongoDB-specific behavior)
- Updated reference-server README with Docker instructions for both backends
- Created READMEs for `@reso/validation`, `@reso/data-generator`, and
  `@reso/certification-test-runner`

---

## v0.0.9 — 2026-03-03

### Server-Driven Pagination with @odata.nextLink

Fixed `$expand` + pagination bug and added OData `@odata.nextLink` support for
server-driven infinite scroll.

**Bug fix**: When `$expand=Media` was used with `$top`/`$skip`, the PostgreSQL
adapter applied `LIMIT`/`OFFSET` to the flat LEFT JOIN result rows instead of
parent entities. With 50 Properties × 5 Media each, `LIMIT 25` returned only 5
unique Properties after grouping, and `COUNT(*) OVER()` reported 250 instead of
50.

**Fix**: The PostgreSQL adapter now uses a CTE (`WITH parent_page AS (...)`) to
paginate parent rows first, then LEFT JOINs expanded navigation properties in
the outer query. `COUNT` and `LIMIT`/`OFFSET` apply to parent entities only.

**@odata.nextLink**: The collection handler now generates `@odata.nextLink` URLs
when more pages exist (i.e., `result.value.length === $top`). The nextLink
encodes `$top`/`$skip` along with all other query parameters ($filter, $select,
$orderby, $count, $expand).

**UI**: The `useCollection` hook now follows `@odata.nextLink` for infinite
scroll instead of manually computing `$skip` offsets. The `fetchCollectionByUrl`
client function fetches raw nextLink URLs.

**DAL abstraction**: The `DataAccessLayer` interface (`data-access.ts`) is
unchanged — the CTE logic is PostgreSQL-specific. The MongoDB adapter sketch
already handles this correctly (cursor pagination + batch expand). The nextLink
generation is OData protocol logic in the handler layer.

---

## v0.0.8 — 2026-03-03

### Required Address Fields

Enforced required address fields across all address-bearing resources. City,
StateOrProvince, PostalCode, and Country (with resource-specific prefixes for
Member and Office) must always be present.

- **Validation**: Added `required` flag to `FieldRule` interface, with required
  address rules for Property, Member (`MemberCity`, etc.), and Office
  (`OfficeCity`, etc.)
- **Data generators**: Property now always generates `StateOrProvince` and
  `Country`; Office now generates `OfficeStateOrProvince` and `OfficeCountry`;
  Member now generates full address fields (`MemberAddress1`, `MemberCity`,
  `MemberStateOrProvince`, `MemberPostalCode`, `MemberCountry`)

### Cross-Field Validation Rules

Added relationship constraints between fields in the Property resource:

- **ListPrice >= ListPriceLow** — when both are present, ListPrice must be
  greater than or equal to ListPriceLow
- **BathroomsTotalInteger = sum of parts** — when BathroomsTotalInteger and any
  bathroom part fields (BathroomsFull, BathroomsHalf, BathroomsPartial,
  BathroomsOneQuarter, BathroomsThreeQuarter) are present, the total must equal
  the sum of the parts
- New `CrossFieldRule` interface with callback-based validators

### Data Generator Consistency

- Bathroom fields generated parts-first (BathroomsFull, BathroomsHalf, etc.),
  then BathroomsTotalInteger computed as their sum
- ListPriceLow generated as 80–100% of ListPrice

### Address Formatting Fix

Fixed USPS-format address separators in the UI:

- Added comma between City and StateOrProvince (was missing)
- Added space between StateOrProvince and PostalCode (was missing)
- Fixed StreetDirSuffix separator (was comma, should be space — it's part of
  the street line)
- Addresses now render correctly: `8653 Main Blvd, Salem, OR 45241`

### Summary Display

- Summary cards now show all configured fields in fixed order, displaying `—`
  for missing values instead of hiding empty fields

### Test Summary

| Package | Tests |
|---------|------:|
| `@reso/validation` | 91 |
| `@reso/odata-filter-parser` | 97 |
| `@reso/odata-client` | 101 |
| `@reso/data-generator` | 71 |
| `@reso/reference-server` | 76 |
| `@reso/certification-add-edit` | 49 |
| **Total** | **485** |

---

## v0.0.7 — 2026-03-03

### Business Rules Validation

Added field-specific business rules to the `@reso/validation` package in a new
`business-rules/` subfolder. Rules are enforced on both the server API (POST/PATCH)
and the UI input forms.

- **Price fields** (ListPrice, OriginalListPrice, PreviousListPrice, ClosePrice,
  ListPriceLow): must be between 0 and 1,000,000,000
- **Bedroom fields** (BedroomsTotal, BedroomsPossible, MainLevelBedrooms): must be
  between 0 and 100
- **Bathroom fields** (BathroomsTotalInteger, BathroomsFull, BathroomsHalf,
  BathroomsOneQuarter, BathroomsPartial, BathroomsThreeQuarter, MainLevelBathrooms):
  must be between 0 and 100
- Rule registry pattern (`getBusinessRules(resourceName)`) extensible to other resources
- Integrated into `validateRecord` with deduplication — fields that already have
  type-level failures skip business rule checks
- **24 new tests** in `business-rules.test.ts`

### Locale-Aware Formatting

Added `formatFieldValue` utility in the UI for locale-aware display formatting:

- **Currency fields** formatted as USD with `Intl.NumberFormat` (e.g., `$4,034,663.58`)
- **Integer fields** formatted with thousands separators (e.g., `1,234`)
- **Decimal fields** formatted with up to 2 fraction digits
- **DateTimeOffset** fields formatted with `toLocaleString()`
- **Booleans** displayed as `Yes` / `No`
- Handles both numeric and string representations from the OData API (Edm.Decimal
  values are serialized as JSON strings per OData 4.01 spec)

### Address Display

- **Summary cards**: Property records show a single composed USPS-format address line
  (e.g., `123 Main St, Springfield, IL 60601`) instead of individual address fields
- **Detail pages**: formatted address displayed in the pinned header section
- `formatAddress` assembles addresses from StreetNumber, StreetDirPrefix, StreetName,
  StreetSuffix, StreetDirSuffix, City, StateOrProvince, PostalCode; falls back to
  UnparsedAddress if structured fields are empty

### Nginx SPA Routing Fix

Fixed browser refresh on SPA routes (e.g., `/Property`) returning raw JSON instead of
the React app. The nginx config now uses the `Accept` header to distinguish API requests
(`application/json`) from browser navigation (`text/html`), using the `error_page 418`
+ named location pattern for reliable proxying.

### PostgreSQL Schema: TEXT instead of VARCHAR

Changed the schema generator to use `TEXT` for all `Edm.String` columns instead of
`VARCHAR(n)`. PostgreSQL stores TEXT and VARCHAR identically, but TEXT columns benefit
from TOAST for large values.

### Test Summary

| Package | Tests |
|---------|------:|
| `@reso/validation` | 65 |
| `@reso/odata-filter-parser` | 97 |
| `@reso/odata-client` | 101 |
| `@reso/data-generator` | 71 |
| `@reso/reference-server` | 76 |
| `@reso/certification-add-edit` | 49 |
| **Total** | **459** |

---

## v0.0.6 — 2026-03-03

### PostgreSQL Schema: TEXT instead of VARCHAR

Changed the PostgreSQL schema generator to use `TEXT` for all `Edm.String` columns
instead of `VARCHAR(n)`. PostgreSQL stores TEXT and VARCHAR identically, but TEXT
columns benefit from TOAST (Transparent Oversized-Attribute Storage), which moves
large values out-of-line and reduces the fixed-width row portion. This resolves
"row is too big" errors when populating resources with many fields (e.g., DD 2.0
Property has 652 fields).

### Data Dictionary 2.0 Metadata

Updated server metadata from DD 1.7 to DD 2.0 (1,727 fields, 3,256 lookups, 41
resources vs 1,316 fields, 2,951 lookups in 1.7).

### Data Generator Fixes

- Decimal fields now respect RESO metadata `precision`/`scale` constraints
  (e.g., NUMERIC(5,2) capped at 999.99 instead of 10,000)
- All date/time fields use ISO 8601 format (no bare time strings)
- Nullable fields randomly populated at 60% fill rate for realistic sparse records
- Docker fixes: multi-dependency builds, Alpine wget healthcheck, nginx admin/oauth routes

### Test Summary

| Package | Tests |
|---------|------:|
| `@reso/validation` | 41 |
| `@reso/odata-filter-parser` | 97 |
| `@reso/odata-client` | 101 |
| `@reso/data-generator` | 71 |
| `@reso/reference-server` | 76 |
| `@reso/certification-add-edit` | 49 |
| **Total** | **435** |

---

## v0.0.5 — 2026-03-03

### Data Generator: `@reso/data-generator`

New standalone package at `tools/data-generator/` that generates realistic RESO Data
Dictionary records for seeding OData servers with test data.

#### Three Output Modes

- **HTTP** — POSTs records directly to a running OData server via the Add/Edit API
- **JSON** — Writes records as JSON files to a directory structure (`<resource>/<key>.json`)
- **curl** — Generates a `seed.sh` script with curl commands for later execution

#### Resource Generators

Six resource-specific generators produce realistic field values:

- **Property** — addresses, pricing ($50k–$10M), bedrooms, bathrooms, coordinates (US bounds), public remarks
- **Member** — first/last name pools, email patterns, phone numbers, designations
- **Office** — brokerage names, office phones, addresses, national association IDs
- **Media** — image URLs, sequential ordering, MIME types, linked to parent via ResourceName/ResourceRecordKey
- **OpenHouse** — future-dated events with weekend preference, time ranges, linked to parent Property
- **Showing** — future-dated appointments with time ranges, instructions, linked to parent Property

A generic field generator handles all Edm types (String, Boolean, Int16/32/64, Decimal,
Date, DateTimeOffset, TimeOfDay, Guid) and enum/collection lookups from metadata.

#### CLI

Interactive mode with `@inquirer/prompts`:
```bash
npx reso-data-generator
```

Non-interactive mode with flags:
```bash
npx reso-data-generator -r Property -n 50 -f json -o ./seed-data \
  --related Media:5,OpenHouse:2,Showing:2 -t admin-token
```

#### 69 tests across 4 test files

### Auth System for Reference Server

Added role-based authentication to `@reso/reference-server`.

- **Three roles**: `read`, `write`, `admin` with hierarchy (admin > write > read)
- **Static tokens** from environment variables (`ADMIN_TOKEN`, `WRITE_TOKEN`, `READ_TOKEN`)
  with development defaults
- **Dynamic tokens** issued by the mock OAuth2 endpoint with optional `role` query parameter
- **Express middleware** (`requireAuth`) — 401 for missing/invalid token, 403 for insufficient role
- **Backward compatible** — auth is optional by default (`AUTH_REQUIRED=false`) so existing
  workflows continue without tokens
- **9 auth tests**

### Admin API and UI

#### Server: Admin Data Generator Endpoint

- `POST /admin/data-generator` — generates records using the DAL directly (requires admin role)
- `GET /admin/data-generator/status` — returns available resources with field and record counts
- Admin router mounted at `/admin` with admin auth middleware

#### UI: Data Generator Page

- Resource selector dropdown with field and record counts
- Record count input (1–10,000)
- Related record checkboxes with per-resource count inputs
- Generation plan summary showing total records to be created
- Progress indicator and results display (created/failed/duration)
- Amber-themed admin section with auth token input

### Seed Script and Docker

- `seed.sh` — executable bash script that waits for server health, then seeds
  50 Properties (with Media, OpenHouse, Showing), 20 Members, and 10 Offices
- Docker Compose updated with server healthcheck and optional seed service
  (`docker-compose --profile seed up`)

### Root Tooling

- Lefthook pre-commit hooks updated with `typecheck-data-generator` and `test-data-generator`
- Root `package.json` updated with `test:data-generator` script

### Test Summary

| Package | Tests |
|---------|------:|
| `@reso/validation` | 41 |
| `@reso/odata-filter-parser` | 97 |
| `@reso/odata-client` | 101 |
| `@reso/data-generator` | 69 |
| `@reso/reference-server` | 76 |
| `@reso/certification-add-edit` | 49 |
| **Total** | **433** |

---

## v0.0.4 — 2026-03-03

### Certification Test Runner: `@reso/certification-test-runner`

Extracted generic OData certification test infrastructure from `certification/add-edit`
into a reusable package at `tools/certification/test-runner/`. Future certification
modules (e.g., read-only, search) can import the shared framework instead of
duplicating test logic.

- **OData protocol validators** — status codes, headers (OData-Version, Location,
  EntityId, Preference-Applied), response body validators (JSON, annotations, etag,
  error format), payload echo checks
- **Reporter** — console (human-readable) and JSON output formats
- **HTTP client** — OData request wrapper delegating to `@reso/odata-client`
- **Auth helpers** — bearer token and OAuth2 Client Credentials resolution
- **Metadata helpers** — CSDL parsing, entity type lookup, payload validation
  against metadata using `@reso/validation`
- **Generic test helpers** — primary key extraction, schema assertions, scenario
  result building

### Validation Integration in Certification

`certification/add-edit` now uses `@reso/validation` for metadata-driven field
validation. The `validatePayloadAgainstMetadata()` function performs full type
checking (unknown fields, Edm type mismatches, negative numerics, MaxLength,
integer enforcement, collection/enum checks) via the shared validation package.

The certification scenario pre-flight check (`makeSchemaAssertion`) validates
field existence only — value validation is the server's responsibility and is
exercised by the failure test scenarios.

### Package Restructure

- `certification/add-edit` lib files are now thin re-exports from
  `@reso/certification-test-runner`, keeping only add-edit-specific scenario
  implementations (8 scenarios), types (PayloadSet, DeletePayload, ScenarioName),
  mock server, and CLI
- Updated build order: `test-runner` builds before `add-edit`
- Lefthook pre-commit hooks updated with `typecheck-test-runner` at priority 2

### Test Summary

| Package | Tests |
|---------|------:|
| `@reso/validation` | 41 |
| `@reso/odata-filter-parser` | 97 |
| `@reso/odata-client` | 101 |
| `@reso/reference-server` | 67 |
| `@reso/certification-add-edit` | 49 |
| **Total** | **355** |

---

## v0.0.3 — 2026-03-03

### Shared Validation Package: `@reso/validation`

Extracted field validation logic into a standalone isomorphic package at
`tools/validation/`. Both the reference server API and the React UI now import
from this single source of truth.

- **Isomorphic** — no Node.js or browser APIs; works in any JS runtime
- **Metadata-driven** field validation: unknown field detection, type checking
  for all Edm types, negative number rejection, MaxLength enforcement,
  integer-only enforcement for Int types, collection and enum validation
- **Subpath exports** — `@reso/validation` (top-level barrel) and
  `@reso/validation/metadata` (direct subpath) for future extensibility
- **41 tests** — [tests/validate.test.ts](validation/tests/validate.test.ts)
- `// TODO: Add executable business rules validation` placeholder for future
  grammar-based rules engine

### Reference Server UI

Added a React UI for the reference server at `tools/reso-reference-server/ui/`.

- **Tailwind CSS v4**, **React Router v7**, **Vite 6** dev server with proxy
- Resource browser with infinite scroll pagination (`$top`/`$skip`)
- Detail pages with fields grouped by RESO Data Dictionary "Groups" categories
- Media carousel with mock placeholder images
- Dynamic Add/Edit forms generated from RESO field metadata
- Advanced search with grouped field filters that build OData `$filter` expressions
- Delete confirmation with key prompt
- Dark mode toggle (system preference + manual override)
- Client-side validation using `@reso/validation` with per-field error display
  that clears as fields are corrected

### Improved Error Messages

- **Server API** — OData error responses now include `target` (operation name)
  at the top level, and per-field errors use human-friendly messages with
  `target` (field name) and `message` properties in the `details` array
- **UI** — Fixed client-server error mapping (`target`/`message` instead of
  `field`/`reason`), added submit error banner with auto-clear

### Test Summary

| Package | Tests |
|---------|------:|
| `@reso/validation` | 41 |
| `@reso/odata-filter-parser` | 97 |
| `@reso/odata-client` | 101 |
| `@reso/reference-server` | 67 |
| `@reso/certification-add-edit` | 49 |
| **Total** | **355** |

---

## v0.0.2 — 2026-03-02

### Developer Tooling: Biome + Lefthook

Added pre-commit hooks and a shared linter/formatter to enforce code quality across all packages.

- **[Biome](https://biomejs.dev/)** for linting and formatting — configured to match the
  [RESO certification-utils](https://github.com/RESOStandards/reso-certification-utils)
  style (single quotes, semicolons, no trailing commas, 140 char line width, LF line
  endings, arrow parens avoided)
  — [biome.json](../biome.json)
- **[Lefthook](https://github.com/evilmartians/lefthook)** for git pre-commit hooks
  — [lefthook.yml](../lefthook.yml)
- **Root `package.json`** with `lint`, `lint:fix`, and `test` convenience scripts
  — [package.json](../package.json)

#### Pre-commit Hook Flow

1. **Lint + auto-fix** — Biome checks staged `.ts` files, auto-fixes formatting and lint
   issues, and re-stages the fixed files
2. **Type check** — `tsc --noEmit` in all 4 packages, respecting the build dependency
   order (filter-parser first, then client/server/test-tool)
3. **Tests** — `vitest run` in all 4 packages (314 tests)

#### Codebase Reformatted

All 79 TypeScript source files were reformatted to the RESO standard style. All 314
tests pass after reformatting.

#### Setup

```bash
npm install           # installs biome + lefthook
npx lefthook install  # activates git hooks
```

---

## v0.0.1 — 2026-03-02

Initial release of the RESO Transport tooling suite. Introduces four interconnected
packages for building, testing, and validating OData 4.01 services that conform to
the RESO Data Dictionary and Web API specifications.

### New Packages

#### `@reso/odata-filter-parser` — [odata-filter-parser/](odata-filter-parser/)

Standalone, zero-dependency library for parsing OData `$filter` expressions into a
typed AST. Shared by both the client SDK (query validation) and the reference server
(SQL translation).

- **8 comparison operators**: `eq`, `ne`, `gt`, `ge`, `lt`, `le`, `has`, `in`
- **6 arithmetic operators**: `add`, `sub`, `mul`, `div`, `mod`, `divby`
- **28+ built-in functions**: string (`contains`, `startswith`, `endswith`, `tolower`,
  `toupper`, `trim`, `concat`, `length`, `indexof`, `substring`, `matchesPattern`),
  date/time (`year`, `month`, `day`, `hour`, `minute`, `second`, `fractionalseconds`,
  `now`, `date`, `time`, `maxdatetime`, `mindatetime`, `totaloffsetminutes`,
  `totalseconds`), math (`round`, `floor`, `ceiling`), type (`cast`, `isof`)
- **Lambda operators**: `any` and `all` with variable binding
- **Literal types**: string, number, boolean, null, date, datetimeoffset, timeofday,
  duration, guid, enum
- **97 tests** — [tests/filter-parser.test.ts](odata-filter-parser/tests/filter-parser.test.ts)

#### `@reso/odata-client` — [odata-client/](odata-client/)

OData 4.01 client SDK for TypeScript, inspired by
[Apache Olingo](https://olingo.apache.org/doc/odata4/index.html). Provides URI
building, CRUD helpers, CSDL metadata parsing/validation, query option validation,
and response parsing.

- **URI Builder** with compound keys, `$filter`, `$select`, `$orderby`, `$top`,
  `$skip`, `$count`, `$expand`, `$search`, `$compute`, `$format`
  — [src/uri/builder.ts](odata-client/src/uri/builder.ts)
- **CSDL/EDMX Parser** supporting EntityType, ComplexType, EnumType,
  NavigationProperty (with ReferentialConstraint, Partner, ContainsTarget),
  Action, Function, Singleton, EntityContainer, inheritance (BaseType/Abstract),
  OpenType, HasStream, IsFlags
  — [src/csdl/parser.ts](odata-client/src/csdl/parser.ts)
- **CSDL Validator** for structural correctness
  — [src/csdl/validator.ts](odata-client/src/csdl/validator.ts)
- **HTTP Client** with OAuth2 Client Credentials and bearer token auth
  — [src/http/client.ts](odata-client/src/http/client.ts)
- **CRUD Helpers**: `createEntity`, `readEntity`, `updateEntity` (PATCH),
  `replaceEntity` (PUT), `deleteEntity` with If-Match/If-None-Match ETag support
  — [src/crud/](odata-client/src/crud/)
- **Query Validator** checking `$filter`, `$select`, `$orderby`, `$expand` against
  CSDL metadata — [src/query/validator.ts](odata-client/src/query/validator.ts)
- **Response Parser** with `@odata.nextLink` auto-paging (`followAllPages`),
  annotation extraction, and OData error parsing
  — [src/response/parser.ts](odata-client/src/response/parser.ts)
- **Metadata Fetcher** (`fetchRawMetadata`, `fetchAndParseMetadata`)
  — [src/metadata/fetcher.ts](odata-client/src/metadata/fetcher.ts)
- **101 tests** across 5 test files
- **5 runnable examples**:
  - [Fetch a Property by key](odata-client/examples/fetch-property.ts)
  - [Query with $filter, $select, $orderby](odata-client/examples/query-with-filter.ts)
  - [Create and update a record](odata-client/examples/create-and-update.ts)
  - [Validate CSDL metadata](odata-client/examples/validate-metadata.ts)
  - [OAuth2 Client Credentials flow](odata-client/examples/oauth-flow.ts)

#### `@reso/reference-server` — [reso-reference-server/](reso-reference-server/)

Metadata-driven OData 4.01 reference server for the RESO Data Dictionary. Reads
RESO JSON metadata and dynamically generates PostgreSQL tables, OData CRUD endpoints,
EDMX metadata, and OpenAPI documentation.

- **Data Access Layer** interface abstracting persistence from query handling
  — [server/src/db/data-access.ts](reso-reference-server/server/src/db/data-access.ts)
- **PostgreSQL implementation** using LEFT JOIN + app-side grouping for `$expand`
  — [server/src/db/postgres-dal.ts](reso-reference-server/server/src/db/postgres-dal.ts)
- **MongoDB example** demonstrating batch-query pattern for document stores
  — [server/src/db/mongo-dal.example.ts](reso-reference-server/server/src/db/mongo-dal.example.ts)
- **`$filter` → SQL translation** using `@reso/odata-filter-parser` AST with
  parameterized queries (SQL injection safe)
  — [server/src/db/filter-to-sql.ts](reso-reference-server/server/src/db/filter-to-sql.ts)
- **Collection GET handler** with `$filter`, `$select`, `$orderby`, `$top`, `$skip`,
  `$count`, `$expand` support
  — [server/src/odata/handlers.ts](reso-reference-server/server/src/odata/handlers.ts)
- **Navigation property auto-detection** via RESO `ResourceName`/`ResourceRecordKey`
  FK convention
  — [server/src/odata/router.ts](reso-reference-server/server/src/odata/router.ts)
- **6 target resources**: Property, Member, Office, Media, OpenHouse, Showing
  (1,316 fields, 2,951 lookup values)
- **67 tests** across 5 test files

#### `@reso/certification-add-edit` — [certification/add-edit/](certification/add-edit/)

RESO Web API Add/Edit Endorsement (RCP-010) compliance testing tool. Sends
known-good and known-bad JSON payloads to OData servers and validates responses
against 8 Gherkin BDD certification scenarios.

- **Refactored** to use `@reso/odata-client` for HTTP, authentication, and
  CSDL metadata parsing (previously used raw `fetch` and `fast-xml-parser` directly)
- **49 tests** across 4 test files — all passing after refactoring

### Cross-Package Architecture

```
validation (zero deps)
    ├──> reso-reference-server (depends on validation + filter-parser)
    └──> reso-reference-server/ui (depends on validation)

odata-filter-parser (zero deps)
    ├──> odata-client (depends on filter-parser)
    │       └──> certification/add-edit (depends on odata-client)
    └──> reso-reference-server (depends on filter-parser)
```

- The **shared filter parser** is used by the client SDK for query validation and by
  the server for SQL WHERE clause generation — ensuring consistent `$filter` behavior
- The **data access layer** abstraction allows swapping persistence backends (Postgres,
  MongoDB, in-memory) without changing handler logic
- The **test tool refactoring** removes duplicated HTTP and metadata parsing code in
  favor of the shared client SDK

### OData 4.01 Spec Compliance

Implementation was audited against three OData 4.01 specification documents:

- [OData Protocol](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html)
- [OData URL Conventions](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html)
- [OData CSDL XML](https://docs.oasis-open.org/odata/odata-csdl-xml/v4.01/odata-csdl-xml-v4.01.html)

All high and medium priority gaps were addressed. Remaining lower-priority items are
tracked in [TODO.md](TODO.md).

### Test Summary

| Package | Tests |
|---------|------:|
| `@reso/odata-filter-parser` | 97 |
| `@reso/odata-client` | 101 |
| `@reso/reference-server` | 67 |
| `@reso/certification-add-edit` | 49 |
| **Total** | **314** |
