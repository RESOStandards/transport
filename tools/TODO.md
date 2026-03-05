# RESO Transport Tools — TODO

Track outstanding requirements and future work items. These will be migrated
to GitHub Issues once the repository is ready for public collaboration.

---

## High Priority

### ~~#1 — Data Access Layer: MongoDB Backend~~
**Package:** `reso-reference-server`
**Status:** Closed

~~The server defines a `DataAccessLayer` interface. Currently only the
PostgreSQL implementation exists. Add MongoDB as a fully functional
alternative backend, selectable via `DB_BACKEND` environment variable.~~

- ~~`filter-to-mongo.ts` — OData $filter AST to MongoDB query translator (33 tests)~~
- ~~`mongo-dal.ts` — Production MongoDB DAL adapter~~
- ~~`mongo-init.ts` — Collection and index setup~~
- ~~Config/startup updates for backend selection~~
- ~~Docker Compose profile for MongoDB~~
- ~~Documentation: server README, reference-server README, module READMEs~~

### ~~#16 — UI: Fixed Header/Sidebar and Request URI Fix~~
**Package:** `reso-reference-server/ui`
**Status:** Closed

~~Header and left navigation sidebar now stay fixed while only the main content
area scrolls. Also fixed "Request URI too large" error caused by sending all
field names in `$select` for `__all__` resources, and increased nginx header
buffer limit to 32k for complex OData queries.~~

~~Affected files:~~
- ~~`ui/src/components/layout.tsx`~~
- ~~`ui/src/pages/search-page.tsx`~~
- ~~`ui/nginx.conf.template`~~

### ~~#15 — UI: Detail Page Layout — Summary Pane + Media Carousel~~
**Package:** `reso-reference-server/ui`
**Status:** Closed

~~Redesign the detail page layout so the media carousel takes half the horizontal
space on the right, with a summary pane on the left. For Property (which has
groupings), the left pane shows address, key, timestamp, and configured summary
fields; grouped field sections appear below. For resources without groupings,
all fields display alphabetically in a two-column layout beside the carousel.~~

~~Affected files:~~
- ~~`ui/src/pages/detail-page.tsx`~~

### ~~#14 — UI: Remove "Other" Group When No Groupings Are Defined~~
**Package:** `reso-reference-server/ui`
**Status:** Closed

~~When a resource has no field groupings defined in the Data Dictionary Google Sheet
(e.g., Member, Office, Media, OpenHouse, Showing), all fields end up in a
collapsible "Other" section. Instead, display them as a flat alphabetical list
without any group wrapper. Only use the "Other" section when a resource HAS
groupings defined (like Property) and some fields fall outside those groups.~~

~~Affected files:~~
- ~~`ui/src/pages/detail-page.tsx`~~
- ~~`ui/src/components/record-form.tsx`~~
- ~~`ui/src/components/advanced-search.tsx`~~

### #2 — Multi-Level $expand Support
**Package:** `reso-reference-server`, `odata-client`

Currently only single-level `$expand` is implemented (e.g., `$expand=Media`).
Multi-level expansion like `$expand=Media($expand=Tags)` needs:

1. Parse nested `$expand` within parenthesized options (client-side parser
   already handles the syntax via `splitExpandParts`)
2. Server-side: recursively resolve navigation properties for each level
3. For RDBMS backends: chain LEFT JOINs or issue sub-queries per navigation
   level, then nest results in app-side grouping
4. For document store backends: resolve nested references by following
   `ResourceName`/`ResourceRecordKey` pairs at each level
5. Respect `$levels=N` and `$levels=max` parameters
6. Add depth limit configuration to prevent unbounded recursive expansion

### #3 — Data Access Layer: Additional Backends
**Package:** `reso-reference-server`

Future backends to consider beyond PostgreSQL and MongoDB:
- SQLite (lightweight local testing)
- In-memory (unit testing without database)

---

## Medium Priority

### #4 — Batch Operations (`$batch`)
**Package:** `odata-client`, `reso-reference-server`

OData 4.01 supports batch requests via `POST /$batch` in both
multipart/mixed and JSON formats. Implement:
- Client: `batchRequest(client, requests[])` helper
- Server: `$batch` endpoint with changeset (atomic group) support
- JSON batch format (preferred for 4.01)

### #5 — Asynchronous Requests
**Package:** `odata-client`, `reso-reference-server`

`Prefer: respond-async` for long-running operations:
- Client: detect `202 Accepted`, poll status monitor URL
- Server: queue handler, status monitor endpoint, `Retry-After` header

### #6 — Delta Responses / Change Tracking
**Package:** `odata-client`, `reso-reference-server`

`Prefer: odata.track-changes` with `@odata.deltaLink`:
- Client: `followDelta(client, deltaLink)` helper
- Server: track changes via database triggers or logical timestamps,
  return delta payloads with added/changed/deleted entities

### #7 — Type Casting in URLs
**Package:** `odata-client`, `odata-filter-parser`

Support `EntitySet/Namespace.DerivedType` path segments for filtering
to derived entity types. Requires parser updates for type-qualified
segments.

---

## Lower Priority

### #8 — Geo Functions and Types
**Package:** `odata-filter-parser`, `odata-client`

`geo.distance()`, `geo.intersects()`, `geo.length()` filter functions
and `Edm.Geography*`/`Edm.Geometry*` types. Not commonly used in RESO
but part of the OData 4.01 spec.

### #9 — TypeDefinition Parsing
**Package:** `odata-client`

CSDL `TypeDefinition` elements (named aliases for primitive types with
constrained facets). Parse and validate in CSDL parser.

### #10 — Annotation Parsing
**Package:** `odata-client`

Full `Annotation` element parsing from CSDL XML, including vocabulary
references (`edmx:Reference`/`edmx:Include`) and standard vocabularies
(Core, Capabilities, Validation, Measures).

### #11 — $apply (Aggregation)
**Package:** `odata-filter-parser`, `reso-reference-server`

OData Data Aggregation Extension: `groupby`, `aggregate`, `filter`,
`compute` transformations. Complex feature — implement when needed.

---

## Infrastructure

### #17 — Compliance Testing: Docker Compose + GitHub Actions
**Package:** `reso-reference-server`

Add external RESO compliance tools as Docker Compose services that run against the
reference server. Both tools test against both Postgres and MongoDB backends with
bearer token and client credentials auth.

**Tools:**
1. **Data Dictionary 2.0** (`reso-certification-utils` v3.0.0) — Node.js + embedded Java.
   Validates metadata, field mappings, data availability, JSON schemas.
2. **Web API Core 2.0.0** (`web-api-commander`) — Java/Gradle. Validates OData queries,
   filtering, sorting, metadata compliance. Needs a RESOScript config per resource.

**Deliverables:**
- `compliance/` directory with Dockerfiles, configs, entrypoint scripts
- Dynamic RESOScript generation from live server data (per resource)
- 4 Docker Compose services (DD + Core × Postgres + MongoDB)
- Fix mock OAuth endpoint for urlencoded body parsing
- GitHub Actions workflow (`.github/workflows/compliance.yml`)

**Target resources:** Property, Member, Office, Media, OpenHouse, Showing,
PropertyGreenVerification, PropertyPowerProduction, PropertyRooms, PropertyUnitTypes

### ~~#18 — Fix DD 2.0 Compliance Test Failures~~
**Package:** `reso-reference-server`
**Status:** Closed

~~DD 2.0 compliance tests now pass: 928 passed, 676 skipped, 0 variations.~~

**Fixes applied:**
- ~~Replaced `server-metadata.json` with `reso-certification-etl` DD 2.0 reference (fixes 6 `ID`→`Id` casing issues)~~
- ~~Fixed expansion fields (HistoryTransactional, SocialMedia) returned without `$expand` — both PostgreSQL and MongoDB DALs~~
- ~~Fixed data generator non-DD fields: removed MimeType, OpenHouse/Showing ResourceName/ResourceRecordKey, fixed Showing field names to DD 2.0 (ShowingStartTimestamp, ShowingEndTimestamp)~~
- ~~Fixed DateTimeOffset fractional seconds in filter parser (replication phase `$filter` queries)~~

**Remaining post-test issue:** Client credentials replication requires HTTPS tokenUri (tracked in #20)

### ~~#22 — UI: Auto-Expand Field Groups Containing Validation Errors~~
**Package:** `reso-reference-server/ui`
**Status:** Closed

~~When editing a Property record (or any resource with field groups), validation errors
may be inside collapsed group sections. The error banner says "Please fix the 2 field
errors highlighted below" but the user has to manually open each group to find which
fields have errors.~~

~~**Fix:** When validation errors occur (client-side or server-side), automatically expand
any `FieldGroupSection` that contains a field with an error. Groups without errors
can remain in their current collapsed/expanded state.~~

~~Affected files:~~
- ~~`ui/src/components/record-form.tsx` — pass error field names to determine which groups should be open~~
- ~~`ui/src/components/field-group-section.tsx` — accept a `forceOpen` prop or similar to override collapsed state~~

~~**Note:** Only applies to resources with field groups (e.g., Property). Resources without
groups display fields in a flat list and are unaffected.~~

### ~~#23 — UI: Pin Search Toolbar and Detail Header~~
**Package:** `reso-reference-server/ui`
**Status:** Closed

~~On search pages, the resource title, search bar, sort buttons, and result count
now stay pinned at the top while only the result cards scroll. On detail pages,
the back link, title, and Edit/Delete buttons stay pinned while the content scrolls.
Each page manages its own scroll container since `<main>` is now `overflow-hidden`.~~

~~Affected files:~~
- ~~`ui/src/components/layout.tsx` — `<main>` changed to `overflow-hidden`, padding removed~~
- ~~`ui/src/pages/search-page.tsx` — flex layout with pinned toolbar + scrollable results~~
- ~~`ui/src/components/results-list.tsx` — count display moved to search page~~
- ~~`ui/src/pages/detail-page.tsx` — flex layout with pinned header + scrollable content~~
- ~~`ui/src/pages/add-page.tsx`, `edit-page.tsx`, `delete-page.tsx`, `not-found-page.tsx`, `admin-layout.tsx` — scroll wrappers + padding~~

### ~~#24 — Data Generator Improvements~~
**Package:** `data-generator`, `reso-reference-server`
**Status:** Closed

~~Referentially correct multi-resource seed data with FK resolution, dependency
graph, cycle breaking (Office ↔ Member), three output modes (HTTP/JSON/curl),
CLI `--deps` flag, server `resolveDependencies` endpoint, and UI integration.
Docker-verified on both PostgreSQL and MongoDB (892 records, 0 errors each).~~

~~Sub-tasks 1–6 complete. Sub-task 7 (HistoryTransactional as child collection)
deferred to a future ticket.~~

### ~~#25 — UI: Expansion Cards for Navigation Properties~~
**Package:** `reso-reference-server/ui`
**Status:** Closed

~~Inset cards in a collapsible "Related Records" section display field-value data
for expanded navigation properties. To-one expansions show a single card with
"View" link; to-many show paginated cards with back/next navigation. Each card
uses a two-column, vertically scrollable field layout (max 4 rows). Also fixed
nginx proxy missing Teams, TeamMembers, and OUID resources.~~

~~Affected files:~~
- ~~`ui/src/components/expanded-entity-card.tsx` — new component~~
- ~~`ui/src/pages/detail-page.tsx` — filter expansions, render in Related Records section~~
- ~~`ui/nginx.conf.template` — add Teams, TeamMembers, OUID to proxy allowlist~~

### #26 — RESOScript Generation: Allow Null Parameters for Partial Resources
**Package:** `reso-reference-server`

Currently, if a resource cannot fill every required data type parameter (Integer,
Decimal, Date, Timestamp, SingleValueLookup, MultipleValueLookup), it is skipped
entirely during Web API Core compliance testing. Only Property has all types.

Allow partial RESOScript generation so resources with some missing types can still
run the tests they support (e.g., Member has timestamps and lookups but no integers).
Requires either:
- Conditional parameter omission with commander-side graceful skipping
- Multiple RESOScript variants per resource (one per supported test subset)
- Upstream commander changes to skip tests when parameters are blank

### #27 — RESOScript Generation: OData Edm.EnumType Namespace Support
**Package:** `reso-reference-server`

The RESOScript generator currently sets `SingleValueLookupNamespace` and
`MultipleValueLookupNamespace` to empty strings because the RESO Data Dictionary
uses string enums (`Edm.String` with `LookupName` annotations) and the commander
is invoked with `-DuseStringEnums=true`.

When the server adds support for OData `Edm.EnumType` typed enums, the generator
must resolve the full enum namespace (e.g., `org.reso.metadata.enums.StandardStatus`)
from `$metadata` and populate these parameters accordingly.

### ~~#19 — Fix Web API Core 2.0.0 Compliance Test Failures~~
**Package:** `reso-reference-server`
**Status:** Closed

~~All 42 applicable Web API Core 2.0.0 tests pass (3 skipped: `has` operator,
N/A for string enumerations). Fixes: PostgreSQL numeric coercion, Edm.Date
truncation, lambda any()/all() SQL + MongoDB translation, RESOScript generator
improvements (date format, timestamp field selection, integer median, multi-value
lookup population), fractional seconds in filter parser, service document endpoint.~~

### #20 — Client Credentials Compliance: HTTPS Token URI
**Package:** `reso-reference-server`

The `reso-certification-utils` client credentials replication phase requires
`tokenUri` to be a valid HTTPS URL. The mock OAuth endpoint uses HTTP.

**Options:**
1. Add a self-signed TLS certificate to the Docker Compose setup
2. Configure the compliance tool to skip HTTPS validation
3. Use a reverse proxy (nginx/traefik) with TLS termination

### #21 — Use services.reso.org/metadata as Default Metadata Source
**Package:** `reso-reference-server`

Currently the server uses a bundled `server-metadata.json` from `reso-certification-etl`.
Once `services.reso.org/metadata` is synced with the DD 2.0 reference, switch to
fetching metadata from that endpoint at startup (with the bundled file as fallback).

### #12 — CI/CD Pipeline
Set up GitHub Actions for:
- Build all packages in dependency order
- Run all test suites
- Cross-tool validation (reference server + test tool)
- Publish packages to npm (when ready)

### #29 — Add RESO Lookup Resource Support
**Package:** `reso-reference-server`

The server uses `Edm.String` for enumerations (with `LookupName` annotations) rather
than OData `Edm.EnumType`. To pass DD 2.0 compliance tests, the server must expose
the RESO Lookup Resource — an OData entity set that serves lookup metadata so that
compliance tools can validate enumeration values.

**Deliverables:**
- Lookup entity set in `$metadata` (EDMX)
- `/Lookup` endpoint with `$filter` support (by `LookupName`, `LookupValue`, etc.)
- Seed lookup data from the bundled metadata file
- Service document updated to include Lookup

**Priority:** Before SQLite DAL implementation (#3).

### #28 — Rewrite Web API Core Testing Tools
**Package:** `reso-reference-server`

Rewrite the current Web API Core compliance testing tools. Current approach uses
the RESO `web-api-commander` Java/Gradle tool via Docker. The new approach will
implement tests natively. The Cucumber spec for Web API Core 2.0.0 will serve as
the reference for test scenarios.

**Prerequisite:** All current Commander-based Web API Core tests must pass first
(ticket #19) before rewriting.

### ~~#13 — Migrate TODOs to GitHub Issues~~
~~Move items from this file into GitHub Issues with proper labels,
milestones, and assignees once the repository is public.~~
Tracking with ticket numbers in this file for now.
