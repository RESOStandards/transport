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

### #12 — CI/CD Pipeline
Set up GitHub Actions for:
- Build all packages in dependency order
- Run all test suites
- Cross-tool validation (reference server + test tool)
- Publish packages to npm (when ready)

### ~~#13 — Migrate TODOs to GitHub Issues~~
~~Move items from this file into GitHub Issues with proper labels,
milestones, and assignees once the repository is public.~~
Tracking with ticket numbers in this file for now.
