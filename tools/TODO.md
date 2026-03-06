# RESO Transport Tools — TODO

Track outstanding requirements and future work items. These will be migrated
to GitHub Issues once the repository is ready for public collaboration.

---

## High Priority

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

### #44 — EntityEvent (RCP-027) Compliance Testing Tool
**Package:** `certification`

Native TypeScript compliance testing tool for the RESO EntityEvent Resource (RCP-027).
Tests change tracking via monotonically increasing sequence numbers.

**Two modes:**
- **Observe** — read-only, for third-party servers. Polls for new events with configurable timeout.
- **Full** — write access, for reference server. Creates/updates/deletes a canary resource and
  verifies corresponding EntityEvent records appear.

**Scenarios (11 total):**
- Both modes: metadata-valid, read-only-enforced, event-structure, sequence-monotonic,
  query-filter, query-orderby-top-skip, query-count, incremental-sync
- Full mode only: create-triggers-event, update-triggers-event, delete-triggers-event

**Features:**
- Batch data validation: fetches records referenced by EntityEvent entries using OData `in` operator,
  validates against `$metadata` using lightweight Edm type checker
- `--strict` flag: unknown fields are failures (default: warnings)
- Compliance report generation with per-scenario details and data validation summary
- Mock Express server for test isolation (same pattern as Add/Edit tool)
- CLI subcommand: `reso-cert entity-event`

**Status:** Implementation complete. 42 new tests (30 edm-validator + 6 runner + 6 compliance-report).

### #28 — Rewrite Web API Core Testing Tools
**Package:** `certification`

Rewrite the current Web API Core compliance testing tools. Current approach uses
the RESO `web-api-commander` Java/Gradle tool via Docker. The new approach will
implement tests natively in TypeScript using the existing `@reso/certification-test-runner`
framework (same pattern as the Add/Edit tool in `certification/add-edit/`).

**Multi-step plan:**
1. Parse the Cucumber `.feature` files from Web API Core 2.0.0 spec into structured test scenarios
2. Implement OData query validation scenarios (filter, orderby, select, expand, top, skip, count)
3. Implement metadata validation scenarios (EDMX parsing, entity type checks)
4. Implement response validation (OData headers, annotations, error format)
5. RESOScript-equivalent config generation (auto-detect fields/types from metadata)
6. Report generation matching existing compliance JSON format
7. Docker Compose integration replacing the `web-api-commander` service

**Prerequisite:** All current Commander-based Web API Core tests must pass first
(ticket #19, done). Add/Edit tool (#41) serves as the architectural template.

### #42 — Rewrite Data Dictionary Testing Tools
**Package:** `certification`

Rewrite the current DD 2.0 compliance testing tools. Current approach uses
`reso-certification-utils` (Node.js + embedded Java) via Docker. The new approach
will implement tests natively in TypeScript.

**Multi-step plan:**
1. Enumerate DD 2.0 test categories: metadata validation, field presence, data type checks,
   lookup value validation, schema validation, data availability/replication
2. **EDMX→JSON Schema generator** — Convert parsed `$metadata` EntityType definitions to
   JSON Schema documents (Edm type→JSON Schema type mapping, nullable, maxLength, precision,
   Collection→array, EnumType→enum). Use `ajv` for validation. This replaces the lightweight
   Edm type checker used in EntityEvent compliance and becomes the shared schema validation
   layer for all compliance tools.
3. Implement metadata-driven field checks against the DD 2.0 reference spreadsheet
4. Implement data replication validation (fetch records, validate against JSON schemas)
5. Implement lookup value validation (enum values match DD 2.0 reference)
6. Report generation matching existing DD compliance JSON format
7. Docker Compose integration replacing the `reso-certification-utils` service

**Prerequisite:** All current DD compliance tests must pass first (#18/#32, done).
Web API Core rewrite (#28) should be completed first as a simpler starting point.

### #31 — OData 4.01 `ne` Operator: Null Handling Spec Compliance
**Package:** `reso-reference-server`

The OData 4.01 spec (Part 2, Section 5.1.1.1) defines that `null ne 'value'` evaluates
to **true** — records with null fields should be **included** in `ne` filter results.
Both backends currently exclude them.

**Full analysis:** See `tools/RESEARCH.md`

**Current behavior (both backends exclude nulls):**
- MongoDB: `{ field: { $nin: [value, null] } }` — explicitly excludes null (added in #29)
- PostgreSQL: `field != $1` — SQL `NULL != value` → NULL → excluded by WHERE clause

**OData 4.01 spec behavior (nulls should be included):**
- MongoDB fix: revert to `{ field: { $ne: value } }` (original behavior was correct)
- PostgreSQL fix: use `field IS DISTINCT FROM $1` or `(field != $1 OR field IS NULL)`

**Complication:** The RESO `web-api-commander` compliance tool has a bug — it crashes
with a NullPointerException when null values appear in `ne` results:
```
Cannot invoke "Object.toString()" because the return value of
"java.util.HashMap.get(Object)" is null
```
This means spec-compliant `ne` behavior will cause commander test failures for
`filter-*-ne` tests, which is a commander bug, not a server bug.

**Decision needed:**
1. Follow the OData 4.01 spec (correct, commander tests fail due to commander bug)
2. Keep SQL semantics (commander tests pass, but `ne` doesn't match the spec)

**Other audit findings (all correct):**
All other comparison operators (`eq`, `gt`, `ge`, `lt`, `le`), logical operators
(`and`, `or`, `not`), null comparisons (`eq null`, `ne null`), lambda expressions,
functions, and arithmetic match the OData 4.01 spec in both backends.

---

## UI

### #33 — Bug: Scrolling Blocked When Advanced Search Is Active
**Package:** `reso-reference-server/ui`

When the Advanced Search bar is open, the main content area scroll is blocked.
The user cannot scroll through results while the search panel is visible.

### #34 — Improvement: Pin Action Buttons to Bottom of Content Pane
**Package:** `reso-reference-server/ui`

Action buttons (Save, Cancel, Delete, etc.) currently scroll with the content
pane. They should be pinned to the bottom so they remain visible regardless of
scroll position.

### #35 — Improvement: Natural Language Filter Display + Edit Icon
**Package:** `reso-reference-server/ui`

Display the current OData `$filter` expression as a human-readable natural
language summary (e.g., "ListPrice greater than $500,000 and City equals
'Austin'"). Add an icon/button to switch to the raw OData filter expression
editor.

### #36 — Improvement: Add Basic Search
**Package:** `reso-reference-server/ui`

Add a basic search mode in addition to the existing advanced search. Basic
search should provide a simple text input that searches across common fields
(address, name, key, etc.) without requiring the user to build OData filter
expressions.

### #37 — Improvement: "Errors Only" Filter for Add/Edit Forms
**Package:** `reso-reference-server/ui`

When adding or editing records, provide an "Errors Only" toggle/filter that
hides fields without validation errors, making it easier to find and fix
problems in large forms.

### #38 — Improvement: Business Rule Handling + Disable Option
**Package:** `reso-reference-server/ui`, `validation`

Improve business rule display and handling in the UI. Add the ability to
disable business rules when needed (e.g., for testing or data import scenarios
where strict validation is not desired).

### #39 — Improvement: HistoryTransactional Writer
**Package:** `data-generator`, `reso-reference-server`

Create a HistoryTransactional data writer that generates historical transaction
records for properties. This was deferred from #24 (sub-task 7).
