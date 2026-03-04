# RESO Transport Tools — Release Notes

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

**Modified files:**

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
