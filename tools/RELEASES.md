# RESO Transport Tools — Release Notes

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
