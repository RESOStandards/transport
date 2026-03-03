# RESO Transport Tools — Release Notes

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

#### `@reso/web-api-add-edit-test` — [web-api-add-edit-test/](web-api-add-edit-test/)

RESO Web API Add/Edit Endorsement (RCP-010) compliance testing tool. Sends
known-good and known-bad JSON payloads to OData servers and validates responses
against 8 Gherkin BDD certification scenarios.

- **Refactored** to use `@reso/odata-client` for HTTP, authentication, and
  CSDL metadata parsing (previously used raw `fetch` and `fast-xml-parser` directly)
- **49 tests** across 4 test files — all passing after refactoring

### Cross-Package Architecture

```
odata-filter-parser (zero deps)
    ├──> odata-client (depends on filter-parser)
    │       └──> web-api-add-edit-test (depends on odata-client)
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
| `@reso/web-api-add-edit-test` | 49 |
| **Total** | **314** |
