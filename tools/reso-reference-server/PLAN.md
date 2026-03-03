# RESO Reference Server — Implementation Plan

## Context

The `reso/transport` repo has a Web API Add/Edit compliance test tool at `tools/web-api-add-edit-test/` that validates OData CRUD operations against RCP-010 Gherkin scenarios. We need a **reference OData server** that:

1. Reads RESO's JSON metadata format (from `https://services.reso.org/metadata?view=all`)
2. Dynamically generates PostgreSQL tables and OData CRUD routes from it
3. Passes all 8 test scenarios in the existing test tool
4. Includes a React UI for browsing/editing records (Stage 2)

## Directory Structure

```
tools/reso-reference-server/
├── package.json                    # Workspace root (npm workspaces)
├── docker-compose.yml
├── README.md
├── CLAUDE.md
├── server/
│   ├── package.json
│   ├── tsconfig.json
│   ├── vitest.config.ts
│   ├── Dockerfile
│   ├── server-metadata.json        # Downloaded from services.reso.org
│   └── src/
│       ├── index.ts                # Entry: load metadata, migrate DB, boot Express
│       ├── config.ts               # Env vars (PORT, DATABASE_URL, METADATA_PATH)
│       ├── metadata/
│       │   ├── types.ts            # ResoMetadata, ResoField, ResoLookup interfaces
│       │   ├── loader.ts           # Read/parse JSON, helper filters
│       │   ├── edmx-generator.ts   # JSON metadata → EDMX XML string
│       │   └── openapi-generator.ts # JSON metadata → OpenAPI 3.0 spec
│       ├── db/
│       │   ├── pool.ts             # pg Pool singleton
│       │   ├── schema-generator.ts # Metadata → CREATE TABLE DDL
│       │   ├── migrate.ts          # Run DDL on startup
│       │   └── queries.ts          # Parameterized CRUD query builders
│       ├── odata/
│       │   ├── router.ts           # Dynamic Express router factory
│       │   ├── handlers.ts         # CRUD handler factories
│       │   ├── headers.ts          # OData response header helpers
│       │   ├── annotations.ts      # @odata.context/id/editLink/etag builders
│       │   ├── errors.ts           # OData error response builder
│       │   └── validation.ts       # Request body validation against metadata
│       ├── auth/
│       │   └── mock-oauth.ts       # /oauth/token mock endpoint
│       └── docs/
│           └── swagger.ts          # Swagger UI at /api-docs
│   └── tests/
│       ├── metadata.test.ts
│       ├── edmx-generator.test.ts
│       ├── schema-generator.test.ts
│       ├── validation.test.ts
│       └── integration.test.ts
└── ui/                             # Stage 2
    ├── package.json
    ├── vite.config.ts
    └── src/
```

## Stage 1 — Reference Server (16 steps)

### Step 1: Project Scaffolding
- Workspace root `package.json` with `workspaces: ["server", "ui"]`
- `server/package.json`: ESM, express, pg, swagger-ui-express, vitest, typescript
- `server/tsconfig.json`: ES2022, Node16, strict (mirrors web-api-add-edit-test)
- `CLAUDE.md`: copy conventions from existing tool

### Step 2: Metadata Types & Loader
- `metadata/types.ts`: `ResoMetadata`, `ResoField`, `ResoLookup`, `ResoResource`, `ResoAnnotation` interfaces
- `metadata/loader.ts`: Read JSON, plus helpers: `getFieldsForResource()`, `getLookupsForField()`, `getKeyFieldForResource()` (static map: Property→ListingKey, Member→MemberKey, etc.)

### Step 3: Download server-metadata.json
- Fetch `https://services.reso.org/metadata?view=all`, save as `server/server-metadata.json`

### Step 4: Database Schema Generator
- `db/pool.ts`: pg Pool from `DATABASE_URL`
- `db/schema-generator.ts`: Pure function, metadata → CREATE TABLE DDL

| Edm Type | SQL Type |
|----------|----------|
| Edm.String | TEXT (or VARCHAR(maxLength)) |
| Edm.Int64 | BIGINT |
| Edm.Int32 | INTEGER |
| Edm.Decimal | NUMERIC(precision, scale) |
| Edm.Boolean | BOOLEAN |
| Edm.Date | DATE |
| Edm.DateTimeOffset | TIMESTAMPTZ |
| Enum (string) | TEXT |
| Enum (int) | INTEGER |
| Collection | JSONB |

- `db/migrate.ts`: Run DDL on startup, idempotent with `CREATE TABLE IF NOT EXISTS`

### Step 5: CRUD Query Builders
- `db/queries.ts`: `buildInsertQuery`, `buildSelectByKeyQuery`, `buildUpdateQuery`, `buildDeleteQuery`
- All parameterized (`$1`, `$2`, etc.), no string interpolation of user values
- Collection fields serialize as JSONB

### Step 6: OData Response Helpers
- `odata/headers.ts`: `setODataHeaders()` — OData-Version, Location, EntityId, Preference-Applied
- `odata/annotations.ts`: `buildAnnotations()` — @odata.context, @odata.id, @odata.editLink, @odata.etag
- `odata/errors.ts`: `buildODataError()` — error.code, error.message, error.details[].target/.message

### Step 7: Request Validation
- `odata/validation.ts`: Validate body against metadata fields
- Unknown fields → 400 with target in error.details
- Negative numeric values → 400 (matches test tool convention)
- Type mismatches → 400

### Step 8: CRUD Route Handlers
- `odata/handlers.ts`: `createHandler`, `readHandler`, `updateHandler`, `deleteHandler` factories
- Create: POST, generate UUID key, insert, return 201 (representation) or 204 (minimal)
- Read: GET by key, return 200 or 404
- Update: PATCH, merge semantics, return 200 or 204
- Delete: DELETE, return 204 or 404
- All set proper OData headers/annotations per existing test tool validators

### Step 9: Dynamic Router
- `odata/router.ts`: Takes metadata + pool, registers routes for all target resources
- OData key syntax via regex: `/{Resource}('key')`

### Step 10: EDMX XML Generator
- `metadata/edmx-generator.ts`: Generate valid EDMX 4.0 XML from JSON metadata
- Must be parseable by `fast-xml-parser` with the same options as `web-api-add-edit-test/src/lib/metadata.ts`
- String templating (no XML library needed)

### Step 11: Mock OAuth Endpoint
- `auth/mock-oauth.ts`: POST /oauth/token → `{ access_token, token_type, expires_in }`

### Step 12: OpenAPI + Swagger UI
- `metadata/openapi-generator.ts`: Generate OpenAPI 3.0 spec from metadata
- `docs/swagger.ts`: Mount swagger-ui-express at /api-docs

### Step 13: Server Entry Point
- `config.ts`: PORT (8080), DATABASE_URL, METADATA_PATH, BASE_URL
- `index.ts`: Load metadata → create pool → migrate → build router → mount middleware → listen

### Step 14: Docker
- `server/Dockerfile`: Multi-stage build (node:22-alpine)
- `docker-compose.yml`: server + postgres:16-alpine + pgdata volume

### Step 15: Tests
- Unit tests for metadata loader, EDMX generator, schema generator, validation
- Integration test: full CRUD cycle against real PostgreSQL

### Step 16: README Files
- Top-level README: overview, docker-compose quickstart, architecture
- server/README: dev setup, env vars, testing

## Stage 2 — React UI + Seed Data

### Step 17: Seed Script (`server/src/seed.ts`)
- 10 records per resource, ~80% field fill
- Enum fields: random valid lookup value from metadata
- Media: 2-3 per Property with ResourceName/ResourceRecordKey links, sample image URLs
- Run via `npm run seed`

### Step 18-19: React UI (`ui/`)
- Vite + React + TypeScript
- Resource browser, table view, detail view, create/edit forms
- Vite proxy to server at localhost:8080

## Key Decisions

1. **Metadata-driven** — no code generation; routes/tables built dynamically at startup
2. **Raw pg queries** — no ORM; pure functions returning `{ text, values }`
3. **Compatibility = acceptance criterion** — all 8 test scenarios must pass
4. **Negative numeric = validation error** — matches existing test tool convention
5. **ETag format: `W/"<base64-timestamp>"`** — matches existing mock server
6. **Static key field map** — RESO standard keys (ListingKey, MemberKey, etc.) hardcoded
7. **No FK constraints** — reference server is intentionally simple; Media linking is app-level

## Critical Reference Files
- `web-api-add-edit-test/src/mock/handlers.ts` — OData response format the test tool expects
- `web-api-add-edit-test/src/lib/validators.ts` — Exact compliance checks to satisfy
- `web-api-add-edit-test/src/lib/test-runner.ts` — 8 scenario request/response flows
- `web-api-add-edit-test/src/lib/metadata.ts` — EDMX parsing (our generator must match)

## Verification

1. `npm test` — all unit and integration tests pass
2. Cross-tool validation:
```bash
cd tools/web-api-add-edit-test
npx testWebApiAddEdit \
  --url http://localhost:8080 \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token test
```
All 8 scenarios must pass.
