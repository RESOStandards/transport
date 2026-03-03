# RESO Reference Server — API

The OData 4.01 reference server for the RESO Data Dictionary. Built with Node.js, Express, TypeScript, and PostgreSQL.

## Development Setup

### Prerequisites

- Node.js 22+
- PostgreSQL 16+ (or use Docker)

### Install and Build

```bash
npm install
npm run build
```

### Start PostgreSQL

Using Docker:

```bash
docker run -d \
  --name reso-postgres \
  -e POSTGRES_USER=reso \
  -e POSTGRES_PASSWORD=reso \
  -e POSTGRES_DB=reso_reference \
  -p 5432:5432 \
  postgres:16-alpine
```

Or use the docker-compose from the parent directory.

### Run the Server

```bash
npm start
```

The server starts at `http://localhost:8080` by default.

### Run Tests

```bash
npm test
```

Tests include:
- `metadata.test.ts` — Metadata loader and helpers
- `edmx-generator.test.ts` — EDMX XML generation and fast-xml-parser compatibility
- `schema-generator.test.ts` — PostgreSQL DDL generation
- `validation.test.ts` — Request body validation
- `filter-to-sql.test.ts` — OData `$filter` to parameterized SQL WHERE translation (31 tests)

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `PORT` | `8080` | HTTP server port |
| `DATABASE_URL` | `postgresql://reso:reso@localhost:5432/reso_reference` | PostgreSQL connection string |
| `METADATA_PATH` | `./server-metadata.json` | Path to RESO metadata JSON file |
| `BASE_URL` | `http://localhost:{PORT}` | Base URL for OData annotations (Location, @odata.id, etc.) |

## Project Structure

```
server/
├── src/
│   ├── index.ts                # Entry point: load metadata, migrate DB, boot Express
│   ├── config.ts               # Environment variable parsing
│   ├── metadata/
│   │   ├── types.ts            # ResoMetadata, ResoField, ResoLookup interfaces
│   │   ├── loader.ts           # Read/parse metadata JSON, helper filters
│   │   ├── edmx-generator.ts   # JSON metadata → EDMX 4.0 XML
│   │   └── openapi-generator.ts # JSON metadata → OpenAPI 3.0 spec
│   ├── db/
│   │   ├── pool.ts             # PostgreSQL connection pool
│   │   ├── schema-generator.ts # Metadata → CREATE TABLE DDL
│   │   ├── migrate.ts          # Run DDL on startup
│   │   ├── queries.ts          # Parameterized CRUD query builders
│   │   ├── data-access.ts      # DataAccessLayer interface (abstracts persistence)
│   │   ├── postgres-dal.ts     # PostgreSQL DAL (LEFT JOIN + app-side grouping for $expand)
│   │   ├── filter-to-sql.ts    # OData $filter AST → parameterized SQL WHERE
│   │   └── mongo-dal.example.ts # MongoDB DAL sketch (non-functional demo)
│   ├── odata/
│   │   ├── router.ts           # Dynamic Express router (auto-detects navigation bindings)
│   │   ├── handlers.ts         # CRUD + collection handler factories via DAL interface
│   │   ├── headers.ts          # OData response header helpers
│   │   ├── annotations.ts      # @odata.context, @odata.id, @odata.editLink, @odata.etag
│   │   ├── errors.ts           # OData error response builder
│   │   └── validation.ts       # Request body validation against metadata
│   ├── auth/
│   │   └── mock-oauth.ts       # Mock OAuth2 Client Credentials endpoint
│   └── docs/
│       └── swagger.ts          # Swagger UI at /api-docs
├── tests/                      # Vitest tests
├── server-metadata.json        # RESO Data Dictionary v1.7 metadata
├── Dockerfile
├── package.json
├── tsconfig.json
└── vitest.config.ts
```

## OData Query Support

Collection GET endpoints (`GET /{Resource}`) support OData system query options:

| Query Option | SQL Translation |
|-------------|----------------|
| `$filter` | Parsed via `@reso/odata-filter-parser` → parameterized `WHERE` clause |
| `$select` | `SELECT` column list |
| `$orderby` | `ORDER BY` clause (validated against metadata) |
| `$top` | `LIMIT` |
| `$skip` | `OFFSET` |
| `$count` | `COUNT(*) OVER()` window function |
| `$expand` | `LEFT JOIN` with app-side row grouping |

### $filter Translation

The `$filter` query option is parsed into an AST by `@reso/odata-filter-parser` and translated to parameterized SQL. All values use `$1`, `$2`, etc. placeholders — no string interpolation.

| OData | SQL |
|-------|-----|
| `eq`, `ne`, `gt`, `ge`, `lt`, `le` | `=`, `!=`, `>`, `>=`, `<`, `<=` |
| `and`, `or`, `not` | `AND`, `OR`, `NOT` |
| `contains(f, 'v')` | `f ILIKE '%v%'` |
| `startswith(f, 'v')` | `f ILIKE 'v%'` |
| `endswith(f, 'v')` | `f ILIKE '%v'` |
| `year(f)`, `month(f)`, etc. | `EXTRACT(YEAR FROM f)` |
| `round(f)`, `floor(f)`, `ceiling(f)` | `ROUND(f)`, `FLOOR(f)`, `CEIL(f)` |

## Data Access Layer

The server uses a `DataAccessLayer` interface to abstract persistence from query handling. This allows swapping PostgreSQL for other backends.

- **`data-access.ts`** — Interface definition with `queryCollection`, `readByKey`, `insert`, `update`, `deleteByKey`
- **`postgres-dal.ts`** — PostgreSQL implementation using LEFT JOIN + app-side grouping for `$expand`
- **`mongo-dal.example.ts`** — Non-functional sketch showing how a MongoDB implementation would use batch queries instead of JOINs

### Navigation Properties ($expand)

Navigation bindings are auto-detected via the RESO convention: child tables have `ResourceName` and `ResourceRecordKey` columns. The router discovers these at startup and logs them.

## UI Configuration

The server exposes additional endpoints for the React UI:

### `GET /ui-config`

Returns the UI summary field configuration from `src/ui-config.json`. This controls which fields appear in the search results summary cards.

```json
{
  "resources": {
    "Property": {
      "summaryFields": ["ListingKey", "ListingId", "ListPrice", "BedroomsTotal", ...]
    },
    "Member": { "summaryFields": "__all__" }
  }
}
```

Use `"__all__"` to show all fields for a resource. Use an explicit array to pick specific fields.

### `GET /field-groups`

Returns RESO Data Dictionary field group mappings from `src/field-groups.json`. Used by the UI to organize detail views, forms, and advanced search into collapsible sections.

### `GET /api/metadata/fields?resource=Property`

Returns `ResoField[]` for the specified resource (JSON alternative to the EDMX `$metadata` endpoint).

### `GET /api/metadata/lookups?type=StandardStatus`

Returns `ResoLookup[]` for a specific enum type.

### `GET /api/metadata/lookups-for-resource?resource=Property`

Returns all lookup values for all enum fields in a resource as `Record<string, ResoLookup[]>`. Used by the UI to populate dropdown menus in one request.

### Mock Images

Placeholder SVG images are served from `public/images/` for the media carousel during development. These are referenced by Media records that don't have a `MediaURL`.

## Key Design Decisions

1. **Metadata-driven** — No code generation. Routes, tables, and validation are built dynamically from `server-metadata.json` at startup.
2. **DAL abstraction** — `DataAccessLayer` interface separates query handling from persistence. PostgreSQL uses LEFT JOIN; document stores can use batch queries.
3. **Shared filter parser** — `@reso/odata-filter-parser` is used by both this server (SQL translation) and `@reso/odata-client` (query validation).
4. **Parameterized queries** — No ORM. All SQL uses parameterized `{ text, values }` objects to prevent SQL injection.
5. **OData compliance** — Response format, headers, and error structure match what the RESO Web API Add/Edit test tool validates.
6. **Negative numeric = validation error** — Convention shared with the test tool: negative values in numeric fields trigger 400 responses with field-level error details.
