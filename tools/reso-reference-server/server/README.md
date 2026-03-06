# RESO Reference Server — API

The OData 4.01 reference server for the RESO Data Dictionary. Built with Node.js, Express, and TypeScript. Supports PostgreSQL, MongoDB, and SQLite backends, selectable via environment variable.

## Development Setup

### Prerequisites

- Node.js 22+
- PostgreSQL 16+, MongoDB 7+, or SQLite (bundled with `better-sqlite3`, no install needed)

### Install and Build

```bash
npm install
npm run build
```

### Start with PostgreSQL (default)

```bash
docker run -d \
  --name reso-postgres \
  -e POSTGRES_USER=reso \
  -e POSTGRES_PASSWORD=reso \
  -e POSTGRES_DB=reso_reference \
  -p 5432:5432 \
  postgres:16-alpine

npm start
```

### Start with MongoDB

```bash
docker run -d \
  --name reso-mongo \
  -p 27017:27017 \
  mongo:7

DB_BACKEND=mongodb MONGODB_URL=mongodb://localhost:27017/reso_reference npm start
```

### Start with SQLite

No external database needed — SQLite stores data in a local file:

```bash
DB_BACKEND=sqlite npm start
```

This creates `reso_reference.db` in the server directory. To use a custom path:

```bash
DB_BACKEND=sqlite SQLITE_DB_PATH=/tmp/reso.db npm start
```

Or use Docker Compose from the parent directory (see [Docker instructions](../README.md)).

### Run the Server

```bash
npm start
```

The server starts at `http://localhost:8080` by default.

### Run Tests

```bash
npm test
```

198 tests across 10 test files:
- `metadata.test.ts` — Metadata loader and helpers
- `edmx-generator.test.ts` — EDMX XML generation, EntityContainer, NavigationProperty
- `schema-generator.test.ts` — PostgreSQL DDL generation
- `sqlite-schema-generator.test.ts` — SQLite DDL generation and type mapping
- `validation.test.ts` — Request body validation with business rules
- `navigation.test.ts` — Navigation property bindings, $expand, expansion field filtering
- `auth.test.ts` — Authentication and authorization middleware
- `filter-to-sql.test.ts` — OData `$filter` to parameterized SQL WHERE translation (31 tests)
- `filter-to-mongo.test.ts` — OData `$filter` to MongoDB query translation (33 tests)
- `filter-to-sqlite.test.ts` — OData `$filter` to parameterized SQLite WHERE translation (32 tests)

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `PORT` | `8080` | HTTP server port |
| `DB_BACKEND` | `postgres` | Database backend: `postgres`, `mongodb`, or `sqlite` |
| `DATABASE_URL` | `postgresql://reso:reso@localhost:5432/reso_reference` | PostgreSQL connection string (used when `DB_BACKEND=postgres`) |
| `MONGODB_URL` | `mongodb://localhost:27017/reso_reference` | MongoDB connection string (used when `DB_BACKEND=mongodb`) |
| `SQLITE_DB_PATH` | `./reso_reference.db` | SQLite database file path (used when `DB_BACKEND=sqlite`) |
| `METADATA_PATH` | `./server-metadata.json` | Path to RESO metadata JSON file |
| `BASE_URL` | `http://localhost:{PORT}` | Base URL for OData annotations (Location, @odata.id, etc.) |
| `ENUM_MODE` | `string` | Enumeration mode: `string` (Edm.String + Lookup Resource) or `enum-type` (Edm.EnumType definitions) |

## Project Structure

```
server/
├── src/
│   ├── index.ts                # Entry point: load metadata, select backend, boot Express
│   ├── config.ts               # Environment variable parsing (DB_BACKEND, DATABASE_URL, MONGODB_URL, SQLITE_DB_PATH)
│   ├── metadata/
│   │   ├── types.ts            # ResoMetadata, ResoField, ResoLookup interfaces
│   │   ├── loader.ts           # Read/parse metadata JSON, helper filters
│   │   ├── edmx-generator.ts   # JSON metadata → EDMX 4.0 XML
│   │   └── openapi-generator.ts # JSON metadata → OpenAPI 3.0 spec
│   ├── db/
│   │   ├── data-access.ts      # DataAccessLayer interface (abstracts persistence)
│   │   ├── postgres-dal.ts     # PostgreSQL adapter (CTE pagination + LEFT JOIN for $expand)
│   │   ├── filter-to-sql.ts    # OData $filter AST → parameterized SQL WHERE
│   │   ├── mongo-dal.ts        # MongoDB adapter (cursor pagination + batch expand)
│   │   ├── filter-to-mongo.ts  # OData $filter AST → MongoDB query documents
│   │   ├── mongo-init.ts       # MongoDB collection and index setup
│   │   ├── sqlite-dal.ts       # SQLite adapter (CTE pagination + LEFT JOIN, same as PostgreSQL)
│   │   ├── filter-to-sqlite.ts # OData $filter AST → parameterized SQLite WHERE
│   │   ├── sqlite-pool.ts      # SQLite connection handle (WAL mode, REGEXP function)
│   │   ├── sqlite-schema-generator.ts # Metadata → CREATE TABLE DDL (SQLite)
│   │   ├── pool.ts             # PostgreSQL connection pool
│   │   ├── schema-generator.ts # Metadata → CREATE TABLE DDL (PostgreSQL)
│   │   ├── migrate.ts          # Run DDL on startup (PostgreSQL)
│   │   └── queries.ts          # Parameterized CRUD query builders (PostgreSQL)
│   ├── odata/
│   │   ├── router.ts           # Dynamic Express router (auto-detects navigation bindings)
│   │   ├── handlers.ts         # CRUD + collection handler factories via DAL interface
│   │   ├── headers.ts          # OData response header helpers
│   │   ├── annotations.ts      # @odata.context, @odata.id, @odata.editLink, @odata.etag
│   │   ├── errors.ts           # OData error response builder
│   │   └── validation.ts       # Request body validation against metadata
│   ├── auth/
│   │   ├── middleware.ts       # Bearer token auth middleware (read/write/admin roles)
│   │   └── mock-oauth.ts       # Mock OAuth2 Client Credentials endpoint
│   ├── admin/
│   │   └── data-generator.ts   # Admin data generator endpoint (resolveDependencies support)
│   └── docs/
│       └── swagger.ts          # Swagger UI at /api-docs
├── tests/                      # Vitest tests
├── server-metadata.json        # RESO Data Dictionary 2.0 metadata (1,727 fields, 3,634 lookups)
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

### $filter Translation (PostgreSQL)

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

The server uses a `DataAccessLayer` interface to abstract persistence from query handling. The backend is selected at startup via the `DB_BACKEND` environment variable.

**Interface** (`data-access.ts`): `queryCollection`, `readByKey`, `insert`, `update`, `deleteByKey`

**Adapters:**

| Adapter | Backend | `$expand` Strategy | `$filter` | Schema |
|---------|---------|-------------------|-----------|--------|
| `postgres-dal.ts` | PostgreSQL | CTE pagination + LEFT JOIN, app-side grouping | Parameterized SQL WHERE (`$1`, `$2`, ...) | DDL migration on startup |
| `mongo-dal.ts` | MongoDB | Cursor pagination + batch `$in` queries per nav property | Native query operators + `$expr` | Collection/index creation on startup |
| `sqlite-dal.ts` | SQLite | CTE pagination + LEFT JOIN, app-side grouping | Parameterized SQL WHERE (`?` positional) | DDL execution on startup |

At startup, `index.ts` branches on `config.dbBackend`:
- **`postgres`** (default): creates a pg pool, runs schema migrations, and instantiates the PostgreSQL DAL
- **`mongodb`**: dynamically imports the `mongodb` package, connects `MongoClient`, initializes collections/indexes, and instantiates the MongoDB DAL
- **`sqlite`**: dynamically imports `better-sqlite3`, opens the database file with WAL journal mode, creates tables, and instantiates the SQLite DAL

Dynamic imports ensure backend-specific packages (`mongodb`, `better-sqlite3`) are not loaded when running a different backend.

### MongoDB-Specific Behavior

- **No schema migration** — MongoDB is schemaless. Collections and indexes are created on startup via `mongo-init.ts`
- **Native arrays** — Collection fields (e.g., `PropertyType[]`) are stored as native arrays, not JSONB strings
- **`_id` suppression** — MongoDB's auto-generated `_id` is projected out on all read paths
- **Index strategy** — Unique index on primary key per resource; compound index on `(ResourceName, ResourceRecordKey)` for child collections used by `$expand`

### SQLite-Specific Behavior

- **WAL journal mode** — Enables concurrent reads while writing; set on connection open via `PRAGMA journal_mode = WAL`
- **REGEXP function** — Registered as a custom function for OData `matchesPattern` support; uses JavaScript `RegExp`
- **Dates as TEXT** — All `Edm.Date` and `Edm.DateTimeOffset` values are stored as ISO 8601 strings; `strftime()` used for date extraction functions
- **Booleans as INTEGER** — Stored as `0`/`1`; coerced to `true`/`false` on read
- **Collections as TEXT** — Multi-value fields stored as JSON strings; `json_each()` table-valued function used for lambda `any()`/`all()` queries
- **No schema migration** — Tables are created with `CREATE TABLE IF NOT EXISTS` on startup; schema changes require dropping the database file

### $filter Translation (SQLite)

The `$filter` query option is parsed into an AST by `@reso/odata-filter-parser` and translated to parameterized SQLite SQL. All values use `?` positional placeholders.

| OData | SQLite |
|-------|--------|
| `eq`, `ne`, `gt`, `ge`, `lt`, `le` | `=`, `!=`, `>`, `>=`, `<`, `<=` |
| `and`, `or`, `not` | `AND`, `OR`, `NOT` |
| `contains(f, 'v')` | `f LIKE '%v%'` |
| `startswith(f, 'v')` | `f LIKE 'v%'` |
| `endswith(f, 'v')` | `f LIKE '%v'` |
| `year(f)`, `month(f)`, etc. | `CAST(strftime('%Y', f) AS INTEGER)` |
| `round(f)`, `floor(f)`, `ceiling(f)` | `ROUND(f)`, `floor(f)`, `ceil(f)` |
| `now()` | `strftime('%Y-%m-%dT%H:%M:%fZ', 'now')` |
| `any(v:v eq 'x')` | `EXISTS (SELECT 1 FROM json_each(col) WHERE value = ?)` |

### $filter Translation (MongoDB)

The `$filter` query option is parsed into an AST by `@reso/odata-filter-parser` and translated to MongoDB query documents. Two translation modes are used:

- **Query mode** — native MongoDB operators (`{ field: { $gt: value } }`) for simple comparisons that benefit from indexes
- **Expression mode** — `$expr` aggregation expressions for functions and arithmetic

| OData | MongoDB |
|-------|---------|
| `eq`, `ne`, `gt`, `ge`, `lt`, `le` | `{ field: value }`, `$ne`, `$gt`, `$gte`, `$lt`, `$lte` |
| `and`, `or`, `not` | `$and`, `$or`, `$nor` |
| `contains(f, 'v')` | `{ f: { $regex: 'v', $options: 'i' } }` |
| `startswith(f, 'v')` | `{ f: { $regex: '^v', $options: 'i' } }` |
| `endswith(f, 'v')` | `{ f: { $regex: 'v$', $options: 'i' } }` |
| `year(f)`, `month(f)`, etc. | `{ $expr: { $eq: [{ $year: '$f' }, value] } }` |
| `round(f)`, `floor(f)`, `ceiling(f)` | `{ $expr: { $round: ['$f', 0] } }` |

### Navigation Properties ($expand)

Navigation bindings are auto-detected at startup using three FK strategies:

| Strategy | Description | Example |
|----------|-------------|---------|
| `resource-record-key` | Polymorphic FK via `ResourceName` + `ResourceRecordKey` columns | Media on any parent resource |
| `direct` | Child has the parent's key field directly | OpenHouse.ListingKey, PropertyRooms.ListingKey |
| `parent-fk` | Parent holds a FK to the target (to-one navigation) | Property.ListAgentKey → Member.MemberKey |

The router discovers these bindings from `isExpansion` metadata attributes and logs them at startup. Expansion fields (e.g., HistoryTransactional, SocialMedia) are excluded from responses unless explicitly requested via `$expand`.

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
