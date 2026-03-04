# Add MongoDB Document Store Backend

## Context

The reference server has a clean `DataAccessLayer` interface (`data-access.ts`) with a PostgreSQL adapter. A MongoDB example sketch (`mongo-dal.example.ts`, 353 lines) already exists with working CRUD logic and batch expand, but `filterToMongoQuery()` is stubbed. The goal is to make MongoDB a fully functional alternative backend, selectable via environment variable.

---

## 1. Add `mongodb` dependency

**Modify:** `server/package.json` — add `"mongodb": "^6.12.0"` to dependencies. Run `npm install`.

---

## 2. Create `filter-to-mongo.ts` — OData $filter AST to MongoDB query translator

**Create:** `server/src/db/filter-to-mongo.ts` (~250 lines)

Mirrors `filter-to-sql.ts` structure. Uses `parseFilter()` from `@reso/odata-filter-parser` and walks the AST recursively.

**Two translation modes:**
- **Query mode** (`toQuery`) — produces `{ field: { $op: value } }` for top-level filters, `$and`/`$or` arrays. Used for simple comparisons that benefit from MongoDB indexes.
- **Expression mode** (`toExpr`) — produces aggregation expressions like `{ $gt: ['$field', value] }` for use inside `$expr`. Used when comparisons involve functions or arithmetic.

**Operator mapping:**

| AST Node | OData | MongoDB Query |
|----------|-------|---------------|
| comparison eq | `City eq 'Austin'` | `{ City: 'Austin' }` |
| comparison ne | `City ne 'Dallas'` | `{ City: { $ne: 'Dallas' } }` |
| comparison gt/ge/lt/le | `ListPrice gt 200000` | `{ ListPrice: { $gt: 200000 } }` |
| comparison eq null | `City eq null` | `{ City: null }` |
| comparison in | `City in ('A','B')` | `{ City: { $in: ['A', 'B'] } }` |
| logical and/or | `A and B` | `{ $and: [A, B] }` |
| not | `not (expr)` | `{ $nor: [expr] }` |
| contains | `contains(City, 'Aus')` | `{ City: { $regex: 'Aus', $options: 'i' } }` |
| startswith | `startswith(City, 'A')` | `{ City: { $regex: '^A', $options: 'i' } }` |
| endswith | `endswith(City, 'tin')` | `{ City: { $regex: 'tin$', $options: 'i' } }` |
| matchesPattern | `matchesPattern(City, '^A')` | `{ City: { $regex: '^A' } }` |
| year/month/day/etc | `year(Timestamp) eq 2024` | `{ $expr: { $eq: [{ $year: '$Timestamp' }, 2024] } }` |
| round/floor/ceiling | `round(ListPrice) eq 200000` | `{ $expr: { $eq: [{ $round: ['$ListPrice', 0] }, 200000] } }` |
| arithmetic | `ListPrice add 1000 gt 300000` | `{ $expr: { $gt: [{ $add: ['$ListPrice', 1000] }, 300000] } }` |
| length/tolower/toupper/trim/concat | function in comparison | `{ $expr: { ... } }` |
| lambda | any/all | Throw unsupported (same as SQL) |

**Key helpers:**
- `escapeRegex(str)` — escape regex special characters for `$regex` values
- Field name validation against `ctx.fields` (same as SQL translator)
- Prefer native query operators over `$expr` for index efficiency

**Reuse:** `parseFilter` from `@reso/odata-filter-parser` (`tools/odata-filter-parser/src/index.ts`), `ResoField` type from `server/src/metadata/types.ts`

---

## 3. Create production `mongo-dal.ts` from example sketch

**Create:** `server/src/db/mongo-dal.ts` (~200 lines)

Based on `mongo-dal.example.ts` with these changes:

- Replace type stubs with real `mongodb` imports (`Db`, `Collection`, `Document`)
- Import and use `filterToMongo` from `./filter-to-mongo.js`
- Add `{ projection: { _id: 0 } }` on all read paths (find, findOne, batch expand)
- Strip `_id` from insert return values
- Validate `$orderby` field names against `ctx.fields`
- No `deserializeRow` needed — MongoDB stores arrays natively (no JSONB serialization)

**Factory:** `export const createMongoDal = (db: Db): DataAccessLayer`

---

## 4. Create `mongo-init.ts` — collection and index setup

**Create:** `server/src/db/mongo-init.ts` (~40 lines)

Analogous to `migrate.ts` + `schema-generator.ts` for PostgreSQL:

- Create unique index on key field per resource
- Create compound index on `(ResourceName, ResourceRecordKey)` for child collections (Media, OpenHouse, Showing) — used by the RESO FK convention in `$expand` resolution

---

## 5. Update `config.ts` — add backend selector

**Modify:** `server/src/config.ts`

Add to `ServerConfig`:
- `dbBackend: 'postgres' | 'mongodb'` — from `DB_BACKEND` env var, defaults to `'postgres'`
- `mongodbUrl: string` — from `MONGODB_URL` env var, defaults to `'mongodb://localhost:27017/reso_reference'`

---

## 6. Update `index.ts` — conditional DAL instantiation

**Modify:** `server/src/index.ts`

Branch on `config.dbBackend`:

- **`'postgres'`** (default, unchanged): create pg pool, run migrations, create PostgreSQL DAL
- **`'mongodb'`**: dynamic `import('mongodb')`, connect `MongoClient`, call `initializeMongoCollections`, create MongoDB DAL

Use dynamic imports so the `mongodb` package isn't loaded when running PostgreSQL mode.

---

## 7. Update `docker-compose.yml` — add MongoDB profile

**Modify:** `docker-compose.yml`

Add services under the `mongodb` profile:

- `mongodb` — `mongo:7` with healthcheck (`mongosh --eval "db.adminCommand('ping')"`)
- `server-mongo` — same server image but with `DB_BACKEND=mongodb`, `MONGODB_URL=mongodb://mongodb:27017/reso_reference`, depends on `mongodb`
- `seed-mongo` — same seed logic pointing at `server-mongo`
- Add `mongodata` volume

**Usage:**
- PostgreSQL (unchanged): `docker compose up -d`
- MongoDB: `docker compose --profile mongodb up -d`
- MongoDB + seed: `docker compose --profile mongodb --profile seed-mongo up`

---

## 8. Delete example sketch

**Delete:** `server/src/db/mongo-dal.example.ts` — superseded by production `mongo-dal.ts`

---

## 9. Create `filter-to-mongo.test.ts`

**Create:** `server/tests/filter-to-mongo.test.ts` (~220 lines, ~25-30 tests)

Mirror `filter-to-sql.test.ts` structure, asserting MongoDB query documents:

- Comparison operators (eq, ne, gt, ge, lt, le, in)
- Null comparisons (eq null, ne null)
- Logical operators (and, or, not)
- String functions (contains, startswith, endswith, matchesPattern)
- Date functions (year, month, day)
- Math functions (round, floor, ceiling)
- Arithmetic operators (add, sub, mul, div)
- Complex nested expressions
- Error handling (unknown fields)

---

## 10. Documentation

### 10A. Update server README — MongoDB backend docs

**Modify:** `tools/reso-reference-server/server/README.md`

Add section documenting:
- `DB_BACKEND` and `MONGODB_URL` environment variables
- How to start with MongoDB locally (prerequisites, connection)
- How the DAL abstraction works (interface → adapter selection)
- MongoDB-specific behavior (native arrays, no schema migration, index creation)

### 10B. Update reference server README — Docker instructions for both backends

**Modify:** `tools/reso-reference-server/README.md`

Update Quick Start to show both backends:
- PostgreSQL (default, unchanged commands)
- MongoDB (`docker compose --profile mongodb up -d` + seed)
- Switching between backends

### 10C. Create missing module READMEs

Three tool modules are missing READMEs:

**Create:** `tools/validation/README.md`
- Isomorphic validation library for RESO metadata-driven field validation
- Public API: `validateRecord`, `getBusinessRules`, `CrossFieldRule`
- Usage examples for server and UI integration
- Business rules (price, bedroom, bathroom ranges, cross-field constraints)

**Create:** `tools/data-generator/README.md`
- Generates realistic RESO Data Dictionary test data
- CLI usage (interactive and non-interactive modes)
- Three output modes: HTTP, JSON, curl
- Resource-specific generators (Property, Member, Office, Media, OpenHouse, Showing)

**Create:** `tools/certification/test-runner/README.md`
- Generic OData certification test infrastructure
- Validators, reporter, HTTP client, auth helpers, metadata helpers
- How certification modules (like add-edit) depend on this shared framework

---

## Files Summary

| File | Action | ~Lines |
|------|--------|--------|
| `server/package.json` | Modify | +1 |
| `server/src/db/filter-to-mongo.ts` | Create | 250 |
| `server/src/db/mongo-dal.ts` | Create | 200 |
| `server/src/db/mongo-init.ts` | Create | 40 |
| `server/src/config.ts` | Modify | +5 |
| `server/src/index.ts` | Modify | +25 |
| `docker-compose.yml` | Modify | +50 |
| `server/tests/filter-to-mongo.test.ts` | Create | 220 |
| `server/src/db/mongo-dal.example.ts` | Delete | -353 |
| `server/README.md` | Modify | +40 |
| `README.md` (reference-server) | Modify | +30 |
| `tools/validation/README.md` | Create | 80 |
| `tools/data-generator/README.md` | Create | 100 |
| `tools/certification/test-runner/README.md` | Create | 60 |

---

## Verification

```bash
# 1. Unit tests (all existing + new filter-to-mongo tests)
cd tools/reso-reference-server/server && npm test

# 2. PostgreSQL regression (default, unchanged)
cd tools/reso-reference-server
docker compose down -v
docker compose build && docker compose up -d
docker compose --profile seed rm -f seed && docker compose --profile seed up seed
# Verify: GET /Property?$expand=Media&$top=3&$count=true — count=50, 3 entities, nextLink

# 3. MongoDB integration
docker compose down -v
docker compose --profile mongodb up -d
docker compose --profile mongodb --profile seed-mongo up
# Verify same queries work:
#   GET /Property?$filter=ListPrice gt 200000
#   GET /Property?$expand=Media&$top=3&$count=true
#   GET /Property?$filter=contains(City,'Aus')
#   POST /Property, PATCH /Property('key'), DELETE /Property('key')
# Verify _id never appears in responses
# Verify collection fields returned as arrays (not JSON strings)
```
