# OData Filter Parser, Client Library, Server Query Parsing & Test Tool Refactoring

## Context

The `reso/transport` repo already has:
- **`tools/web-api-add-edit-test/`** — RCP-010 compliance test tool (8 Gherkin scenarios, 49 tests passing) using raw `fetch` via `odataRequest()`
- **`tools/reso-reference-server/`** — Metadata-driven OData 4.01 server (code written, not yet npm installed/built/tested)

The user wants:
1. A **shared OData `$filter` parser library** used by both client and server
2. An **OData client SDK** modeled after Apache Olingo's Java Client — query builder, CRUD helpers, CSDL validation, examples
3. **Server-side query parsing** so GET collection requests honor `$filter`, `$select`, `$orderby`, `$top`, `$skip`
4. **Refactor `web-api-add-edit-test`** to use the new OData client instead of raw fetch

Inspired by Apache Olingo's architecture but implemented idiomatically in TypeScript (functional, no classes).

---

## Part 0: OData Filter Parser — `tools/odata-filter-parser/`

Standalone library for parsing OData `$filter` expressions into an AST. Used by both the client (validation) and the server (SQL translation).

### Directory Structure

```
tools/odata-filter-parser/
├── package.json              # @reso/odata-filter-parser, ESM, zero runtime deps
├── tsconfig.json
├── vitest.config.ts
├── CLAUDE.md
├── README.md
├── src/
│   ├── index.ts              # Public API: parseFilter, types
│   ├── types.ts              # AST node types (FilterExpression union)
│   ├── lexer.ts              # Tokenizer: string → Token[]
│   └── parser.ts             # Recursive descent: Token[] → FilterExpression
└── tests/
    └── filter-parser.test.ts # Comprehensive parser tests
```

### Step 1: AST Types (`src/types.ts`)
- `ComparisonExpr` — `{ type: "comparison", left, operator: "eq"|"ne"|"gt"|"ge"|"lt"|"le", right }`
- `LogicalExpr` — `{ type: "logical", operator: "and"|"or", left, right }`
- `NotExpr` — `{ type: "not", operand }`
- `FunctionCallExpr` — `{ type: "function", name: "contains"|"startswith"|"endswith", args }`
- `ArithmeticExpr` — `{ type: "arithmetic", operator: "add"|"sub"|"mul"|"div"|"mod", left, right }`
- `LiteralExpr` — `{ type: "literal", value: string|number|boolean|null, dataType }`
- `PropertyExpr` — `{ type: "property", name: string }`
- `FilterExpression` — union of all above

### Step 2: Lexer (`src/lexer.ts`)
- `tokenize(input: string) → Token[]`
- Token types: `property`, `literal_string`, `literal_number`, `literal_boolean`, `literal_null`, `operator`, `function`, `lparen`, `rparen`, `comma`
- Handles single-quoted strings with `''` escape, numeric literals (int/decimal), `true`/`false`/`null`

### Step 3: Parser (`src/parser.ts`)
- `parseFilter(filterString: string) → FilterExpression`
- Recursive descent with precedence: `or` < `and` < `not` < comparison < arithmetic
- Function calls: `contains(PropertyName, 'value')`, `startswith(...)`, `endswith(...)`
- Parenthesized grouping
- Throws descriptive errors with position info on invalid input

### Step 4: Tests (`tests/filter-parser.test.ts`)
- Simple comparisons: `ListPrice gt 200000`, `City eq 'Austin'`
- Logical: `ListPrice gt 200000 and City eq 'Austin'`
- Not: `not contains(City, 'Austin')`
- Functions: `contains(City, 'Aus')`, `startswith(City, 'A')`
- Nested: `(ListPrice gt 100000 or ListPrice lt 50000) and City eq 'Austin'`
- Arithmetic: `ListPrice add 1000 gt 300000`
- Null: `City eq null`
- Error cases: malformed expressions, unknown operators

---

## Part 1: OData Client Library — `tools/odata-client/`

### Directory Structure

```
tools/odata-client/
├── package.json              # @reso/odata-client, deps: fast-xml-parser, @reso/odata-filter-parser
├── tsconfig.json
├── vitest.config.ts
├── CLAUDE.md
├── README.md
├── src/
│   ├── index.ts              # Public API barrel export
│   ├── types.ts              # All shared types
│   ├── uri/
│   │   ├── builder.ts        # OData URI builder (Olingo-style fluent API via functions)
│   │   └── parser.ts         # Parse OData query string into structured options
│   ├── query/
│   │   └── validator.ts      # Validate query options against CSDL metadata (uses @reso/odata-filter-parser)
│   ├── csdl/
│   │   ├── parser.ts         # EDMX/CSDL XML → structured metadata
│   │   ├── types.ts          # CsdlSchema, CsdlEntityType, CsdlProperty, CsdlEnumType
│   │   └── validator.ts      # Validate CSDL document structure and required elements
│   ├── http/
│   │   ├── client.ts         # OData HTTP client (fetch + OData headers + auth)
│   │   └── auth.ts           # OAuth2 Client Credentials + Bearer token
│   ├── crud/
│   │   ├── create.ts         # createEntity(client, resource, body, opts) → ODataResponse
│   │   ├── read.ts           # readEntity(client, resource, key) → entity
│   │   ├── update.ts         # updateEntity(client, resource, key, body, opts) → ODataResponse
│   │   ├── delete.ts         # deleteEntity(client, resource, key) → ODataResponse
│   │   └── query.ts          # queryEntities(client, resource, queryOpts) → collection
│   ├── response/
│   │   ├── parser.ts         # Parse OData JSON response (annotations, errors)
│   │   └── error.ts          # ODataError type + parseODataError()
│   └── metadata/
│       └── fetcher.ts        # fetchMetadata(url, token) → raw XML, then parse via csdl/parser
├── examples/
│   ├── fetch-property.ts     # Fetch a Property record by key
│   ├── query-with-filter.ts  # Query with $filter, $select, $orderby
│   ├── create-and-update.ts  # Create then update a record
│   ├── validate-metadata.ts  # Fetch and validate server CSDL metadata
│   └── oauth-flow.ts         # OAuth2 Client Credentials token exchange
└── tests/
    ├── uri-builder.test.ts
    ├── query-validator.test.ts
    ├── csdl-parser.test.ts
    ├── csdl-validator.test.ts
    ├── crud.test.ts
    └── response-parser.test.ts
```

### Step 5: Project Scaffolding
- `package.json`: name `@reso/odata-client`, ESM, TypeScript, dependencies: `fast-xml-parser`, `@reso/odata-filter-parser`
- `tsconfig.json`: ES2022, Node16, strict
- `CLAUDE.md`: same conventions

### Step 6: Types (`src/types.ts`)
- `ODataQueryOptions` — `{ $filter?, $select?, $orderby?, $top?, $skip?, $count? }`
- `ODataResponse` — `{ status, headers, body, rawBody }`
- `ODataEntity` — `Record<string, unknown>` with `@odata.*` annotations
- `ODataCollection` — `{ value: ODataEntity[], "@odata.count"?, "@odata.nextLink"? }`
- `ODataError` — `{ error: { code, message, details?: { target, message }[] } }`
- `AuthConfig` — discriminated union: `token` | `client_credentials` (reuse pattern from test tool)
- `ClientConfig` — `{ baseUrl, auth: AuthConfig, defaultHeaders? }`
- `PreferReturn` — `"representation" | "minimal"`

### Step 7: URI Builder (`src/uri/builder.ts`)
Olingo-inspired functional builder pattern (not class-based):

```typescript
// Usage: buildUri("http://localhost:8080", "Property")
//   .key("ABC123")
//   .select("ListPrice", "City")
//   .filter("ListPrice gt 200000")
//   .orderby("ListPrice desc")
//   .top(10)
//   .build()  → full URL string
```

- `buildUri(baseUrl, resource)` returns a builder object with chainable methods
- `.key(value)` — adds OData key syntax `('value')`
- `.select(...fields)` — `$select=field1,field2`
- `.filter(expr)` — `$filter=expr` (raw string; parsed/validated separately)
- `.orderby(expr)` — `$orderby=field asc/desc`
- `.top(n)` / `.skip(n)` / `.count(bool)` — pagination
- `.build()` — returns the final URL string

### Step 8: Query Validator (`src/query/validator.ts`)
- Imports `parseFilter` from `@reso/odata-filter-parser` to parse and walk the `$filter` AST
- `validateQueryOptions(options: ODataQueryOptions, entityType: CsdlEntityType) → ValidationResult`
- Checks `$select` fields exist in entity type
- Checks `$orderby` fields exist
- Checks `$filter` property references exist (walks AST from shared parser)
- Checks `$top`/`$skip` are non-negative integers

### Step 9: CSDL Parser (`src/csdl/parser.ts`, `types.ts`)
Move and enhance the EDMX parsing logic from `web-api-add-edit-test/src/lib/metadata.ts`:

**Types** (`csdl/types.ts`):
- `CsdlSchema` — `{ namespace, entityTypes, enumTypes, entityContainer }`
- `CsdlEntityType` — `{ name, key: string[], properties: CsdlProperty[] }`
- `CsdlProperty` — `{ name, type, nullable?, maxLength?, precision?, scale? }`
- `CsdlEnumType` — `{ name, members: { name, value }[] }`
- `CsdlEntityContainer` — `{ name, entitySets: { name, entityType }[] }`

**Functions:**
- `parseCsdlXml(xml: string) → CsdlSchema` — uses fast-xml-parser, same options as current `parseMetadataXml`
- `getEntityType(schema, name) → CsdlEntityType | undefined`
- `getEnumType(schema, name) → CsdlEnumType | undefined`

### Step 10: CSDL Validator (`src/csdl/validator.ts`)
- `validateCsdl(schema: CsdlSchema) → CsdlValidationResult`
- Checks: namespace present, entity types have keys, properties have valid Edm types, enum types referenced exist, entity sets reference valid entity types
- Returns `{ valid: boolean, errors: CsdlValidationError[] }`

### Step 11: HTTP Client (`src/http/client.ts`, `auth.ts`)
**Auth** (`auth.ts`):
- `resolveToken(auth: AuthConfig) → Promise<string>` — bearer passthrough or OAuth2 Client Credentials exchange
- Reuse logic from `web-api-add-edit-test/src/lib/auth.ts`

**Client** (`client.ts`):
- `createClient(config: ClientConfig) → ODataClient`
- `ODataClient` is an object (not class) with:
  - `request(method, url, opts?) → Promise<ODataResponse>` — sets OData-Version, Content-Type, Accept, Authorization headers
  - `baseUrl`, `auth` for reference
- Uses native `fetch`

### Step 12: CRUD Helpers (`src/crud/`)
High-level functions that compose URI builder + HTTP client:

- `createEntity(client, resource, body, { prefer? }) → Promise<ODataResponse>`
- `readEntity(client, resource, key) → Promise<ODataResponse>`
- `updateEntity(client, resource, key, body, { prefer? }) → Promise<ODataResponse>`
- `deleteEntity(client, resource, key) → Promise<ODataResponse>`
- `queryEntities(client, resource, queryOpts?) → Promise<ODataResponse>` — builds URL with $filter/$select/$orderby/$top/$skip, issues GET

### Step 13: Response Parser + Error (`src/response/`)
- `parseODataError(response: ODataResponse) → ODataError | null` — extract structured error from response body
- `isODataError(body: unknown) → body is { error: ... }` — type guard
- `extractAnnotations(entity: ODataEntity) → ODataAnnotations` — pull @odata.context/id/editLink/etag

### Step 14: Metadata Fetcher (`src/metadata/fetcher.ts`)
- `fetchAndParseMetadata(baseUrl, token) → Promise<CsdlSchema>` — fetches `$metadata` endpoint, parses XML, returns structured schema
- `fetchRawMetadata(baseUrl, token) → Promise<string>` — returns raw XML

### Step 15: Examples (`examples/`)
Runnable TypeScript examples (ts-node or tsx):
- `fetch-property.ts` — create client, read a Property by key
- `query-with-filter.ts` — query with `$filter=ListPrice gt 200000 and City eq 'Austin'`, `$select=ListPrice,City`, `$orderby=ListPrice desc`, `$top=10`
- `create-and-update.ts` — POST a new Property, then PATCH it
- `validate-metadata.ts` — fetch CSDL from server, validate it, print results
- `oauth-flow.ts` — OAuth2 Client Credentials flow, then query

### Step 16: Tests
- `uri-builder.test.ts` — URL construction for all query options
- `query-validator.test.ts` — field existence validation (uses `@reso/odata-filter-parser` to walk AST)
- `csdl-parser.test.ts` — parse EDMX XML (use sample-metadata.xml from test tool)
- `csdl-validator.test.ts` — valid/invalid CSDL documents
- `crud.test.ts` — mock server integration for create/read/update/delete
- `response-parser.test.ts` — error parsing, annotation extraction

---

## Part 2: Server Query Parsing — `tools/reso-reference-server/server/`

Add support for OData query options on GET collection endpoints. The server imports `@reso/odata-filter-parser` for `$filter` parsing (same parser the client uses).

### Step 17: Add `@reso/odata-filter-parser` dependency
- Add to `reso-reference-server/server/package.json`

### Step 18: Query Parser for Server (`src/odata/query-parser.ts`)
- `parseQueryOptions(queryString) → ODataQueryOptions` — parse Express `req.query` into structured options
- Uses `parseFilter` from `@reso/odata-filter-parser` (shared library)
- Validate field references against server metadata

### Step 19: SQL Query Builder for $filter (`src/db/filter-to-sql.ts`)
- `filterToSql(ast: FilterExpression, fields: ResoField[]) → { where: string, values: unknown[] }`
- Imports AST types from `@reso/odata-filter-parser`
- Comparison operators → SQL operators (`eq`→`=`, `ne`→`!=`, `gt`→`>`, etc.)
- `and`/`or` → SQL `AND`/`OR`
- `not` → SQL `NOT`
- `contains(field, 'val')` → `field ILIKE '%val%'`
- `startswith(field, 'val')` → `field ILIKE 'val%'`
- `endswith(field, 'val')` → `field ILIKE '%val'`
- All values parameterized (never string-interpolated)

### Step 20: Collection Query Builder (`src/db/queries.ts` update)
- Add `buildCollectionQuery(resource, keyField, fields, queryOpts)` → `{ text, values }`
- Supports `$select` → SQL SELECT column list
- Supports `$filter` → SQL WHERE via `filterToSql` (AST from shared `@reso/odata-filter-parser`)
- Supports `$orderby` → SQL ORDER BY (validate field names against metadata)
- Supports `$top` → SQL LIMIT
- Supports `$skip` → SQL OFFSET
- Supports `$count` → add `COUNT(*) OVER()` window function

### Step 21: Collection GET Handler (`src/odata/handlers.ts` update)
- Add `collectionHandler(ctx)` — GET `/{Resource}` without key
- Parse query options from URL
- Build and execute collection query
- Return `{ "@odata.context": "...", "value": [...] }` with optional `"@odata.count"`
- Register new route in `router.ts`

### Step 22: Install, Build & Test Reference Server
- Run `npm install` and `npm run build` for the reference server
- Run `npm test` to verify all 4 existing test suites pass
- Add tests for query parser and filter-to-SQL translation

---

## Part 3: Refactor `web-api-add-edit-test` to Use OData Client

### Step 23: Add `@reso/odata-client` Dependency
- Add file reference dependency to `web-api-add-edit-test/package.json`

### Step 24: Replace `client.ts` with OData Client
- Replace `odataRequest()` calls in `test-runner.ts` with CRUD helpers from `@reso/odata-client`
- Replace `buildResourceUrl()` with URI builder
- Replace `fetchMetadata()` / `parseMetadataXml()` with `fetchAndParseMetadata()`
- Map `CsdlEntityType` to the existing `EntityType` interface (or update validators to use CSDL types directly)

### Step 25: Update Validators
- Update `validatePayloadAgainstMetadata()` to use `CsdlEntityType` from client library
- Keep all 12 validators intact (they test OData compliance, not client behavior)
- Update type references as needed

### Step 26: Verify All 49 Tests Still Pass
- Run `npm test` in `web-api-add-edit-test/`
- Run cross-tool validation against the reference server

---

## Key Decisions

1. **Shared `odata-filter-parser` package** — `tools/odata-filter-parser/` is a standalone, zero-dependency library exporting `parseFilter()` and AST types; used by both client and server
2. **Separate `odata-client` package** — `tools/odata-client/` depends on `@reso/odata-filter-parser` for query validation
3. **Server imports shared parser** — `reso-reference-server/server` depends on `@reso/odata-filter-parser` and translates AST → SQL
4. **Functional builder pattern** — URI builder returns a chainable object (not a class), following project conventions
5. **Recursive descent parser** — hand-written, no parser generator dependency, zero runtime deps
6. **CSDL types separate from RESO types** — `CsdlEntityType` (OData standard) vs `ResoField` (RESO-specific); the server maps between them
7. **Preserve test tool validators** — refactoring replaces the HTTP/metadata layer, not the assertion logic
8. **Cite Apache Olingo** — README and source comments reference Olingo's architecture as inspiration

## Critical Files

| File | Change |
|------|--------|
| New: `odata-filter-parser/` (entire package) | ~4 source files, shared by client + server |
| New: `odata-client/` (entire package) | ~14 source files + 5 examples |
| `reso-reference-server/server/package.json` | Add `@reso/odata-filter-parser` dep |
| `reso-reference-server/server/src/odata/handlers.ts` | Add `collectionHandler` |
| `reso-reference-server/server/src/odata/router.ts` | Register GET collection route |
| `reso-reference-server/server/src/db/queries.ts` | Add `buildCollectionQuery` |
| New: `reso-reference-server/server/src/odata/query-parser.ts` | Server query option parsing |
| New: `reso-reference-server/server/src/db/filter-to-sql.ts` | $filter AST → SQL WHERE |
| `web-api-add-edit-test/package.json` | Add `@reso/odata-client` dep |
| `web-api-add-edit-test/src/lib/client.ts` | Replace with imports from `@reso/odata-client` |
| `web-api-add-edit-test/src/lib/metadata.ts` | Replace parser with `@reso/odata-client/csdl` |
| `web-api-add-edit-test/src/lib/test-runner.ts` | Use CRUD helpers instead of `odataRequest()` |
| `web-api-add-edit-test/src/lib/types.ts` | Add re-exports from `@reso/odata-client` types |

## Build Order

1. `tools/odata-filter-parser` — no deps, build first
2. `tools/odata-client` — depends on filter parser
3. `tools/reso-reference-server/server` — depends on filter parser
4. `tools/web-api-add-edit-test` — depends on odata-client

## Verification

1. `cd tools/odata-filter-parser && npm test` — all parser tests pass
2. `cd tools/odata-client && npm test` — all client library tests pass
3. `cd tools/reso-reference-server/server && npm test` — all server tests pass (including new query/filter tests)
4. `cd tools/web-api-add-edit-test && npm test` — all 49 tests still pass
5. Cross-tool validation:
```bash
# Start reference server with Docker
cd tools/reso-reference-server && docker-compose up -d

# Run compliance tests using refactored test tool
cd tools/web-api-add-edit-test
npx testWebApiAddEdit \
  --url http://localhost:8080 \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token test
```
All 8 scenarios must pass.
6. Run examples from `odata-client/examples/` against the reference server.
