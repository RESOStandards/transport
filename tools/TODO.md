# RESO Transport Tools — TODO

Track outstanding requirements and future work items. These will be migrated
to GitHub Issues once the repository is ready for public collaboration.

---

## High Priority

### Multi-Level $expand Support
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

### Data Access Layer — Additional Backends
**Package:** `reso-reference-server`

The server defines a `DataAccessLayer` interface. Currently only the
PostgreSQL implementation exists. Future backends to consider:
- MongoDB (document store — example interface sketch exists)
- SQLite (lightweight local testing)
- In-memory (unit testing without database)

### Refactor `web-api-add-edit-test` to Use `@reso/odata-client`
**Package:** `web-api-add-edit-test`

Replace raw `fetch` calls (`odataRequest()`) with CRUD helpers from
`@reso/odata-client`. Replace `fetchMetadata()`/`parseMetadataXml()` with
`fetchAndParseMetadata()`. All 49 existing tests must continue to pass.

---

## Medium Priority

### Batch Operations (`$batch`)
**Package:** `odata-client`, `reso-reference-server`

OData 4.01 supports batch requests via `POST /$batch` in both
multipart/mixed and JSON formats. Implement:
- Client: `batchRequest(client, requests[])` helper
- Server: `$batch` endpoint with changeset (atomic group) support
- JSON batch format (preferred for 4.01)

### Asynchronous Requests
**Package:** `odata-client`, `reso-reference-server`

`Prefer: respond-async` for long-running operations:
- Client: detect `202 Accepted`, poll status monitor URL
- Server: queue handler, status monitor endpoint, `Retry-After` header

### Delta Responses / Change Tracking
**Package:** `odata-client`, `reso-reference-server`

`Prefer: odata.track-changes` with `@odata.deltaLink`:
- Client: `followDelta(client, deltaLink)` helper
- Server: track changes via database triggers or logical timestamps,
  return delta payloads with added/changed/deleted entities

### Type Casting in URLs
**Package:** `odata-client`, `odata-filter-parser`

Support `EntitySet/Namespace.DerivedType` path segments for filtering
to derived entity types. Requires parser updates for type-qualified
segments.

---

## Lower Priority

### Geo Functions and Types
**Package:** `odata-filter-parser`, `odata-client`

`geo.distance()`, `geo.intersects()`, `geo.length()` filter functions
and `Edm.Geography*`/`Edm.Geometry*` types. Not commonly used in RESO
but part of the OData 4.01 spec.

### TypeDefinition Parsing
**Package:** `odata-client`

CSDL `TypeDefinition` elements (named aliases for primitive types with
constrained facets). Parse and validate in CSDL parser.

### Annotation Parsing
**Package:** `odata-client`

Full `Annotation` element parsing from CSDL XML, including vocabulary
references (`edmx:Reference`/`edmx:Include`) and standard vocabularies
(Core, Capabilities, Validation, Measures).

### $apply (Aggregation)
**Package:** `odata-filter-parser`, `reso-reference-server`

OData Data Aggregation Extension: `groupby`, `aggregate`, `filter`,
`compute` transformations. Complex feature — implement when needed.

---

## Infrastructure

### Migrate TODOs to GitHub Issues
Move items from this file into GitHub Issues with proper labels,
milestones, and assignees once the repository is public.

### CI/CD Pipeline
Set up GitHub Actions for:
- Build all packages in dependency order
- Run all test suites
- Cross-tool validation (reference server + test tool)
- Publish packages to npm (when ready)
