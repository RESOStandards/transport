# src/mock/ — Mock OData Server

A lightweight Express-based mock OData server for offline Add/Edit compliance testing. Provides deterministic CRUD behavior without requiring a real OData server.

## Modules

### server.ts
Sets up and starts the Express application with routes for `/$metadata` and CRUD operations on a configurable resource name. Uses regex route patterns to handle OData key syntax (e.g., `Property('12345')`) and the `$metadata` endpoint (Express treats `$` as special in string routes).

- `startMockServer(options)` — Starts the server and returns the HTTP server instance and resolved URL
- `stopMockServer(server)` — Gracefully shuts down the server

### handlers.ts
Route handler functions for each OData operation, backed by an in-memory `Map` store:

- `handleMetadata(xml)` — Serves the XML metadata document at `/$metadata`
- `handleCreate(resource)` — POST handler: generates a key, stores the record, returns 201 (representation) or 204 (minimal) based on the `Prefer` header
- `handleUpdate(resource)` — PATCH handler: merges request body with existing record, returns 200 or 204
- `handleDelete()` — DELETE handler: removes the record and returns 204, or 404 if not found
- `handleGet(resource)` — GET handler: returns the record with OData annotations, or 404

**Error simulation**: Any request body containing a negative numeric field value triggers a 400 response with an OData-compliant error format. This matches the convention used in the sample `*-fails.json` payloads.

**Test helpers**:
- `resetStore()` — Clears all records (use between test runs)
- `seedStore(key, record)` — Pre-populates a record for update/delete testing
