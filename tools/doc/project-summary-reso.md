# RESO Transport Tools — Technical Summary

## Overview

This project delivers a metadata-driven reference implementation of RESO Web API Core 2.0.0 and Data Dictionary 2.0, along with supporting libraries, a test data generator, a compliance testing pipeline, and a web-based administration UI. The reference server has passed RESO Certification for Web API Core 2.0.0.

---

## Components

### Reference Server

A fully metadata-driven OData 4.01 server supporting all 13 DD 2.0 resources: Property, Member, Office, Media, OpenHouse, Contacts, Teams, TeamMembers, Showing, PropertyRooms, PropertyUnitTypes, InternetTracking, SavedSearch, Prospecting, OUID, and Lookup.

Key capabilities:

- **Full CRUD** with OData system query options: `$filter`, `$select`, `$orderby`, `$top`, `$skip`, `$count`, `$expand`
- **Three database backends**: PostgreSQL (default), MongoDB, and SQLite — abstracted behind a common DAL interface
- **Navigation property expansion** (`$expand`) with three FK resolution strategies: `resource-record-key` (polymorphic), `direct` (child→parent), and `parent-fk` (parent→child)
- **1,727 DD 2.0 fields** across all resources with correct Edm types and annotations
- **3,634 lookup values** from the DD 2.0 reference metadata
- **Server-driven pagination** with `@odata.nextLink`
- **EDMX metadata** and OData service document generation from DD metadata
- **EntityEvent Resource (RCP-027)** — opt-in change tracking via monotonically increasing sequence numbers. DAL decorator pattern captures all writes (API, seeding, admin). Database-native auto-increment (PG BIGSERIAL, SQLite AUTOINCREMENT, MongoDB atomic counter). Scheduled compaction removes superseded events. Read-only OData resource with standard query options.

### Enumeration Modes

The server supports two mutually exclusive enumeration representations, selectable via `ENUM_MODE` environment variable:

- **`string` (default)** — Enum fields map to `Edm.String` with RESO `LookupName` annotations. A Lookup Resource exposes all valid values. Human-readable display names used as values (e.g., "Active Under Contract").
- **`enum-type`** — Enum fields reference `Edm.EnumType` definitions in a second EDMX Schema namespace (`org.reso.metadata.enums`). PascalCase member names (e.g., `ActiveUnderContract`) with sequential integer values. No Lookup Resource exposed.

Both modes pass Web API Core 2.0.0 certification independently.

### Web Administration UI

A React SPA (Vite + Tailwind CSS) providing:

- Paginated data browsing for all 13 resources
- Record creation and editing with real-time validation
- Cross-field business rule enforcement (shared `@reso/validation` package)
- Navigation property traversal between related records

### Test Data Generator

Generates referentially correct seed data across all DD 2.0 resources:

- Metadata-driven FK resolution with automatic dependency graph construction and topological sorting
- Handles the Office ↔ Member circular dependency via deferred back-fill
- Three output modes: HTTP POST to a running server, JSON files, and curl scripts
- CLI with `--deps` flag for multi-resource orchestration

### Compliance Testing Pipeline

Docker-based integration with the RESO Commander and a custom Add/Edit test runner:

- **RESOScript generation** — Automatically extracts test parameters (single/multi lookup fields, enum namespaces) from the server's EDMX metadata
- **Web API Core 2.0.0** — 42 passed, 0 failed, 3 skipped (45 specification tests)
- **Data Dictionary 2.0** — 1,034 passed, 570 skipped, 0 failed, 0 schema validation errors
- **Add/Edit (RCP-010)** — 8 passed, 0 failed. Structured JSON compliance report generation with per-scenario details
- **EntityEvent (RCP-027)** — manual testing verified on SQLite and MongoDB backends. Compliance testing tool planned.
- **Both enum modes** tested independently via `ENUM_MODE` environment variable
- **PATCH validation** — Partial updates skip required-field checks (`skipRequired` parameter)
- Configurable via Docker Compose profiles (`compliance-core`, `compliance-dd`, `compliance-addedit`)

### Reusable Libraries

| Package | Description |
|---------|-------------|
| `@reso/validation` | Isomorphic field + cross-field validation rules (browser & server) |
| `odata-filter-parser` | Standalone OData `$filter` expression parser, zero dependencies |
| `odata-client` | OData 4.01 client SDK with type-safe query building |

---

## Architecture

- **Functional TypeScript** — Arrow functions only, no classes, `Readonly<T>`, pure functions
- **ESM modules** with Node16 resolution (`.js` import extensions)
- **Shared metadata** — DD 2.0 JSON metadata drives server schema, EDMX generation, data generation, and validation
- **DAL abstraction** — `CollectionResult { value, count? }` interface with PostgreSQL (CTE-based pagination + LEFT JOIN expand), MongoDB (cursor pagination + batch `$in` expand), and SQLite adapters
- **735 automated tests** across 7 packages
- **Biome** for linting/formatting, **Lefthook** for pre-commit hooks

## Certification Status

The reference server has passed **RESO Certification** for Web API Core 2.0.0, Data Dictionary 2.0, and Add/Edit (RCP-010).

---

## Planned Future Work

| Feature | RCP | Description |
|---------|-----|-------------|
| Validation Expressions | RCP-019 | Server-side validation expression evaluation and client exposure via metadata annotations |
| ~~EntityEvent~~ | ~~RCP-027~~ | ~~Change tracking — delivered in v0.0.27. Compliance testing tool planned.~~ |
| Webhooks | RCP-028 | Push-based notification delivery for real-time data change subscriptions |

---

## Quick Start

```bash
# Start server + UI + PostgreSQL
docker compose up -d --build

# Seed test data (892 records)
docker compose --profile seed up seed

# Access
# Server: http://localhost:8080
# UI:     http://localhost:5173
# EDMX:   http://localhost:8080/$metadata

# Run compliance tests
docker compose --profile compliance-core up compliance-core
docker compose --profile compliance-dd up compliance-dd

# Enum-type mode
ENUM_MODE=enum-type docker compose up -d --build

# Enable EntityEvent change tracking
ENTITY_EVENT=true docker compose up -d --build
```
