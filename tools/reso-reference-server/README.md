# RESO Reference OData Server

A metadata-driven OData 4.01 reference server for the [RESO Data Dictionary](https://www.reso.org/data-dictionary/). Reads the RESO JSON metadata format and dynamically generates PostgreSQL tables, OData CRUD endpoints, EDMX metadata, and OpenAPI documentation.

## Quick Start (Docker)

The server supports three database backends: **PostgreSQL** (default), **MongoDB**, and **SQLite**. Each has its own Docker Compose profile.

### PostgreSQL (default)

```bash
cd tools/reso-reference-server
docker compose up -d
```

This starts:
- **UI** at `http://localhost:5173` (React SPA with nginx reverse proxy)
- **Server** at `http://localhost:8080` (OData API, `DB_BACKEND=postgres`)
- **PostgreSQL** at `localhost:5432`

Seed with test data:

```bash
docker compose --profile seed up seed
```

### MongoDB

```bash
cd tools/reso-reference-server
docker compose --profile mongodb up -d mongodb server-mongo ui-mongo
```

This starts:
- **UI** at `http://localhost:5173`
- **Server** at `http://localhost:8080` (OData API, `DB_BACKEND=mongodb`)
- **MongoDB** at `localhost:27017`

Seed with test data:

```bash
docker compose --profile seed-mongo up seed-mongo
```

### SQLite

```bash
cd tools/reso-reference-server
docker compose --profile sqlite up -d server-sqlite ui-sqlite
```

This starts:
- **UI** at `http://localhost:5173`
- **Server** at `http://localhost:8080` (OData API, `DB_BACKEND=sqlite`)
- No external database — SQLite file stored in a Docker volume

Seed with test data:

```bash
docker compose --profile sqlite --profile seed-sqlite up seed-sqlite
```

### Switching Between Backends

Stop the current backend and start the other:

```bash
# Stop everything and remove volumes
docker compose --profile mongodb --profile sqlite down -v

# Start with PostgreSQL
docker compose up -d
docker compose --profile seed up seed

# — or start with MongoDB —
docker compose --profile mongodb up -d mongodb server-mongo ui-mongo
docker compose --profile mongodb --profile seed-mongo up seed-mongo

# — or start with SQLite —
docker compose --profile sqlite up -d server-sqlite ui-sqlite
docker compose --profile sqlite --profile seed-sqlite up seed-sqlite
```

### Verify

```bash
# Health check
curl http://localhost:8080/health

# OData metadata
curl http://localhost:8080/\$metadata

# Browse the UI
open http://localhost:5173

# Query Property records via the API
curl -H 'Accept: application/json' 'http://localhost:8080/Property?\$top=5&\$select=ListPrice,City,StateOrProvince'

# Create a Property record
curl -X POST http://localhost:8080/Property \
  -H "Content-Type: application/json" \
  -H "Prefer: return=representation" \
  -H "Authorization: Bearer test" \
  -d '{"ListPrice": 250000, "City": "Austin", "StateOrProvince": "TX", "PostalCode": "78701", "Country": "US", "BedroomsTotal": 3}'
```

Seeding uses the data generator with automatic dependency resolution (`resolveDependencies: true`). A single seed call creates all resources in topological order with valid FK linkages: Office (10), Member (25), OUID (2), Teams (5), Property (50), plus child collections (Media, OpenHouse, Showing, Rooms, etc.) — 892 records total.

### Reseed (drop existing data)

```bash
docker compose down -v        # or: docker compose --profile mongodb down -v
docker compose up -d          # start your chosen backend
docker compose --profile seed up seed   # or: --profile seed-mongo up seed-mongo
```

## Architecture

```
reso-reference-server/
├── server/          # Node/Express/TypeScript OData server
├── ui/              # React SPA for browsing/editing records (Vite + Tailwind)
├── compliance/      # RESO compliance test infrastructure (Docker)
├── docker-compose.yml
└── CLAUDE.md        # Coding conventions
```

The server is **metadata-driven**: it reads `server-metadata.json` (RESO Data Dictionary 2.0) at startup and dynamically:

1. Creates database schema (PostgreSQL tables, MongoDB collections/indexes, or SQLite tables) for each target resource
2. Registers OData CRUD routes with proper headers, annotations, and error format
3. Generates EDMX XML metadata at `/$metadata`
4. Generates OpenAPI 3.0 documentation at `/api-docs`

The `DataAccessLayer` interface abstracts persistence, allowing the same OData handlers to work with PostgreSQL, MongoDB, or SQLite. See [server/README.md](server/README.md) for backend-specific details.

## Supported Resources

| Resource | Primary Key | Fields |
|----------|-------------|--------|
| Property | ListingKey | 652 |
| Member | MemberKey | 87 |
| Office | OfficeKey | 73 |
| Media | MediaKey | 41 |
| OpenHouse | OpenHouseKey | 26 |
| Showing | ShowingKey | 44 |
| PropertyGreenVerification | GreenVerificationKey | 15 |
| PropertyPowerProduction | PowerProductionKey | 12 |
| PropertyRooms | RoomKey | 19 |
| PropertyUnitTypes | UnitTypeKey | 17 |
| Teams | TeamKey | 45 |
| TeamMembers | TeamMemberKey | 21 |
| OUID | OUIDKey | 46 |

## OData Compliance

The server implements OData 4.01 features required by the RESO Web API Add/Edit Endorsement (RCP-010):

- `OData-Version: 4.01` response header
- `Prefer: return=representation` and `return=minimal` support
- `Location`, `EntityId`, `Preference-Applied` response headers
- `@odata.context`, `@odata.id`, `@odata.editLink`, `@odata.etag` annotations
- OData error format with `error.code`, `error.message`, `error.details[].target`
- OData key syntax: `/{Resource}('{key}')`
- EDMX 4.0 metadata at `/$metadata`

## API Endpoints

| Method | Path | Description |
|--------|------|-------------|
| GET | `/$metadata` | EDMX XML metadata document |
| GET | `/api-docs` | Swagger UI documentation |
| GET | `/health` | Health check |
| POST | `/oauth/token` | Mock OAuth2 token endpoint |
| GET | `/{Resource}` | Query collection (`$filter`, `$select`, `$orderby`, `$top`, `$skip`, `$count`, `$expand`) |
| POST | `/{Resource}` | Create a new record |
| GET | `/{Resource}('{key}')` | Get a record by key (supports `$expand`) |
| PATCH | `/{Resource}('{key}')` | Update a record |
| DELETE | `/{Resource}('{key}')` | Delete a record |
| GET | `/admin/data-generator/status` | Resource counts and available generators |
| POST | `/admin/data-generator` | Generate seed data (supports `resolveDependencies`) |

## Compliance Testing

The server includes Docker-based compliance testing against both RESO certification tools. Tests run against seeded data and validate OData protocol compliance, metadata structure, field mappings, and query behavior.

### Web API Core 2.0.0

Validates OData query operations (`$filter`, `$select`, `$orderby`, `$top`, `$skip`, `$count`, `$expand`), response formats, metadata, and service document compliance. Uses the RESO [web-api-commander](https://github.com/RESOStandards/web-api-commander).

**Current status: 42 passed, 0 failed, 3 skipped** (3 skipped: `has` operator tests, N/A for string enumerations)

```bash
# PostgreSQL
docker compose up -d --build --wait
docker compose --profile seed up --exit-code-from seed
docker compose --profile compliance-core up --build --exit-code-from compliance-core

# MongoDB
docker compose --profile mongodb up -d --build --wait mongodb server-mongo ui-mongo
docker compose --profile mongodb --profile seed-mongo up seed-mongo
docker compose --profile compliance-core-mongo up --build --exit-code-from compliance-core-mongo

# SQLite
docker compose --profile sqlite up -d --build --wait server-sqlite
docker compose --profile sqlite --profile seed-sqlite up seed-sqlite
docker compose --profile sqlite --profile compliance-core-sqlite up --build --exit-code-from compliance-core-sqlite
```

The test generates RESOScript XML configs dynamically from live server data (`compliance/generate-resoscripts.sh`), sampling records to find appropriate field values for each data type (integer, decimal, date, timestamp, single/multi-value lookups).

### Data Dictionary 2.0

Validates metadata compliance, field mappings, and data availability against the RESO Data Dictionary 2.0 specification. Uses the RESO [reso-certification-utils](https://github.com/RESOStandards/reso-certification-utils).

**Current status: 928 passed, 676 skipped, 0 schema validation errors**

```bash
# PostgreSQL
docker compose --profile compliance-dd up --build --exit-code-from compliance-dd

# MongoDB
docker compose --profile compliance-dd-mongo up --build --exit-code-from compliance-dd-mongo

# SQLite
docker compose --profile sqlite --profile compliance-dd-sqlite up --build --exit-code-from compliance-dd-sqlite
```

### Web API Add/Edit (RCP-010)

```bash
cd ../certification/add-edit
npx reso-cert-add-edit \
  --url http://localhost:8080 \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token test
```

### CI/CD

Compliance tests run automatically on push to `main` and on pull requests via GitHub Actions (`.github/workflows/compliance.yml`). Both PostgreSQL and MongoDB backends are tested in parallel. Results are uploaded as build artifacts.

## Development

See [server/README.md](server/README.md) for development setup and testing instructions.

## License

See [LICENSE](../../License.txt) in the repository root.
