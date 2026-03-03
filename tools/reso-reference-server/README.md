# RESO Reference OData Server

A metadata-driven OData 4.01 reference server for the [RESO Data Dictionary](https://www.reso.org/data-dictionary/). Reads the RESO JSON metadata format and dynamically generates PostgreSQL tables, OData CRUD endpoints, EDMX metadata, and OpenAPI documentation.

## Quick Start (Docker)

```bash
cd tools/reso-reference-server
docker compose up -d
```

This starts:
- **UI** at `http://localhost:5173` (React SPA with nginx reverse proxy)
- **Server** at `http://localhost:8080` (OData API)
- **PostgreSQL** at `localhost:5432`

### Seed with Test Data

```bash
docker compose --profile seed up seed
```

This generates 50 Property records (with Media, OpenHouse, Showing), 20 Members,
and 10 Offices using the built-in data generator.

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

### Reseed (drop existing data)

```bash
docker compose down -v
docker compose up -d
docker compose --profile seed rm -f seed
docker compose --profile seed up seed
```

## Architecture

```
reso-reference-server/
├── server/          # Node/Express/TypeScript OData server
├── ui/              # React SPA for browsing/editing records (Vite + Tailwind)
├── docker-compose.yml
└── CLAUDE.md        # Coding conventions
```

The server is **metadata-driven**: it reads `server-metadata.json` (RESO Data Dictionary 2.0) at startup and dynamically:

1. Creates PostgreSQL tables for each target resource (Property, Member, Office, Media, OpenHouse, Showing)
2. Registers OData CRUD routes with proper headers, annotations, and error format
3. Generates EDMX XML metadata at `/$metadata`
4. Generates OpenAPI 3.0 documentation at `/api-docs`

## Supported Resources

| Resource | Primary Key | Fields |
|----------|-------------|--------|
| Property | ListingKey | 652 |
| Member | MemberKey | 87 |
| Office | OfficeKey | 73 |
| Media | MediaKey | 35 |
| OpenHouse | OpenHouseKey | 34 |
| Showing | ShowingKey | 22 |

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
| POST | `/{Resource}` | Create a new record |
| GET | `/{Resource}('{key}')` | Get a record by key |
| PATCH | `/{Resource}('{key}')` | Update a record |
| DELETE | `/{Resource}('{key}')` | Delete a record |

## Cross-Tool Validation

This server is designed to pass the RESO Web API Add/Edit compliance test tool:

```bash
cd ../certification/add-edit
npx reso-cert-add-edit \
  --url http://localhost:8080 \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token test
```

## Development

See [server/README.md](server/README.md) for development setup and testing instructions.

## License

See [LICENSE](../../License.txt) in the repository root.
