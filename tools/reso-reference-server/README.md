# RESO Reference OData Server

A metadata-driven OData 4.01 reference server for the [RESO Data Dictionary](https://www.reso.org/data-dictionary/). Reads the RESO JSON metadata format and dynamically generates PostgreSQL tables, OData CRUD endpoints, EDMX metadata, and OpenAPI documentation.

## Quick Start (Docker)

```bash
docker-compose up -d
```

This starts:
- **Server** at `http://localhost:8080`
- **PostgreSQL** at `localhost:5432`

### Verify

```bash
# Health check
curl http://localhost:8080/health

# OData metadata
curl http://localhost:8080/\$metadata

# OpenAPI docs
open http://localhost:8080/api-docs

# Create a Property record
curl -X POST http://localhost:8080/Property \
  -H "Content-Type: application/json" \
  -H "Prefer: return=representation" \
  -H "Authorization: Bearer test" \
  -d '{"ListPrice": 250000, "City": "Austin", "BedroomsTotal": 3}'
```

## Architecture

```
reso-reference-server/
├── server/          # Node/Express/TypeScript OData server
├── ui/              # React UI for browsing records (Stage 2)
├── docker-compose.yml
└── CLAUDE.md        # Coding conventions
```

The server is **metadata-driven**: it reads `server-metadata.json` (RESO Data Dictionary v1.7) at startup and dynamically:

1. Creates PostgreSQL tables for each target resource (Property, Member, Office, Media, OpenHouse, Showing)
2. Registers OData CRUD routes with proper headers, annotations, and error format
3. Generates EDMX XML metadata at `/$metadata`
4. Generates OpenAPI 3.0 documentation at `/api-docs`

## Supported Resources

| Resource | Primary Key | Fields |
|----------|-------------|--------|
| Property | ListingKey | 300+ |
| Member | MemberKey | 70+ |
| Office | OfficeKey | 50+ |
| Media | MediaKey | 30+ |
| OpenHouse | OpenHouseKey | 30+ |
| Showing | ShowingKey | 20+ |

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
