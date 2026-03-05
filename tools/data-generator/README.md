# @reso/data-generator

Generates realistic RESO Data Dictionary test data for OData servers. Supports three output modes (HTTP, JSON, curl), automatic dependency resolution with FK linkage, and domain-specific generators for Property, Member, Office, Media, OpenHouse, Showing, Teams, TeamMembers, OUID, and Property child resources.

## CLI Usage

### Interactive Mode

```bash
npx reso-data-generator
```

Prompts for output format, server URL, auth token, resource, record count, dependency resolution, and related record configuration.

### Non-Interactive Mode

```bash
# POST 50 Properties with auto-resolved dependencies (Member, Office, Teams, OUID)
npx reso-data-generator -u http://localhost:8080 -r Property -n 50 \
  --related Media:5,OpenHouse:2,Showing:2 -t admin-token

# Generate without dependency resolution
npx reso-data-generator -r Property -n 10 --no-deps -f json -o ./seed-data

# Override dependency counts
npx reso-data-generator -r Property -n 50 --dep-counts Office:20,Member:100

# Generate curl seed script
npx reso-data-generator -r Property -n 10 -f curl -o ./seed.sh -t admin-token
```

### Options

| Flag | Description | Default |
|------|-------------|---------|
| `-u, --url <url>` | Server URL | `http://localhost:8080` |
| `-r, --resource <name>` | Resource to generate (required for non-interactive) | — |
| `-n, --count <number>` | Number of records | `10` |
| `--related <spec>` | Related records (`Resource:count,...`) | — |
| `--deps / --no-deps` | Enable/disable dependency resolution | `true` |
| `--dep-counts <spec>` | Override dependency counts (`Resource:count,...`) | — |
| `-t, --auth-token <token>` | Bearer token | — |
| `-f, --format <format>` | Output: `http`, `json`, or `curl` | `http` |
| `-o, --output <path>` | Output directory (json) or file (curl) | — |

## Dependency Resolution

When `--deps` is enabled (the default), the generator automatically discovers FK relationships from metadata and creates dependency resources in topological order before the requested resource.

For example, requesting 50 Property records will automatically create:
- Office records (ceil(50/5) = 10)
- Member records (ceil(50/2) = 25) with valid `OfficeKey` references
- OUID records (2)
- Teams records (ceil(50/10) = 5)
- Then Property records with valid `ListAgentKey`, `ListOfficeKey`, etc.

The **Office ↔ Member circular dependency** is handled via deferred back-fill: Office is created first (without broker/manager FKs), then Member (with valid `OfficeKey`), then Office records are PATCHed with `OfficeBrokerKey`/`OfficeManagerKey` pointing to real Member keys.

All three output modes support dependency resolution:
- **HTTP** — POSTs dependencies first, then PATCHes back-fill records
- **JSON** — Creates subdirectories per resource in dependency order
- **curl** — Generates POST commands in order, followed by PATCH commands for back-fill

## Output Modes

- **`http`** — POSTs records to the OData server via the Add/Edit API
- **`json`** — Writes individual JSON files to `<outputDir>/<resource>/<0001..N>.json`
- **`curl`** — Generates a `seed.sh` bash script with curl commands and a health-check loop

## Resource Generators

Each resource has a specialized generator that applies domain-specific overrides on top of generic field generation.

**Property** — Realistic addresses, pricing (ListPrice, ListPriceLow with constraints), structure (bedrooms, bathrooms, living area), geolocation, property type/status, listing dates, public remarks, tax data (state-specific rates), expense fields.

**Member** — Agent/broker profiles with names, email (firstname.lastname@domain), phone numbers, designations (CRS, ABR, GRI, etc.), NAR IDs.

**Office** — Brokerage office records with addresses and contact info.

**Media** — Image records linked to parent resources via RESO FK convention (ResourceName + ResourceRecordKey). Includes placeholder URLs, descriptions, and ordering.

**OpenHouse** — Open house events linked to parent properties via ListingKey.

**Showing** — Showing appointments linked to parent properties via ListingKey.

**PropertyRooms, PropertyGreenVerification, PropertyPowerProduction, PropertyUnitTypes** — Child collection resources linked to Property via ListingKey, generated with a generic child generator.

A generic field generator handles all Edm types (String, Boolean, Int16/32/64, Decimal, Date, DateTimeOffset, TimeOfDay, Guid) and enum/collection lookups from metadata.

## Programmatic API

```typescript
import { generateSeedData, generateWithDependencies, getGenerator, buildSeedPlan } from '@reso/data-generator';

// Generate with automatic dependency resolution
const result = await generateWithDependencies(seedOptions, outputOptions, metadata, (progress) => {
  console.log(`${progress.resource}: ${progress.created}/${progress.total}`);
});

// Generate a single resource without dependencies
const result = await generateSeedData(seedOptions, outputOptions, (progress) => {
  console.log(`${progress.resource}: ${progress.created}/${progress.total}`);
});
```

### Key Exports

- `generateSeedData(options, output, onProgress?)` — Single-resource data generation
- `generateWithDependencies(options, output, metadata, onProgress?)` — Multi-resource generation with FK resolution
- `getGenerator(resourceName)` — Returns the domain-specific generator for a resource
- `buildSeedPlan(config)` — Builds a generation plan from configuration
- `getRelatedResources(parent, fieldsByResource)` — Discovers child collection resources via FK convention
- `getDefaultRelatedCount(resource)` — Default counts: Media=5, OpenHouse=2, Showing=2, Rooms=3, etc.
- `discoverForeignKeys(resource, fields, fieldsByResource, keyFieldMap)` — Discovers to-one FK relationships from metadata
- `buildDependencyGraph(targetResources, fieldsByResource, keyFieldMap)` — Builds a resource dependency graph
- `topologicalSort(dependencies, targetResources)` — Returns seed phases with cycle breaking

## Metadata Integration

The CLI fetches field metadata and lookup values from the server's metadata API endpoints (`/api/metadata/fields`, `/api/metadata/lookups-for-resource`) to generate type-correct values for all fields including enums and collections.

## Tests

104 tests across 5 test files covering generators, field generation, FK resolution, dependency graph, and client output modes.
