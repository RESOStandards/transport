# @reso/data-generator

Generates realistic RESO Data Dictionary test data for OData servers. Supports three output modes (HTTP, JSON, curl) and includes domain-specific generators for Property, Member, Office, Media, OpenHouse, and Showing resources.

## CLI Usage

### Interactive Mode

```bash
npx reso-data-generator
```

Prompts for output format, server URL, auth token, resource, record count, and related record configuration.

### Non-Interactive Mode

```bash
# POST 50 Properties with related records to server
npx reso-data-generator -u http://localhost:8080 -r Property -n 50 \
  --related Media:5,OpenHouse:2,Showing:2 -t admin-token

# Generate JSON files
npx reso-data-generator -r Property -n 10 -f json -o ./seed-data

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
| `-t, --auth-token <token>` | Bearer token | — |
| `-f, --format <format>` | Output: `http`, `json`, or `curl` | `http` |
| `-o, --output <path>` | Output directory (json) or file (curl) | — |

## Output Modes

- **`http`** — POSTs records to the OData server via the Add/Edit API
- **`json`** — Writes individual JSON files to `<outputDir>/<resource>/<0001..N>.json`
- **`curl`** — Generates a `seed.sh` bash script with curl commands and a health-check loop

## Resource Generators

Each resource has a specialized generator that applies domain-specific overrides on top of generic field generation.

**Property** — Realistic addresses, pricing (ListPrice, ListPriceLow with constraints), structure (bedrooms, bathrooms, living area), geolocation, property type/status, listing dates, public remarks.

**Member** — Agent/broker profiles with names, email (firstname.lastname@domain), phone numbers, designations (CRS, ABR, GRI, etc.), NAR IDs.

**Office** — Brokerage office records with addresses and contact info.

**Media** — Image records linked to parent resources via RESO FK convention (ResourceName + ResourceRecordKey). Includes placeholder URLs, descriptions, ordering, and MIME types.

**OpenHouse** — Open house events linked to parent properties.

**Showing** — Showing appointments linked to parent properties.

## Programmatic API

```typescript
import { generateSeedData, getGenerator, buildSeedPlan } from '@reso/data-generator';

const result = await generateSeedData(seedOptions, outputOptions, (progress) => {
  console.log(`${progress.resource}: ${progress.created}/${progress.total}`);
});
```

### Key Exports

- `generateSeedData(options, output, onProgress?)` — Main entry point for data generation
- `getGenerator(resourceName)` — Returns the domain-specific generator for a resource
- `buildSeedPlan(config)` — Builds a generation plan from configuration
- `getRelatedResources(parent, fieldsByResource)` — Discovers related resources via FK convention
- `getDefaultRelatedCount(resource)` — Default counts: Media=5, OpenHouse=2, Showing=2

## Metadata Integration

The CLI fetches field metadata and lookup values from the server's metadata API endpoints (`/api/metadata/fields`, `/api/metadata/lookups-for-resource`) to generate type-correct values for all fields including enums and collections.
