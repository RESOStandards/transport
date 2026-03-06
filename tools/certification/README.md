# @reso/certification

RESO certification testing toolkit for validating OData 4.01 server compliance against RESO Web API endorsement specifications. Ships as a single package with spec-specific modules for Add/Edit (RCP-010), Web API Core, and Data Dictionary.

Uses shared test infrastructure (`src/test-runner/`) providing OData protocol validators, metadata parsing (via [`@reso/odata-client`](../odata-client/)), field validation (via [`@reso/validation`](../validation/)), reporting, and HTTP client helpers.

## Install

```bash
npm install @reso/certification
```

## Project Structure

```
certification/
├── src/
│   ├── index.ts              # Public SDK exports
│   ├── cli/index.ts          # Unified CLI entry point (bin: reso-cert)
│   ├── test-runner/          # Shared infrastructure (validators, client, auth, metadata, reporter)
│   ├── add-edit/             # RCP-010 Add/Edit scenarios + mock server
│   ├── web-api-core/         # Web API Core 2.0.0 (stub)
│   └── data-dictionary/      # Data Dictionary 2.0 (stub)
├── tests/                    # 60 Vitest tests
├── sample-payloads/          # JSON payload files for Add/Edit testing
├── sample-metadata.xml       # Sample EDMX metadata for mock server
└── Dockerfile                # Multi-stage Docker build for CI
```

## Certification Modules

### Add/Edit (RCP-010)

Validates OData CRUD operations against 8 Gherkin BDD certification scenarios:

1. **Create with `Prefer: return=representation`** — POST returns 201 with entity, OData annotations, and follow-up GET
2. **Create with `Prefer: return=minimal`** — POST returns 204 with headers only, and follow-up GET
3. **Create with invalid payload** — POST returns 400 with OData error format and field-level details
4. **Update with `Prefer: return=representation`** — PATCH returns 200 with entity and `@odata.etag`
5. **Update with `Prefer: return=minimal`** — PATCH returns 204 with headers only
6. **Update with invalid payload** — PATCH returns 400 with OData error format
7. **Delete succeeds** — DELETE returns 204, follow-up GET returns 404
8. **Delete fails** — DELETE to non-existent resource returns 4xx

**Current status: 8 passed, 0 failed**

### Web API Core 2.0.0 (stub)

Future home of programmatic Web API Core certification. Currently tested via the RESO [web-api-commander](https://github.com/RESOStandards/web-api-commander) Docker workflow.

**Current status: 42 passed, 0 failed, 3 skipped** (via Docker)

### Data Dictionary 2.0 (stub)

Future home of programmatic Data Dictionary certification. Currently tested via [reso-certification-utils](https://github.com/RESOStandards/reso-certification-utils) Docker workflow.

**Current status: 1,034 passed, 570 skipped, 0 failed** (via Docker)

## CLI Usage

```bash
npm run build

# Run Add/Edit against a live server
npx reso-cert \
  --url https://api.example.com \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token <token>

# Run against the built-in mock server (offline testing / CI)
npx reso-cert \
  --url http://localhost:8800 \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token test \
  --mock

# Generate a compliance report
npx reso-cert \
  --url http://localhost:8080 \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token test \
  --compliance-report ./report.json \
  --spec-version 2.0.0
```

### CLI Options

| Option | Description |
|--------|-------------|
| `--url <url>` | OData server base URL |
| `--resource <name>` | OData resource name (e.g., `Property`) |
| `--payloads <dir>` | Directory containing payload JSON files |
| `--auth-token <token>` | Pre-fetched bearer token |
| `--client-id <id>` | OAuth2 client ID (alternative to `--auth-token`) |
| `--client-secret <secret>` | OAuth2 client secret |
| `--token-url <url>` | OAuth2 token endpoint URL |
| `--metadata <path>` | Path to local XML metadata file |
| `--mock` | Start a built-in mock OData server on port 8800 |
| `--output <format>` | Output format: `console` (default) or `json` |
| `--compliance-report <path>` | Write structured JSON compliance report to file |
| `--spec-version <version>` | Spec version for the report (default: `2.0.0`) |

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | All scenarios passed |
| 1 | One or more scenarios failed |
| 2 | Runtime error |

## Programmatic API

```typescript
import {
  runAllScenarios,
  generateComplianceReport,
  startMockServer,
  stopMockServer
} from '@reso/certification';

// Run all 8 Add/Edit scenarios
const report = await runAllScenarios({
  serverUrl: 'http://localhost:8080',
  resource: 'Property',
  payloadsDir: './sample-payloads',
  auth: { mode: 'token', authToken: 'test' }
});

console.log(`${report.summary.passed} passed, ${report.summary.failed} failed`);
```

### Shared Test Infrastructure

The test-runner module is also exported for building custom certification scenarios:

```typescript
import {
  // Validators
  validateODataVersionHeader,
  validateStatusCode,
  validateLocationHeader,
  validateODataAnnotation,
  validateODataError,
  validateResponseContainsPayload,

  // Client
  odataRequest,
  buildResourceUrl,

  // Metadata
  fetchMetadata,
  parseMetadataXml,
  getEntityType,
  validatePayloadAgainstMetadata,

  // Auth
  resolveAuthToken,

  // Reporter
  formatConsoleReport,
  formatJsonReport,

  // Helpers
  extractPrimaryKey,
  buildScenarioResult
} from '@reso/certification';
```

## Docker Compliance Testing

Run compliance tests against the [RESO Reference Server](../reso-reference-server/) using Docker Compose.

### Setup

```bash
cd ../reso-reference-server

# Start server and seed data
docker compose up -d --build --wait
docker compose --profile seed up --exit-code-from seed seed
```

### Web API Core 2.0.0

```bash
docker compose --profile compliance-core up --build --exit-code-from compliance-core
```

### Add/Edit (RCP-010)

```bash
docker compose --profile compliance-addedit up --build --exit-code-from compliance-addedit
```

### Data Dictionary 2.0

```bash
docker compose --profile compliance-dd up --build --exit-code-from compliance-dd
```

### Local CLI (without Docker)

```bash
# Against a running reference server
npx reso-cert \
  --url http://localhost:8080 \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token test \
  --compliance-report ./compliance-report.json \
  --spec-version 2.0.0
```

## Compliance Report Format

The `--compliance-report` flag generates a structured JSON file suitable for API submission. The report captures the outcome of each certification scenario along with the fields, enumerations, and expansions tested.

### Top-Level Structure

```json
{
  "description": "Web API Add/Edit",
  "version": "2.0.0",
  "generatedOn": "2026-03-06T09:00:00.000Z",
  "outcome": "passed",
  "remarks": "Tested Create (representation, minimal), Update (representation, minimal), and Delete operations on Property with 5 of 652 fields.",
  "scenarios": [...]
}
```

| Field | Type | Description |
|-------|------|-------------|
| `description` | `string` | Endorsement name (`"Web API Add/Edit"`) |
| `version` | `string` | Spec version (from `--spec-version`) |
| `generatedOn` | `string` | ISO 8601 timestamp |
| `outcome` | `"passed" \| "failed"` | Overall result |
| `remarks` | `string` | Human-readable summary of operations, fields, and any failures |
| `scenarios` | `ScenarioDetail[]` | Per-scenario results |

### Scenario Detail

Each scenario includes the operation type, response preference, tested fields, enumerations, and any failures:

```json
{
  "name": "create-succeeds-representation",
  "operation": "Create",
  "preference": "return=representation",
  "resource": "Property",
  "status": "passed",
  "durationMs": 42,
  "fields": {
    "count": 5,
    "names": ["ListPrice", "City", "StateOrProvince", "PostalCode", "Country"]
  },
  "enumerations": {
    "count": 1,
    "values": [{ "fieldName": "StandardStatus", "value": "Active" }]
  },
  "expansions": [],
  "failures": []
}
```

| Field | Type | Description |
|-------|------|-------------|
| `name` | `string` | Scenario identifier (e.g., `create-succeeds-representation`) |
| `operation` | `"Create" \| "Update" \| "Delete"` | CRUD operation type |
| `preference` | `string?` | `return=representation` or `return=minimal` (omitted for Delete) |
| `resource` | `string` | OData resource tested |
| `status` | `"passed" \| "failed"` | Scenario result |
| `durationMs` | `number` | Execution time in milliseconds |
| `fields.count` | `number` | Number of non-key fields in the payload |
| `fields.names` | `string[]` | Field names tested |
| `enumerations.count` | `number` | Number of enum fields with values |
| `enumerations.values` | `{fieldName, value}[]` | Enum field names and their test values |
| `expansions` | `string[]` | Navigation properties included in the payload |
| `failures` | `FailureDetail[]` | Assertion failures (empty when passed) |

### Failure Detail

When a scenario fails, each failing assertion is captured:

```json
{
  "assertion": "OData-Version header is 4.0 or 4.01",
  "expected": "4.0 or 4.01",
  "actual": "(missing)"
}
```

## Payload Format

Place JSON files in the payloads directory. The tool expects 6 files:

| File | Purpose |
|------|---------|
| `create-succeeds.json` | Known-good payload for POST |
| `create-fails.json` | Known-bad payload for POST (e.g., negative ListPrice) |
| `update-succeeds.json` | Known-good payload for PATCH (includes primary key) |
| `update-fails.json` | Known-bad payload for PATCH |
| `delete-succeeds.json` | `{ "id": "<key>" }` — existing record to delete |
| `delete-fails.json` | `{ "id": "<key>" }` — non-existent record |

Known-bad payloads use negative numeric values to trigger server-side validation errors (e.g., `"ListPrice": -1`).

## Authentication

| Method | Options |
|--------|---------|
| Bearer token | `--auth-token <token>` |
| OAuth2 Client Credentials | `--client-id <id> --client-secret <secret> --token-url <url>` |

When using `--mock`, the mock server provides its own OAuth2 token endpoint automatically.

## Mock Server

The `--mock` flag starts a built-in Express-based OData mock server on port 8800 that implements enough of the OData protocol to exercise all 8 scenarios. Useful for offline testing and CI.

The mock server:
- Serves EDMX metadata at `/$metadata`
- Implements CRUD at `/{Resource}` and `/{Resource}('{key}')`
- Returns OData-compliant headers, annotations, and error responses
- Triggers 400 validation errors for negative numeric field values
- Provides a mock OAuth2 token endpoint at `/oauth/token`

## Development

```bash
npm install
npm run build
npm test        # 60 tests
```

## License

See [LICENSE](../../License.txt) in the repository root.
