# @reso/certification-add-edit

RESO Web API Add/Edit Endorsement (RCP-010) compliance testing tool. Sends known-good and known-bad JSON payloads to OData 4.01 servers and validates responses against 8 Gherkin BDD certification scenarios.

Uses [`@reso/odata-client`](../../odata-client/) for HTTP requests, authentication, and CSDL metadata parsing.

## Install

```bash
npm install @reso/certification-add-edit
```

## Usage

### CLI

```bash
# Run against a live server
npx reso-cert-add-edit \
  --url https://api.example.com \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token <token>

# Run against the built-in mock server
npx reso-cert-add-edit \
  --url http://localhost:8800 \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token test \
  --mock
```

### CLI Options

| Option | Description |
|--------|-------------|
| `--url <url>` | OData server base URL |
| `--resource <name>` | Resource to test (e.g., `Property`) |
| `--payloads <dir>` | Directory containing JSON payload files |
| `--auth-token <token>` | Bearer token for authentication |
| `--client-id <id>` | OAuth2 client ID (alternative to `--auth-token`) |
| `--client-secret <secret>` | OAuth2 client secret |
| `--token-url <url>` | OAuth2 token endpoint URL |
| `--mock` | Start a built-in mock OData server on port 8800 |
| `--json` | Output results as JSON instead of console format |

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | All scenarios passed |
| 1 | One or more scenarios failed |
| 2 | Runtime error |

## Certification Scenarios

The tool validates 8 Gherkin BDD scenarios from the RESO Web API Add/Edit Endorsement:

1. **Create with valid payload** — POST returns 201/204 with correct headers
2. **Create with `Prefer: return=representation`** — Response includes the created entity
3. **Create with invalid field** — Server returns 400 with OData error format
4. **Create with invalid value** — Server returns 400 with field-level error details
5. **Read created record** — GET by key returns the entity with matching values
6. **Update with valid payload** — PATCH returns 200/204
7. **Update with invalid value** — PATCH returns 400 with OData error format
8. **Delete created record** — DELETE returns 204

## Validators

12 built-in validators check OData compliance:

- `OData-Version` response header
- `Content-Type` header format
- `Location` and `EntityId` headers on create
- `Preference-Applied` header when `Prefer` is sent
- `@odata.context`, `@odata.id`, `@odata.editLink`, `@odata.etag` annotations
- OData error format with `error.code`, `error.message`, `error.details[].target`
- HTTP status codes (201, 200, 204, 400)

## Payload Format

Place JSON files in the payloads directory. Files with `invalid` in the name are treated as known-bad payloads.

```
sample-payloads/
├── valid-property.json         # Known-good payload
├── invalid-negative-price.json # Known-bad: negative ListPrice
└── invalid-missing-field.json  # Known-bad: required field missing
```

## Mock Server

The `--mock` flag starts a built-in Express-based OData mock server on port 8800 that implements just enough of the OData protocol to exercise all 8 scenarios. Useful for offline testing and CI.

## Programmatic API

```typescript
import { runAllScenarios } from '@reso/certification-add-edit';

const results = await runAllScenarios({
  serverUrl: 'http://localhost:8080',
  resource: 'Property',
  payloadDir: './sample-payloads',
  auth: { mode: 'token', authToken: 'test' }
});

for (const scenario of results) {
  console.log(`${scenario.name}: ${scenario.passed ? 'PASS' : 'FAIL'}`);
}
```

## Cross-Tool Validation

Run the compliance tests against the [RESO Reference Server](../../reso-reference-server/):

```bash
# Start the reference server
cd ../../reso-reference-server && docker-compose up -d

# Run compliance tests
npx reso-cert-add-edit \
  --url http://localhost:8080 \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token test
```

## Development

```bash
npm install
npm run build
npm test        # 49 tests
```

## License

See [LICENSE](../../../License.txt) in the repository root.
