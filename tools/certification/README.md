# @reso/certification

RESO certification testing tools for validating OData 4.01 server compliance against RESO Web API endorsement specifications. Each subdirectory implements an independent certification module that can be used as a library or run from the command line.

All certification modules use the shared [`test-runner/`](test-runner/) infrastructure, which provides OData protocol validators, metadata parsing (via [`@reso/odata-client`](../odata-client/)), field validation (via [`@reso/validation`](../validation/)), reporting, and HTTP client helpers.

## Packages

### [test-runner/](test-runner/) — Shared Test Infrastructure

Generic OData certification test framework used by all certification modules. Provides:
- OData protocol validators (status codes, headers, annotations, error responses)
- Test report formatting (console and JSON)
- Metadata loading, parsing, and payload validation
- OData HTTP client and authentication helpers
- Common test helpers (key extraction, schema assertions, scenario result building)

### [add-edit/](add-edit/) — Web API Add/Edit Endorsement (RCP-010)

Validates OData CRUD operations against 8 Gherkin BDD certification scenarios defined in the RESO Web API Add/Edit specification. Tests Create, Read, Update, and Delete flows with known-good and known-bad payloads, checking response status codes, OData headers, annotations, and error formats.

## Usage

### As a CLI Tool

Each module provides a CLI binary. Install the module and run it directly:

```bash
cd add-edit && npm install && npm run build

# Run against a live server
npx reso-cert-add-edit \
  --url https://api.example.com \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token <token>

# Run against the built-in mock server (for offline testing / CI)
npx reso-cert-add-edit \
  --url http://localhost:8800 \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token test \
  --mock
```

### As a Library

Import the module's programmatic API in your own test harness:

```typescript
import { runAllScenarios } from '@reso/certification-add-edit';

const report = await runAllScenarios({
  serverUrl: 'http://localhost:8080',
  resource: 'Property',
  payloadsDir: './sample-payloads',
  auth: { mode: 'token', authToken: 'test' }
});

console.log(`${report.summary.passed} passed, ${report.summary.failed} failed`);
```

### Cross-Tool Validation

Run certification tests against the [RESO Reference Server](../reso-reference-server/):

```bash
# Start the reference server (requires Docker)
cd ../reso-reference-server && docker-compose up -d

# Run Add/Edit certification
cd ../certification/add-edit
npx reso-cert-add-edit \
  --url http://localhost:8080 \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token test
```

## Authentication

All modules support two authentication methods:

| Method | Options |
|--------|---------|
| Bearer token | `--auth-token <token>` |
| OAuth2 Client Credentials | `--client-id <id> --client-secret <secret> --token-url <url>` |

When using `--mock`, the mock server provides its own OAuth2 token endpoint automatically.

## Development

```bash
# Install and test a specific module
cd add-edit && npm install && npm test

# Run all certification tests from the repo root
cd ../.. && npm run test:certification
```

## License

See [LICENSE](../../License.txt) in the repository root.
