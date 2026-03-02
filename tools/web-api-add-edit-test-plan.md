# RESO Web API Add/Edit Testing Tool — Implementation Plan

## Context

The RESO Web API Add/Edit Endorsement (RCP-010) defines how OData servers should handle Create, Update, and Delete operations. Section 3 of the spec contains Gherkin BDD scenarios that define certification testing rules. This tool automates those tests — it sends payloads to a server and validates that responses conform to the specification.

The transport repo is currently a pure documentation/specs repo with no code. We're bootstrapping a new TypeScript project at `tools/web-api-add-edit-test/`.

## Architecture

Two layers:
- **SDK** (`src/lib/`) — Programmatic API: load metadata, send requests, validate responses, produce a test report
- **CLI** (`src/cli/`) — `testWebApiAddEdit` command using the SDK
- **Mock Server** (`src/mock/`) — Local OData-compliant mock for offline testing

## CLI Interface

```
testWebApiAddEdit \
  --url https://api.reso.org \
  --resource Property \
  --payloads ./test-payloads/ \
  --auth-token <bearer-token> \
  [--metadata ./metadata.xml] \
  [--mock] \
  [--output console|json]
```

The `--payloads` directory contains per-action files matching the Gherkin scenario names:
- `create-succeeds.json`, `create-fails.json`
- `update-succeeds.json`, `update-fails.json`
- `delete-succeeds.json`, `delete-fails.json`

When `--mock` is used, a local Express server starts and `--url` is overridden to point to it. If `--metadata` is also provided, the mock serves that XML at `/$metadata`.

## 8 Test Scenarios (from Section 3 Gherkin)

| # | Scenario | Method | Prefer Header | Expected Status |
|---|----------|--------|---------------|-----------------|
| 1 | Create succeeds (representation) | POST | return=representation | 201 or 204 |
| 2 | Create succeeds (minimal) | POST | return=minimal | 201 or 204 |
| 3 | Create fails | POST | return=representation | 400 |
| 4 | Update succeeds (representation) | PATCH | return=representation | 200 or 204 |
| 5 | Update succeeds (minimal) | PATCH | return=minimal | 200 or 204 |
| 6 | Update fails | PATCH | return=representation | 400 |
| 7 | Delete succeeds | DELETE | n/a | 204 |
| 8 | Delete fails | DELETE | n/a | 400-499 |

Each scenario validates headers, status codes, body structure, and OData annotations per the Gherkin steps. Success scenarios include a follow-up GET to the `Location` header URL to verify persistence.

## Project Structure

```
tools/web-api-add-edit-test/
├── package.json
├── tsconfig.json
├── vitest.config.ts
├── sample-metadata.xml
├── sample-payloads/
│   ├── create-succeeds.json
│   ├── create-fails.json
│   ├── update-succeeds.json
│   ├── update-fails.json
│   ├── delete-succeeds.json
│   └── delete-fails.json
├── src/
│   ├── index.ts                 # SDK public exports
│   ├── cli/
│   │   └── index.ts             # CLI entry (commander)
│   ├── lib/
│   │   ├── types.ts             # All interfaces
│   │   ├── metadata.ts          # XML metadata parser (fast-xml-parser)
│   │   ├── client.ts            # HTTP client wrapper (fetch)
│   │   ├── validators.ts        # Response validation (pure functions)
│   │   ├── test-runner.ts       # Scenario orchestrator
│   │   └── reporter.ts          # Console/JSON output formatting
│   └── mock/
│       ├── handlers.ts          # Mock route logic
│       └── server.ts            # Express server setup
└── tests/
    ├── metadata.test.ts
    ├── validators.test.ts
    └── integration.test.ts
```

## Dependencies

- `commander` — CLI argument parsing
- `express` — Mock server
- `fast-xml-parser` — OData XML metadata parsing
- Node.js built-in `fetch` (Node 18+) — HTTP requests
- `vitest` — Testing
- `typescript` — Build

## Implementation Order

| Step | Files | Notes |
|------|-------|-------|
| 1 | `package.json`, `tsconfig.json`, `vitest.config.ts`, `.gitignore` update | Scaffold, then `npm install` |
| 2 | `src/lib/types.ts` | All interfaces — foundation for everything |
| 3 | `sample-metadata.xml`, `sample-payloads/*.json` | Copy examples from the spec |
| 4 | `src/lib/metadata.ts` | XML parser + payload-vs-metadata validation |
| 5 | `src/lib/client.ts` | Thin fetch wrapper with OData headers |
| 6 | `src/lib/validators.ts` | Pure validation functions mapping to Gherkin steps |
| 7 | `tests/metadata.test.ts`, `tests/validators.test.ts` | Unit tests, run to verify |
| 8 | `src/lib/reporter.ts` | Console + JSON output formatting |
| 9 | `src/mock/handlers.ts`, `src/mock/server.ts` | Mock OData server |
| 10 | `src/lib/test-runner.ts` | Orchestrates all 8 scenarios |
| 11 | `src/index.ts` | SDK public API re-exports |
| 12 | `src/cli/index.ts` | CLI entry point |
| 13 | `tests/integration.test.ts` | End-to-end test against mock server |

## Key Design Decisions

1. **Payload target for Update/Delete**: Update payloads use `@reso.target` to specify the record key (stripped before sending as PATCH body). Delete payloads use `{ "id": "12345" }` or `{ "url": "..." }`.

2. **Validators are pure functions**: Each takes an `ODataResponse` → returns `TestAssertion[]`. Maps 1:1 to Gherkin `Then` steps. Easy to unit test independently.

3. **Header normalization**: Client normalizes all response headers to lowercase keys immediately to avoid case-sensitivity issues.

4. **Sequential scenario execution**: Scenarios run one at a time since create must complete before delete can target the created record on a real server.

5. **Mock server port**: `port: 0` (OS-assigned) for programmatic/test use; `8800` default for CLI `--mock`.

6. **Mock validation logic**: Negative numeric values trigger 400 errors (matching the spec's examples). This provides a deterministic way to test error scenarios.

## Verification

1. After step 7: `npm test` — unit tests for metadata parser and validators pass
2. After step 13: `npm test` — integration test runs all 8 scenarios against mock, all pass
3. Manual CLI test: `npx testWebApiAddEdit --mock --resource Property --payloads ./sample-payloads --auth-token test --url http://localhost` — all scenarios pass with console output
