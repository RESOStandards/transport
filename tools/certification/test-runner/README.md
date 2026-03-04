# @reso/certification-test-runner

Generic OData 4.01 certification test infrastructure for RESO compliance testing. Provides validators, reporters, an HTTP client, auth helpers, and metadata utilities used by certification modules like `@reso/certification-add-edit`.

## Overview

This package is a shared framework, not a standalone test runner. Certification modules import its building blocks to implement specific RCP test scenarios.

```typescript
import {
  odataRequest, buildResourceUrl,
  resolveAuthToken,
  fetchMetadata, parseMetadataXml, getEntityType, validatePayloadAgainstMetadata,
  validateStatusCode, validateODataVersionHeader, validateJsonResponse,
  validateODataAnnotation, validateResponseContainsPayload, validateODataError,
  formatConsoleReport, formatJsonReport,
  extractPrimaryKey, stripPrimaryKey, buildScenarioResult
} from '@reso/certification-test-runner';
```

## Modules

### Validators (`validators.ts`)

OData 4.01 compliance assertion functions. Each returns `TestAssertion` objects with `pass`, `fail`, `skip`, or `warn` status.

**Header validators:**
- `validateODataVersionHeader(response)` — checks for `4.0` or `4.01`
- `validateStatusCode(response, allowed[])` — status code in allowed list
- `validateEntityIdHeader(response)` — EntityId header on 204 responses
- `validateLocationHeader(response, resource)` — Location URL format
- `validatePreferenceApplied(response, preference)` — Preference-Applied header

**Body validators:**
- `validateJsonResponse(response)` — valid JSON object
- `validateEmptyResponse(response)` — empty body (DELETE success)
- `validateODataAnnotation(response, name, 'MUST'|'MAY')` — `@odata.context`, `@odata.id`, etc.
- `validateResponseContainsPayload(body, payload)` — every payload field present in response

**Error validators:**
- `validateODataError(response, entityType)` — error.code, error.message, error.details[].target validated against metadata field names

### Client (`client.ts`)

- `odataRequest(options)` — sends HTTP request with OData headers; delegates to `@reso/odata-client`
- `buildResourceUrl(serverUrl, resource, key?)` — builds OData URLs with optional key syntax

### Auth (`auth.ts`)

- `resolveAuthToken(auth)` — resolves `AuthConfig` to a bearer token (supports `token` and `client_credentials` modes)

### Metadata (`metadata.ts`)

- `fetchMetadata(serverUrl, authToken)` — fetches `/$metadata` EDMX XML
- `loadMetadataFromFile(filePath)` — reads metadata from local file
- `parseMetadataXml(xml)` — parses EDMX into `ParsedMetadata`
- `getEntityType(metadata, resourceName)` — finds entity type by name
- `validatePayloadAgainstMetadata(payload, entityType)` — validates fields and types using `@reso/validation`
- `toResoFields(entityType)` — converts entity properties to `ResoField[]`

### Reporter (`reporter.ts`)

- `formatConsoleReport(report)` — human-readable output with pass/fail/skip/warn indicators
- `formatJsonReport(report)` — pretty-printed JSON

### Helpers (`helpers.ts`)

- `extractPrimaryKey(payload, entityType)` — gets primary key value from payload
- `stripPrimaryKey(payload, entityType)` — returns payload without the key field
- `makeSchemaAssertion(payload, entityType)` — validates field names exist in metadata
- `buildScenarioResult(scenario, tags, assertions, startTime)` — constructs result with duration

## Key Types

```typescript
type AuthConfig =
  | { mode: 'token'; authToken: string }
  | { mode: 'client_credentials'; clientId: string; clientSecret: string; tokenUrl: string };

interface TestConfig {
  readonly serverUrl: string;
  readonly resource: string;
  readonly payloadsDir: string;
  readonly auth: AuthConfig;
}

type TestStatus = 'pass' | 'fail' | 'skip' | 'warn';

interface TestAssertion {
  readonly description: string;
  readonly status: TestStatus;
  readonly expected?: string;
  readonly actual?: string;
}

interface ScenarioResult {
  readonly scenario: string;
  readonly assertions: ReadonlyArray<TestAssertion>;
  readonly passed: boolean;
  readonly duration: number;
}

interface TestReport {
  readonly serverUrl: string;
  readonly resource: string;
  readonly scenarios: ReadonlyArray<ScenarioResult>;
  readonly summary: { total: number; passed: number; failed: number; skipped: number };
}
```

## Dependencies

- `@reso/odata-client` — OData HTTP client and CSDL parser
- `@reso/validation` — Field-level and business rule validation
