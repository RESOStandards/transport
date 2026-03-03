# src/lib/ — SDK Library

Core library modules for the RESO Web API Add/Edit compliance testing tool. All modules export pure functions (no classes) and use `Readonly<T>` types throughout.

## Modules

### types.ts
All TypeScript interfaces and type definitions shared across the project. Includes `TestConfig`, `PayloadSet`, `EntityType`, `ParsedMetadata`, `TestAssertion`, `ScenarioResult`, `TestReport`, `ODataResponse`, and the `ScenarioName` union type covering all 8 certification scenarios.

### metadata.ts
OData XML metadata handling. Fetches metadata from a server's `/$metadata` endpoint or loads it from a local file, then parses the EDMX XML into structured `EntityType` and `EntityProperty` definitions using `fast-xml-parser`. Also provides `validatePayloadAgainstMetadata()` to check that payload fields exist in the entity type schema.

### client.ts
Thin HTTP client wrapper around `fetch` for OData requests. Sets standard OData headers (OData-Version, Content-Type, Accept, Authorization) and normalizes response headers to lowercase keys. Also provides `buildResourceUrl()` for constructing OData URLs with optional key syntax like `Property('12345')`.

### validators.ts
Pure validation functions that map 1:1 to the Gherkin `Then` steps in the RCP-010 certification specification. Each function takes an `ODataResponse` and returns one or more `TestAssertion` results. Organized into three groups:
- **Header validators** — OData-Version, status code, EntityId, Location, Preference-Applied
- **Body validators** — JSON format, empty response, OData annotations (@odata.context, @odata.id, @odata.editLink, @odata.etag), payload field matching
- **Error validators** — OData error format (error.code, error.message, error.details with target/message)

### test-runner.ts
Scenario orchestrator and public API entry point. `runAllScenarios(config)` loads metadata, validates the target resource, loads all 6 payload files, and runs the 8 certification scenarios sequentially:
1. Create succeeds (return=representation)
2. Create succeeds (return=minimal)
3. Create fails
4. Update succeeds (return=representation)
5. Update succeeds (return=minimal)
6. Update fails
7. Delete succeeds
8. Delete fails

Each scenario sends HTTP requests using the client, validates responses using the validators, and returns a structured `ScenarioResult`. Success scenarios include follow-up GET requests to verify persistence.

### reporter.ts
Output formatting for test results. `formatConsoleReport()` produces a human-readable report with checkmark/cross icons, scenario durations, and a summary footer. `formatJsonReport()` outputs the full `TestReport` as pretty-printed JSON.
