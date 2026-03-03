# RESO Transport Tools

Testing, client SDK, and reference implementation tools for the RESO Web API specification.

## Packages

### Libraries

#### [@reso/validation](validation/)

Isomorphic validation library for RESO metadata-driven field validation. Works in any JS runtime (Node.js, browser, edge). Used by both the reference server API and the React UI.

```bash
cd validation && npm install && npm test  # 41 tests
```

#### [@reso/odata-filter-parser](odata-filter-parser/)

Standalone, zero-dependency library for parsing OData 4.01 `$filter` expressions into a typed AST. Used by both the client SDK (query validation) and the reference server (SQL translation).

```bash
cd odata-filter-parser && npm install && npm test  # 97 tests
```

#### [@reso/odata-client](odata-client/)

OData 4.01 client SDK for TypeScript. URI builder, CRUD helpers, CSDL metadata parsing/validation, query validation, and response parsing. Inspired by Apache Olingo.

```bash
cd odata-client && npm install && npm test  # 101 tests
```

### Tools

#### [certification/](certification/)

RESO certification testing tools. Each subdirectory implements an independent certification module.

##### [certification/add-edit/](certification/add-edit/) — Web API Add/Edit (RCP-010)

Compliance testing tool for the RESO Web API Add/Edit Endorsement. Validates OData CRUD operations against 8 Gherkin BDD certification scenarios.

```bash
cd certification/add-edit && npm install && npm test  # 49 tests

# Run against a server
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

#### [reso-reference-server](reso-reference-server/)

Metadata-driven OData 4.01 reference server backed by PostgreSQL. Reads the RESO Data Dictionary JSON metadata and dynamically generates database tables, OData CRUD endpoints, EDMX metadata, and OpenAPI documentation for Property, Member, Office, Media, OpenHouse, and Showing resources. Includes a React UI for browsing and editing records.

**Build and run with Docker:**

```bash
cd reso-reference-server
docker-compose up -d

# Verify
curl http://localhost:8080/health
curl http://localhost:8080/\$metadata
open http://localhost:8080/api-docs
```

**Build and run locally (requires PostgreSQL):**

```bash
cd reso-reference-server/server
npm install
npm run build
npm start
npm test  # 67 tests
```

## Build Order

Packages have `file:` dependencies. Build in this order:

1. `validation` — no dependencies
2. `odata-filter-parser` — no dependencies
3. `odata-client` — depends on `odata-filter-parser`
4. `reso-reference-server/server` — depends on `validation` + `odata-filter-parser`
5. `certification/test-runner` — depends on `odata-client` + `validation`
6. `certification/add-edit` — depends on `certification/test-runner` + `odata-client` + `validation`

## Development

### Linting and Formatting

The codebase uses [Biome](https://biomejs.dev/) for linting and formatting, configured to match the [RESO certification-utils](https://github.com/RESOStandards/reso-certification-utils) style (single quotes, semicolons, no trailing commas, 140 char line width).

```bash
# From the repo root
npm run lint          # Check for lint/format issues
npm run lint:fix      # Auto-fix issues
```

### Pre-commit Hooks

[Lefthook](https://github.com/evilmartians/lefthook) runs pre-commit hooks automatically on every `git commit`:

1. **Lint + auto-fix** — Biome checks and fixes staged `.ts`/`.tsx` files, re-stages fixes
2. **Type check** — `tsc --noEmit` in all packages (respecting build order)
3. **Tests** — `vitest run` in all packages

To set up after cloning:

```bash
npm install           # installs biome + lefthook
npx lefthook install  # activates git hooks
```

## Cross-Tool Validation

Run the compliance tests against the reference server:

```bash
# Start the reference server
cd reso-reference-server && docker-compose up -d

# Run compliance tests
cd ../certification/add-edit
npx reso-cert-add-edit \
  --url http://localhost:8080 \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token test
```
