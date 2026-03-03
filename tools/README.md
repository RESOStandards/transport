# RESO Transport Tools

Testing, client SDK, and reference implementation tools for the RESO Web API specification.

## Packages

### Libraries

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

#### [web-api-add-edit-test](web-api-add-edit-test/)

RESO Web API Add/Edit Endorsement (RCP-010) compliance testing tool. Sends known-good and known-bad JSON payloads to OData servers and validates responses against 8 Gherkin BDD certification scenarios.

```bash
cd web-api-add-edit-test && npm install && npm test  # 49 tests

# Run against a server
npx testWebApiAddEdit \
  --url https://api.example.com \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token <token>

# Run against the built-in mock server
npx testWebApiAddEdit \
  --url http://localhost:8800 \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token test \
  --mock
```

#### [reso-reference-server](reso-reference-server/)

Metadata-driven OData 4.01 reference server backed by PostgreSQL. Reads the RESO Data Dictionary JSON metadata and dynamically generates database tables, OData CRUD endpoints, EDMX metadata, and OpenAPI documentation for Property, Member, Office, Media, OpenHouse, and Showing resources.

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

1. `odata-filter-parser` — no dependencies
2. `odata-client` — depends on `odata-filter-parser`
3. `reso-reference-server/server` — depends on `odata-filter-parser`
4. `web-api-add-edit-test` — depends on `odata-client`

## Cross-Tool Validation

Run the compliance tests against the reference server:

```bash
# Start the reference server
cd reso-reference-server && docker-compose up -d

# Run compliance tests
cd ../web-api-add-edit-test
npx testWebApiAddEdit \
  --url http://localhost:8080 \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token test
```
