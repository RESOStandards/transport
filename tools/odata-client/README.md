# @reso/odata-client

OData 4.01 client SDK for TypeScript. Provides URI building, CRUD helpers, CSDL metadata parsing and validation, query option validation, and response parsing. Inspired by [Apache Olingo's Java Client SDK](https://olingo.apache.org/doc/odata4/index.html).

## Install

```bash
npm install @reso/odata-client
```

## Quick Start

```typescript
import {
  createClient,
  createEntity,
  readEntity,
  queryEntities,
  buildUri,
  isODataCollection,
} from "@reso/odata-client";

// Create a client with bearer token auth
const client = await createClient({
  baseUrl: "http://localhost:8080",
  auth: { mode: "token", authToken: "test" },
});

// Create a record
const created = await createEntity(client, "Property", {
  ListPrice: 250000,
  City: "Austin",
  BedroomsTotal: 3,
}, { prefer: "representation" });

// Read a record by key
const record = await readEntity(client, "Property", "ABC123");

// Query with $filter, $select, $orderby, $top
const response = await queryEntities(client, "Property", {
  $filter: "ListPrice gt 200000 and City eq 'Austin'",
  $select: "ListPrice,City,BedroomsTotal",
  $orderby: "ListPrice desc",
  $top: 10,
});

if (response.status === 200 && isODataCollection(response.body)) {
  console.log(`Found ${response.body.value.length} properties`);
}
```

## Features

### URI Builder

Chainable functional API for constructing OData URLs with system query options.

```typescript
import { buildUri } from "@reso/odata-client";

const url = buildUri("http://localhost:8080", "Property")
  .key("ABC123")
  .select("ListPrice", "City")
  .filter("ListPrice gt 200000")
  .expand("Media")
  .orderby("ListPrice desc")
  .top(10)
  .skip(20)
  .count()
  .build();

// Compound keys
const url2 = buildUri("http://localhost:8080", "OrderItem")
  .compoundKey({ OrderId: "O1", ItemId: "I1" })
  .build();
```

Supported query options: `$select`, `$filter`, `$orderby`, `$top`, `$skip`, `$count`, `$expand`, `$search`, `$compute`, `$format`.

### CRUD Helpers

High-level functions that compose the URI builder and HTTP client.

```typescript
import {
  createEntity,
  readEntity,
  updateEntity,
  replaceEntity,
  deleteEntity,
  queryEntities,
} from "@reso/odata-client";

// POST /{Resource}
await createEntity(client, "Property", body, { prefer: "representation" });

// GET /{Resource}('{key}')
await readEntity(client, "Property", "ABC123");

// PATCH /{Resource}('{key}')
await updateEntity(client, "Property", "ABC123", patch, {
  prefer: "representation",
  ifMatch: '"etag-value"',
});

// PUT /{Resource}('{key}')
await replaceEntity(client, "Property", "ABC123", fullBody);

// DELETE /{Resource}('{key}')
await deleteEntity(client, "Property", "ABC123");

// GET /{Resource}?$filter=...&$top=...
await queryEntities(client, "Property", {
  $filter: "City eq 'Austin'",
  $top: 25,
});
```

### Authentication

Bearer token or OAuth2 Client Credentials.

```typescript
// Bearer token
const client = await createClient({
  baseUrl: "http://localhost:8080",
  auth: { mode: "token", authToken: "my-token" },
});

// OAuth2 Client Credentials
const client = await createClient({
  baseUrl: "http://localhost:8080",
  auth: {
    mode: "client_credentials",
    clientId: "my-client",
    clientSecret: "my-secret",
    tokenUrl: "https://auth.example.com/oauth/token",
  },
});
```

### CSDL Metadata Parsing

Parse and validate EDMX/CSDL XML metadata documents.

```typescript
import {
  fetchAndParseMetadata,
  parseCsdlXml,
  validateCsdl,
  getEntityType,
} from "@reso/odata-client";

// Fetch and parse from a server
const schema = await fetchAndParseMetadata("http://localhost:8080", "my-token");

// Or parse raw XML
const schema = parseCsdlXml(xmlString);

// Look up entity types
const propertyType = getEntityType(schema, "Property");

// Validate the schema
const result = validateCsdl(schema);
if (!result.valid) {
  console.log("Validation errors:", result.errors);
}
```

Parsed types include: `CsdlEntityType`, `CsdlProperty`, `CsdlNavigationProperty`, `CsdlComplexType`, `CsdlEnumType`, `CsdlEntityContainer`, `CsdlEntitySet`, `CsdlSingleton`, `CsdlAction`, `CsdlFunction`, and more.

### Query Validation

Validate query options against CSDL metadata to catch errors before sending requests.

```typescript
import { validateQueryOptions } from "@reso/odata-client";

const result = validateQueryOptions(
  {
    $select: "ListPrice,City,NonExistentField",
    $filter: "ListPrice gt 200000",
    $orderby: "ListPrice desc",
    $top: 10,
  },
  entityType,
);

if (!result.valid) {
  console.log("Invalid query options:", result.errors);
}
```

Validates `$select` and `$orderby` field references against the entity type, `$filter` property references by walking the parsed AST (using `@reso/odata-filter-parser`), and `$top`/`$skip` value constraints.

### Response Parsing

Extract annotations, detect errors, and handle paging.

```typescript
import {
  extractAnnotations,
  extractEntityData,
  isODataError,
  parseODataError,
  isODataCollection,
  getNextLink,
  followAllPages,
} from "@reso/odata-client";

// Extract @odata annotations
const annotations = extractAnnotations(entity);
// → { context, id, editLink, etag }

// Strip annotations to get raw data
const data = extractEntityData(entity);

// Check for OData errors
if (isODataError(response.body)) {
  const error = parseODataError(response);
  console.log(error.error.message);
}

// Auto-page through all results
const allEntities = await followAllPages(client, firstResponse);
```

### Filter Parser (re-exported)

The `parseFilter` function and AST types from [`@reso/odata-filter-parser`](../odata-filter-parser/) are re-exported for convenience.

```typescript
import { parseFilter } from "@reso/odata-client";

const ast = parseFilter("ListPrice gt 200000 and City eq 'Austin'");
```

## Examples

Runnable examples are in the [examples/](examples/) directory. Each requires the reference server running at `http://localhost:8080`.

| Example | Description |
|---------|-------------|
| [fetch-property.ts](examples/fetch-property.ts) | Read a Property by key |
| [query-with-filter.ts](examples/query-with-filter.ts) | Query with `$filter`, `$select`, `$orderby`, `$top` |
| [create-and-update.ts](examples/create-and-update.ts) | Create a Property, update it, read it back |
| [validate-metadata.ts](examples/validate-metadata.ts) | Fetch and validate CSDL metadata |
| [oauth-flow.ts](examples/oauth-flow.ts) | OAuth2 Client Credentials flow |

```bash
# Start the reference server first
cd ../reso-reference-server && docker-compose up -d

# Run an example
npx tsx examples/query-with-filter.ts
```

## Development

```bash
npm install
npm run build
npm test        # 101 tests
```

## License

See [LICENSE](../../License.txt) in the repository root.
