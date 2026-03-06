# RESO Reference Server — User Guide

This guide covers how to set up, use, and test the RESO reference server, web UI,
and compliance testing tools.

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Server API](#server-api)
   - [Service Document](#service-document)
   - [Metadata](#metadata)
   - [Querying Resources](#querying-resources)
   - [Filtering](#filtering)
   - [Sorting and Pagination](#sorting-and-pagination)
   - [Field Selection](#field-selection)
   - [Expanding Related Records](#expanding-related-records)
   - [Lookup Resource](#lookup-resource)
   - [EntityEvent Resource](#entityevent-resource)
   - [Creating Records](#creating-records)
   - [Updating Records](#updating-records)
   - [Deleting Records](#deleting-records)
3. [Web UI](#web-ui)
   - [Browsing Resources](#browsing-resources)
   - [Searching and Filtering](#searching-and-filtering)
   - [Record Detail View](#record-detail-view)
   - [Creating and Editing Records](#creating-and-editing-records)
   - [Related Records and Navigation](#related-records-and-navigation)
   - [Data Generator](#data-generator)
   - [Dark Mode](#dark-mode)
4. [Enumeration Modes](#enumeration-modes)
5. [Database Backends](#database-backends)
6. [Compliance Testing](#compliance-testing)
   - [Web API Core 2.0.0](#web-api-core-200)
   - [Data Dictionary 2.0](#data-dictionary-20)
   - [Add/Edit (RCP-010)](#addedit-rcp-010)
7. [Data Generator CLI](#data-generator-cli)

---

## Quick Start

### Prerequisites

- Docker and Docker Compose

### Start the Server

```bash
cd tools/reso-reference-server

# Start server (port 8080), UI (port 5173), and PostgreSQL (port 5432)
docker compose up -d --build

# Seed test data (1,250 properties, 625 members, 250 offices, and more)
docker compose --profile seed up seed
```

The server is now running at **http://localhost:8080** and the UI at **http://localhost:5173**.

### Authentication

All API requests require a Bearer token. The reference server accepts any token
value for development purposes:

```bash
curl -H 'Authorization: Bearer test-token' http://localhost:8080/Property
```

---

## Server API

The server implements **OData 4.01** per the **RESO Web API Core 2.0.0**
specification. All examples below use `curl` with the authorization header
abbreviated as `-H 'Authorization: Bearer test-token'`.

### Service Document

The OData service document lists all available resources:

```bash
curl http://localhost:8080/ \
  -H 'Authorization: Bearer test-token'
```

**Response:**

```json
{
  "@odata.context": "http://localhost:8080/$metadata",
  "value": [
    { "name": "Property", "kind": "EntitySet", "url": "Property" },
    { "name": "Member", "kind": "EntitySet", "url": "Member" },
    { "name": "Office", "kind": "EntitySet", "url": "Office" },
    { "name": "Media", "kind": "EntitySet", "url": "Media" },
    { "name": "OpenHouse", "kind": "EntitySet", "url": "OpenHouse" },
    { "name": "Showing", "kind": "EntitySet", "url": "Showing" },
    { "name": "Teams", "kind": "EntitySet", "url": "Teams" },
    { "name": "TeamMembers", "kind": "EntitySet", "url": "TeamMembers" },
    { "name": "OUID", "kind": "EntitySet", "url": "OUID" },
    { "name": "Lookup", "kind": "EntitySet", "url": "Lookup" }
  ]
}
```

### Metadata

The EDMX metadata document describes the full schema — entity types, fields,
data types, annotations, and navigation properties:

```bash
curl http://localhost:8080/\$metadata \
  -H 'Authorization: Bearer test-token'
```

This returns an XML document conforming to the OData CSDL specification. It
includes an `EntityContainer` with `EntitySet` and `NavigationPropertyBinding`
elements, and `Property` elements with correct `Edm.*` types for all 1,727 DD
2.0 fields.

### Querying Resources

Retrieve a collection of records:

```bash
# Get properties (default page size: 100)
curl 'http://localhost:8080/Property' \
  -H 'Authorization: Bearer test-token'

# Get a single property by key
curl "http://localhost:8080/Property('702851eb-ac15-40f4-8c00-3256edf538e5')" \
  -H 'Authorization: Bearer test-token'
```

### Filtering

Use `$filter` to query records with OData filter expressions:

```bash
# Properties in California
curl 'http://localhost:8080/Property?$filter=StateOrProvince%20eq%20%27CA%27' \
  -H 'Authorization: Bearer test-token'

# Properties over $500,000
curl 'http://localhost:8080/Property?$filter=ListPrice%20gt%20500000' \
  -H 'Authorization: Bearer test-token'

# Combine filters with 'and' / 'or'
curl 'http://localhost:8080/Property?$filter=ListPrice%20gt%20500000%20and%20StandardStatus%20eq%20%27Active%27' \
  -H 'Authorization: Bearer test-token'

# String contains
curl "http://localhost:8080/Property?\$filter=contains(City,'Clinton')" \
  -H 'Authorization: Bearer test-token'

# Date comparison
curl "http://localhost:8080/Property?\$filter=ModificationTimestamp%20gt%202026-01-01T00:00:00Z" \
  -H 'Authorization: Bearer test-token'

# Lambda expressions on collection fields (e.g., multi-value lookups)
# any() — at least one element matches
curl "http://localhost:8080/Property?\$filter=AccessibilityFeatures/any(x:x%20eq%20'Accessible%20Approach%20with%20Ramp')" \
  -H 'Authorization: Bearer test-token'

# all() — every element matches
curl "http://localhost:8080/Property?\$filter=AccessibilityFeatures/all(x:x%20eq%20'Accessible%20Approach%20with%20Ramp')" \
  -H 'Authorization: Bearer test-token'
```

**Supported operators:** `eq`, `ne`, `gt`, `ge`, `lt`, `le`, `and`, `or`, `not`

**Supported functions:** `contains()`, `startswith()`, `endswith()`, `tolower()`,
`toupper()`, `trim()`, `length()`, `year()`, `month()`, `day()`, `hour()`,
`minute()`, `second()`, `now()`, `date()`, `time()`, `matchesPattern()`

### Sorting and Pagination

```bash
# Sort by list price descending
curl 'http://localhost:8080/Property?$orderby=ListPrice%20desc' \
  -H 'Authorization: Bearer test-token'

# Sort by multiple fields
curl 'http://localhost:8080/Property?$orderby=StateOrProvince%20asc,ListPrice%20desc' \
  -H 'Authorization: Bearer test-token'

# Pagination
curl 'http://localhost:8080/Property?$top=10&$skip=20' \
  -H 'Authorization: Bearer test-token'

# Include total count
curl 'http://localhost:8080/Property?$top=10&$count=true' \
  -H 'Authorization: Bearer test-token'
```

When more records are available, the response includes an `@odata.nextLink`
URL for server-driven pagination:

```json
{
  "@odata.context": "http://localhost:8080/$metadata#Property",
  "@odata.count": 1250,
  "value": [ ... ],
  "@odata.nextLink": "http://localhost:8080/Property?$top=10&$skip=10"
}
```

### Field Selection

Use `$select` to return only specific fields:

```bash
curl 'http://localhost:8080/Property?$select=ListingKey,ListPrice,City,StandardStatus&$top=3' \
  -H 'Authorization: Bearer test-token'
```

**Response:**

```json
{
  "@odata.context": "http://localhost:8080/$metadata#Property",
  "value": [
    {
      "City": "Clinton",
      "ListingKey": "702851eb-ac15-40f4-8c00-3256edf538e5",
      "ListPrice": 8177125.42,
      "StandardStatus": "Active"
    }
  ]
}
```

### Expanding Related Records

Use `$expand` to include related entities inline. The server supports three
FK resolution strategies (resource-record-key, direct, and parent-fk), all
transparent to the consumer.

```bash
# Expand the listing agent and listing office on a property
curl "http://localhost:8080/Property('702851eb-ac15-40f4-8c00-3256edf538e5')?\$expand=ListAgent,ListOffice&\$select=ListingKey,ListPrice,City" \
  -H 'Authorization: Bearer test-token'
```

**Response (abbreviated):**

```json
{
  "ListingKey": "702851eb-ac15-40f4-8c00-3256edf538e5",
  "ListPrice": 8177125.42,
  "City": "Clinton",
  "ListAgent": {
    "MemberKey": "c8fa2607-7b0f-4919-bfe3-1431295586b1",
    "MemberFirstName": "William",
    "MemberLastName": "Thomas",
    "MemberEmail": "william.thomas@homes.example.com"
  },
  "ListOffice": {
    "OfficeKey": "ddb4bc01-5f71-4e0e-b4c9-0ed9b2be797b",
    "OfficeName": "Heritage Brokerage",
    "OfficePhone": "401-363-8945"
  }
}
```

**Common expand targets:**

| Resource | Navigation Property | Target |
|----------|-------------------|--------|
| Property | ListAgent, BuyerAgent, CoListAgent, CoBuyerAgent | Member |
| Property | ListOffice, BuyerOffice, CoListOffice, CoBuyerOffice | Office |
| Property | ListTeam, BuyerTeam | Teams |
| Property | Media, OpenHouse, Rooms, PropertyRooms | (child collections) |
| Member | Office | Office |
| Office | OfficeBroker, OfficeManager | Member |
| Teams | TeamLead | Member |

```bash
# Expand a member's office
curl "http://localhost:8080/Member('b5f2791f-6f3d-4828-9e7e-5b6d963e7f9d')?\$expand=Office" \
  -H 'Authorization: Bearer test-token'

# Expand media on a property (to-many collection)
curl "http://localhost:8080/Property('702851eb-ac15-40f4-8c00-3256edf538e5')?\$expand=Media" \
  -H 'Authorization: Bearer test-token'
```

### Lookup Resource

The Lookup Resource catalogs all valid enumeration values (available in string
enum mode, which is the default):

```bash
# Get all lookups for the StandardStatus field
curl "http://localhost:8080/Lookup?\$filter=LookupName%20eq%20'StandardStatus'" \
  -H 'Authorization: Bearer test-token'
```

**Response:**

```json
{
  "@odata.context": "http://localhost:8080/$metadata#Lookup",
  "value": [
    {
      "LookupKey": "7df85478...",
      "LookupName": "StandardStatus",
      "LookupValue": "Active",
      "StandardLookupValue": "Active",
      "LegacyODataValue": "Active",
      "ModificationTimestamp": "2024-10-15T04:26:10.660Z"
    },
    {
      "LookupKey": "99686982...",
      "LookupName": "StandardStatus",
      "LookupValue": "Active Under Contract",
      "StandardLookupValue": "Active Under Contract",
      "LegacyODataValue": "ActiveUnderContract",
      "ModificationTimestamp": "2024-10-15T04:26:10.660Z"
    }
  ]
}
```

The Lookup Resource supports standard OData query options (`$filter`, `$select`,
`$orderby`, `$top`, `$skip`, `$count`) but is read-only — POST, PATCH, and
DELETE are not available.

### EntityEvent Resource

When EntityEvent tracking is enabled (`ENTITY_EVENT=true`), every Create, Update,
and Delete operation on the server automatically writes an EntityEvent record with
a monotonically increasing sequence number. This allows consumers to detect what
has changed since their last sync.

```bash
# Get the 10 most recent entity events
curl 'http://localhost:8080/EntityEvent?$orderby=EntityEventSequence%20desc&$top=10' \
  -H 'Authorization: Bearer test-token'
```

**Response:**

```json
{
  "@odata.context": "http://localhost:8080/$metadata#EntityEvent",
  "value": [
    {
      "EntityEventSequence": 4521,
      "ResourceName": "Property",
      "ResourceRecordKey": "702851eb-ac15-40f4-8c00-3256edf538e5"
    },
    {
      "EntityEventSequence": 4520,
      "ResourceName": "Member",
      "ResourceRecordKey": "c8fa2607-7b0f-4919-bfe3-1431295586b1"
    }
  ]
}
```

```bash
# Get all events since a known sequence number (incremental sync)
curl 'http://localhost:8080/EntityEvent?$filter=EntityEventSequence%20gt%204500&$orderby=EntityEventSequence%20asc' \
  -H 'Authorization: Bearer test-token'

# Filter events for a specific resource
curl 'http://localhost:8080/EntityEvent?$filter=ResourceName%20eq%20%27Property%27&$orderby=EntityEventSequence%20desc&$top=5' \
  -H 'Authorization: Bearer test-token'
```

The EntityEvent resource is read-only — POST, PATCH, and DELETE are not available.

**Configuration:**

| Variable | Default | Description |
|----------|---------|-------------|
| `ENTITY_EVENT` | `false` | Enable EntityEvent tracking |
| `ENTITY_EVENT_RESOURCE_RECORD_URL` | `false` | Include optional ResourceRecordUrl field |
| `COMPACTION_INTERVAL_MS` | `3600000` | Compaction interval in ms (0 = disabled) |

### Creating Records

```bash
curl -X POST http://localhost:8080/Property \
  -H 'Authorization: Bearer test-token' \
  -H 'Content-Type: application/json' \
  -d '{
    "ListPrice": 450000,
    "StandardStatus": "Active",
    "City": "Denver",
    "StateOrProvince": "CO",
    "PostalCode": "80202",
    "Country": "US"
  }'
```

The server validates the request body against DD 2.0 field rules and business
rules before persisting. Invalid requests return a 400 response with error
details.

### Updating Records

PATCH performs a partial update — only the fields included in the request body
are modified. Required-field validation is skipped for PATCH requests, so you
can update a single field without providing the full required set.

```bash
curl -X PATCH "http://localhost:8080/Property('702851eb-ac15-40f4-8c00-3256edf538e5')" \
  -H 'Authorization: Bearer test-token' \
  -H 'Content-Type: application/json' \
  -d '{ "ListPrice": 475000 }'
```

### Deleting Records

```bash
curl -X DELETE "http://localhost:8080/Property('702851eb-ac15-40f4-8c00-3256edf538e5')" \
  -H 'Authorization: Bearer test-token'
```

---

## Web UI

The web UI is accessible at **http://localhost:5173** and provides a visual
interface for browsing, searching, creating, and editing records across all RESO
resources.

### Browsing Resources

The sidebar lists all available resources (Property, Member, Office, Media,
OpenHouse, etc.). Selecting a resource displays a paginated card list of records.
Each card shows a summary of key fields for that resource type — for example,
Property cards display the listing photo, price, address, status, and key
listing details.

<!-- Screenshot: Resource list page showing Property cards -->

### Searching and Filtering

The search toolbar is pinned at the top of the page and includes:

- **Search bar** — Enter OData `$filter` expressions directly (e.g.,
  `ListPrice gt 500000 and City eq 'Denver'`). Invalid filters show an inline
  error message with the parse error details. Long filters can be copied to the
  clipboard with the copy button.

- **Advanced search panel** — A form-based filter builder with type-aware
  operators. Each row lets you pick a field, an operator appropriate for the
  field's data type (e.g., `eq`, `ne`, `gt`, `lt` for numbers; `contains`,
  `startswith` for strings), and a value. Collection fields show pills for
  selected values. Changes in the advanced search form automatically update the
  search bar, and vice versa — the two stay in sync.

- **Sort controls** — Sort by any field in ascending or descending order.

- **Result count** — Displayed in the pinned toolbar area, showing the total
  number of matching records.

The toolbar and header remain pinned as you scroll through results.

<!-- Screenshot: Search page with advanced search panel open -->

### Record Detail View

Clicking a record card opens the detail page, which includes:

- **Pinned header** — The record title (e.g., property address or member name),
  and Edit and Delete action buttons. This header stays visible as you scroll.

- **Summary pane** — Key fields displayed prominently at the top. For
  properties, this includes price, status, address, bedrooms, bathrooms, and
  square footage.

- **Media carousel** — For resources with media (photos, virtual tours), a
  side-by-side carousel with thumbnail navigation is displayed.

- **Field groups** — All fields organized into collapsible sections by RESO
  field group (e.g., "Listing", "Location", "Financial", "Interior Features").
  Each field shows its label, value, and data type.

- **Related Records** — Expanded navigation properties displayed as inset cards
  in a collapsible "Related Records" section. To-one expansions (e.g.,
  ListAgent, ListOffice) show a single card with a "View" link to navigate to
  the related entity. To-many expansions (e.g., Media, OpenHouse) show cards
  with back/next pagination for browsing individual records.

Key fields (like ListingKey, MemberKey) have copy-to-clipboard functionality
and are styled for easy identification.

<!-- Screenshot: Detail page showing summary, media carousel, and related records -->

### Creating and Editing Records

The Add and Edit pages present a form with all fields for the resource, organized
into the same collapsible field groups as the detail page.

- **Real-time validation** — Required fields, data type checks, and business
  rules are validated as you type. Errors appear inline next to each field.

- **Cross-field validation** — Business rules that span multiple fields are
  enforced (e.g., ListPrice must be greater than or equal to ListPriceLow;
  bathroom counts must sum correctly).

- **Auto-expanding error groups** — If a collapsed field group contains
  validation errors, it automatically expands with a red badge showing the
  error count.

- **Read-only resources** — Resources like Lookup that are read-only do not show
  Add, Edit, or Delete buttons.

<!-- Screenshot: Edit form showing validation errors and auto-expanded group -->

### Related Records and Navigation

From any detail page, you can navigate to related records by clicking the "View"
link on expansion cards. For example:

- From a Property → click "View" on the ListAgent card → opens the Member detail
  page
- From a Member → click "View" on the Office card → opens the Office detail page

This lets you traverse the relationship graph visually.

### Data Generator

The Admin section (accessible via the sidebar) includes a data generator
interface for seeding test data:

- Select a target resource and record count
- The generator automatically resolves FK dependencies — requesting Property
  records will first create the required Office, Member, and Teams records
- Progress is displayed during generation
- The `resolveDependencies` option ensures all FK linkages are valid so
  `$expand` works correctly

<!-- Screenshot: Admin data generator page -->

### Dark Mode

The UI supports dark mode, toggled via the theme switch in the header. The
preference persists in `localStorage` across sessions. You can also set the
initial theme via the URL query parameter `?theme=dark` or `?theme=light`.

---

## Enumeration Modes

The server supports two ways of representing enumeration fields, controlled by
the `ENUM_MODE` environment variable.

### String Mode (Default)

```bash
ENUM_MODE=string  # or simply omit — this is the default
```

- Enum fields use `Edm.String` types in EDMX metadata
- Fields have `RESO.OData.Metadata.LookupName` annotations
- Values are human-readable display names: `"Active Under Contract"`
- The **Lookup Resource** is exposed at `/Lookup` with all 3,634 valid values
- Collection enums are `Collection(Edm.String)`

### EnumType Mode

```bash
ENUM_MODE=enum-type
```

- Enum fields reference `Edm.EnumType` definitions (e.g.,
  `org.reso.metadata.enums.StandardStatus`)
- A second EDMX Schema block defines all `<EnumType>` elements with PascalCase
  member names and sequential integer values
- Values are PascalCase identifiers: `"ActiveUnderContract"`
- No `LookupName` annotations
- No Lookup Resource
- Collection enums use `Collection(org.reso.metadata.enums.X)` with
  `Nullable="false"`

### Switching Modes

```bash
# Start in enum-type mode
ENUM_MODE=enum-type docker compose up -d --build

# Reseed (data generator automatically uses PascalCase values)
docker compose --profile seed up seed
```

Both modes pass all 42 Web API Core 2.0.0 compliance tests.

---

## Database Backends

Three backends are available, selectable via Docker Compose profiles:

### PostgreSQL (Default)

```bash
docker compose up -d --build
docker compose --profile seed up seed

# With EntityEvent tracking
ENTITY_EVENT=true docker compose up -d --build
```

### MongoDB

```bash
docker compose --profile mongodb up -d mongodb server-mongo ui-mongo
docker compose --profile seed-mongo up seed-mongo

# With EntityEvent tracking
ENTITY_EVENT=true docker compose --profile mongodb up -d mongodb server-mongo ui-mongo
```

Server runs on port 8080 (or 8081 if both profiles are active).

### SQLite

```bash
docker compose --profile sqlite up -d
docker compose --profile sqlite --profile seed-sqlite up seed-sqlite

# With EntityEvent tracking
ENTITY_EVENT=true docker compose --profile sqlite up -d
```

Lightweight option with no external database process — ideal for local
development and testing. The database is stored in a single file.

All three backends support the same OData query capabilities and pass Web API
Core 2.0.0 compliance (42/42).

---

## Compliance Testing

The project includes Docker-based compliance testing using the official RESO
certification tools.

### Web API Core 2.0.0

Uses the RESO Web API Commander to run the 45 Core specification tests:

```bash
# PostgreSQL (default)
docker compose --profile compliance-core up compliance-core

# MongoDB
docker compose --profile compliance-core-mongo up compliance-core-mongo

# SQLite
docker compose --profile compliance-core-sqlite up compliance-core-sqlite

# Enum-type mode
ENUM_MODE=enum-type docker compose --profile compliance-core up compliance-core
```

**Results:** 42 passed, 0 failed, 3 skipped (skipped tests are `has` operator
tests, not applicable with string enumerations).

### Data Dictionary 2.0

Uses reso-certification-utils to validate field names, types, and lookup values
against the DD 2.0 specification:

```bash
docker compose --profile compliance-dd up compliance-dd
```

**Results:** 1,034 passed, 570 skipped, 0 failed, 0 schema validation errors,
0 variations.

### Add/Edit (RCP-010)

Uses the custom `@reso/certification-add-edit` test runner to validate Create,
Update, and Delete operations with representation and minimal response modes:

```bash
# Docker
docker compose --profile compliance-addedit up compliance-addedit

# Local CLI
cd tools/certification/add-edit
npx tsx src/cli/index.ts --server-url http://localhost:8080 --auth-token test-token \
  --compliance-report ./compliance-report.json --spec-version 2.0.0
```

**Results:** 8 passed, 0 failed.

The `--compliance-report` flag generates a structured JSON report with per-scenario
details (fields tested, enum values, expansion names, failure details) suitable
for API submission. The `--spec-version` flag sets the version in the report.

### Compliance Results

Test results are written to Docker volumes and can be found in the container
output logs. The RESOScript test configuration is automatically generated from
the server's live EDMX metadata — no manual configuration required.

---

## Data Generator CLI

The data generator can also be used as a standalone CLI tool for producing test
data in JSON or curl script format:

```bash
cd tools/data-generator

# Generate 10 properties with all dependencies (Office, Member, Teams, etc.)
npx tsx src/cli.ts -r Property -n 10 -f json -o ./output --deps

# Generate without dependency resolution
npx tsx src/cli.ts -r Member -n 5 -f json -o ./output --no-deps

# Generate curl scripts for posting to a server
npx tsx src/cli.ts -r Property -n 10 -f curl -o ./output \
  --server-url http://localhost:8080 \
  --auth-token test-token
```

The `--deps` flag (enabled by default) automatically:

1. Discovers FK relationships from the DD 2.0 metadata
2. Builds a dependency graph and topologically sorts resources
3. Creates dependent records first (e.g., Office → Member → Property)
4. Injects valid FK values so all cross-resource references are correct
5. Back-fills circular dependencies (Office ↔ Member) after initial creation
