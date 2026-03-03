# RESO Reference Server — UI

React-based web interface for browsing and editing RESO Data Dictionary records served by the reference server.

## Features

- **Resource browser**: Select from Property, Member, Office, Media, OpenHouse, Showing
- **Search with infinite scroll**: OData `$filter` search with automatic pagination
- **Advanced search**: Field-level filters organized by RESO Data Dictionary groups
- **Detail view**: Full record display with fields grouped by category, media carousel
- **Add/Edit/Delete**: Dynamic forms generated from RESO metadata with client-side type validation
- **Media carousel**: Image viewer for expanded Media records (placeholder images for development)
- **Dark mode**: System-preference detection with manual toggle, persisted in URL
- **Responsive**: Mobile-friendly layout with Tailwind CSS

## Tech Stack

- React 19 + TypeScript
- Vite 6 (dev server + build)
- Tailwind CSS v4
- React Router v7 (URL-based state for shareable links)
- Communicates with the reference server OData API at `http://localhost:8080`

## Development

```bash
# Install dependencies
npm install

# Start the dev server (proxy to API at localhost:8080)
npm run dev

# Build for production
npm run build
```

The dev server starts at `http://localhost:5173`. All API requests are proxied to `http://localhost:8080` via the Vite dev server config.

**Prerequisites**: The reference server must be running at port 8080. Start it with:

```bash
cd ../  # reso-reference-server/
docker-compose up -d
```

## Configuration

### UI Config (`server/src/ui-config.json`)

Controls which fields appear in the summary results list for each resource.

```json
{
  "resources": {
    "Property": {
      "summaryFields": ["ListingKey", "ListingId", "ListPrice", "BedroomsTotal", ...]
    },
    "Member": { "summaryFields": "__all__" }
  }
}
```

- **Explicit field list**: Array of field names to show in summary cards
- **`"__all__"`**: Show all fields from metadata (used for smaller resources)

Served by the server at `GET /ui-config`.

### Field Groups (`server/src/field-groups.json`)

Maps Property fields to RESO Data Dictionary group categories for organizing detail pages, forms, and advanced search.

```json
{
  "Property": {
    "ListPrice": ["Listing", "Price"],
    "BedroomsTotal": ["Structure"],
    "City": ["Location", "Address"]
  }
}
```

The first element is the primary group (section header), subsequent elements are sub-groups. Non-Property resources have no groups and fields are listed alphabetically.

Served by the server at `GET /field-groups`.

## Routing

All view state is stored in URL query parameters for shareable links and browser history.

| Route | Page | Query Params |
|-------|------|-------------|
| `/:resource` | Search | `$filter`, `$orderby`, `mode`, `theme` |
| `/:resource/:key` | Detail | `theme` |
| `/:resource/add` | Add | `theme` |
| `/:resource/edit` | Edit (key prompt) | `theme` |
| `/:resource/edit/:key` | Edit (form) | `theme` |
| `/:resource/delete` | Delete | `theme` |

## Docker

Build and run with Docker Compose (from the `reso-reference-server/` directory):

```bash
docker-compose up -d
# UI: http://localhost:5173
# API: http://localhost:8080
```

The UI container uses nginx to serve the built React app and proxy API requests to the server container.
