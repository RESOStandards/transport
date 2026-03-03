# RESO Reference Server — UI

React-based web interface for browsing and editing RESO Data Dictionary records served by the reference server.

## Status

**Stage 2 — Not yet implemented.** This package will be built after the server API is complete and verified.

## Planned Features

- Resource browser: select from Property, Member, Office, Media, OpenHouse, Showing
- Table view: paginated list of records with sortable columns
- Detail view: full record display with field metadata (type, description, lookup values)
- Create/Edit forms: dynamically generated from RESO metadata
- Lookup field support: dropdowns populated from RESO enumeration values

## Tech Stack

- React 19
- TypeScript
- Vite
- Communicates with the reference server OData API at `http://localhost:8080`

## Development

```bash
npm install
npm run dev
```

The dev server starts at `http://localhost:5173` with a proxy to the API server.
