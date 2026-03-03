# src/cli/ — Command-Line Interface

CLI entry point for the RESO Web API Add/Edit compliance testing tool.

## Module

### index.ts
Defines the `reso-cert-add-edit` command using `commander`. Parses command-line arguments, optionally starts a local mock OData server, runs all 8 certification scenarios, and outputs results to the console or as JSON.

## Usage

```bash
reso-cert-add-edit \
  --url https://api.reso.org \
  --resource Property \
  --payloads ./sample-payloads \
  --auth-token <bearer-token> \
  [--metadata ./metadata.xml] \
  [--mock] \
  [--output console|json]
```

## Options

| Flag | Required | Description |
|------|----------|-------------|
| `--url <url>` | Yes | Server base URL |
| `--resource <name>` | Yes | OData resource/entity type name (e.g., `Property`) |
| `--payloads <dir>` | Yes | Directory containing the 6 payload JSON files |
| `--auth-token <token>` | Yes | Bearer token for authorization |
| `--metadata <path>` | No | Path to a local XML metadata file (otherwise fetched from server) |
| `--mock` | No | Start a local mock server on port 8800 instead of testing a real server |
| `--output <format>` | No | Output format: `console` (default) or `json` |

## Exit Codes

- `0` — All scenarios passed
- `1` — One or more scenarios failed
- `2` — Runtime error (e.g., missing files, network failure)

## Installation

This package is designed for local use only:

```bash
cd tools/certification/add-edit
npm install
npm run build
npm link
```

The `reso-cert-add-edit` command will then be available globally on your system.
