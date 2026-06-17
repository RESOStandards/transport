# Universal Real Estate Protocol Artifacts

This directory contains the initial machine-readable artifact package for RCP-52, the Universal Real Estate Protocol (UREP).

## Layout

| Path | Purpose |
| :--- | :--- |
| `schemas/` | JSON Schema definitions for discovery profiles, services, capabilities, response envelopes, and real-estate domain payloads. |
| `services/real-estate/rest.openapi.json` | REST binding for the `org.reso.realestate` service. |
| `services/real-estate/mcp.openrpc.json` | MCP/OpenRPC binding for the `org.reso.realestate` service. |
| `examples/` | Provider and consumer profiles plus workflow payload examples. |
| `conformance/manifest.json` | Initial machine-readable manifest describing required profile, service, and fixture validation surfaces. |

## Conformance Use

Implementers can use these artifacts to validate:

- UREP discovery profiles served from `/.well-known/urep`
- negotiated service and capability declarations
- REST request and response payloads
- MCP tool parameters and results
- real-estate workflow payloads such as listing workspaces, validation results, showing requests, offers, and activity events

The conformance manifest is intentionally small and implementation-neutral. It identifies the artifact and fixture set that a future validator should load first, without prescribing a specific test runner or programming language.

The artifacts intentionally reference existing RESO standards instead of redefining them. Payloads that carry real estate resources should use RESO Common Format and include `@reso.context`.
