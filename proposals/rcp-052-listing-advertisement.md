# Universal Real Estate Protocol (UREP)

| **RCP** | 52 |
| :--- | :--- |
| **Version** | **1.0.0** |
| **Authors** | [Patrick Coca](mailto:patrick@sourceredb.com)<br />[Josh Darnell](mailto:josh@darnjo.com)<br />[Chris Haran](mailto:chris.haran@mredllc.com)<br />[Rick Herrera](mailto:rick.herrera@constellation1.com)<br />[Ranga Krishnan](mailto:ranga@bei.re)<br />[Mark Lesswing](mailto:mark@lesswing.com)<br />[Catharine Macintosh](mailto:catharine@listed.inc)<br />[Paul Stusiak](mailto:pstusiak@falcontechnologies.com) |
| **Status** | IN PROGRESS |
| **Date Ratified** | TBD |
| **Dependencies** | [Data Dictionary 1.7+](https://ddwiki.reso.org/display/DDW17/Data+Dictionary+1.7+Wiki)<br />[RESO Common Format](./reso-common-format.md)<br />[Web API Add/Edit](./web-api-add-edit.md)<br />[Web API Validation Expressions](./validation-expressions.md) |
| **Related Links** | [Discussion #162](https://github.com/RESOStandards/transport/discussions/162)<br />[UREP Artifacts](../artifacts/urep/README.md)<br />[ActivityPub Specification](https://www.w3.org/TR/activitypub/)<br />[ActivityStreams Vocabulary](https://www.w3.org/TR/activitystreams-vocabulary/) |

<br />

# RESO End User License Agreement (EULA)

This End User License Agreement (the "EULA") is entered into by and between the Real Estate Standards Organization ("RESO") and the person or entity ("End User") that is downloading or otherwise obtaining the product associated with this EULA ("RESO Product"). This EULA governs End Users use of the RESO Product and End User agrees to the terms of this EULA by downloading or otherwise obtaining or using the RESO Product.

<br />

# Table of Contents

- [Summary of Changes](#summary-of-changes)
- [Introduction](#introduction)
- [Section 1: Purpose](#section-1-purpose)
- [Section 2: Protocol Model](#section-2-protocol-model)
- [Section 3: Discovery and Negotiation](#section-3-discovery-and-negotiation)
- [Section 4: Services, Capabilities, and Transports](#section-4-services-capabilities-and-transports)
- [Section 5: Core Real Estate Capabilities](#section-5-core-real-estate-capabilities)
- [Section 6: Security, Authorization, and Auditability](#section-6-security-authorization-and-auditability)
- [Section 7: Conformance](#section-7-conformance)
- [Section 8: Contributors](#section-8-contributors)
- [Section 9: References](#section-9-references)
- [Section 10: Appendices](#section-10-appendices)
- [Section 11: License](#section-11-license)

<br />

# Summary of Changes

- Defines the Universal Real Estate Protocol (UREP), a discovery-first protocol for interoperable real estate systems, applications, and agents.
- Introduces RESO-governed capability identifiers, profile discovery, server-selected capability negotiation, and transport bindings for REST, MCP, and ActivityPub-compatible event threads.
- Defines core real estate capabilities for listing discovery, listing workspace collaboration, MLS validation, showing coordination, offers, transaction timelines, and listing activity streams.
- Adds machine-readable artifacts under [`artifacts/urep`](../artifacts/urep/README.md), including JSON Schemas, REST OpenAPI, MCP OpenRPC, and example payloads that implementers can validate against.

<br />

# Introduction

Real estate data exchange has historically centered on resource retrieval, replication, and point-to-point integrations. Those primitives remain essential, but modern workflows increasingly require applications, service providers, MLSs, brokerages, and agents to coordinate actions across a listing lifecycle. Examples include premarketing preparation, MLS validation, showing coordination, offer intake, document exchange, and post-listing activity reporting.

UREP provides a common protocol layer for those workflows. It does not replace the RESO Data Dictionary, RESO Common Format, Web API Core, Add/Edit, Validation Expressions, EntityEvent, or Webhooks. Instead, it composes them into a discoverable, capability-based protocol so participants can declare what they support and clients can invoke compatible real estate workflows without custom integrations for every counterparty.

The protocol is designed for traditional applications and agentic systems. A consumer might be a brokerage desktop app, a showing tool, a transaction coordinator, an AI assistant acting for a principal, or another provider. A provider might be an MLS, brokerage, listing input system, showing system, offer platform, validation service, or compliance service.

<br />

# Section 1: Purpose

UREP addresses five goals:

1. **Discovery** - A participant can publish a machine-readable profile describing the real estate services, transports, capabilities, identifiers, and policy requirements it supports.
2. **Negotiation** - A provider can choose the mutually compatible capabilities for a request based on provider and consumer profiles.
3. **Composable workflows** - Core real estate workflows are decomposed into independently versioned capabilities and extensions rather than one monolithic API.
4. **Transport independence** - REST, MCP, and ActivityPub-compatible event threads can expose the same capability model.
5. **Conformance** - Implementers can validate discovery profiles, payloads, and service contracts using the artifacts shipped with this RCP.

The key words **MUST**, **MUST NOT**, **REQUIRED**, **SHALL**, **SHALL NOT**, **SHOULD**, **SHOULD NOT**, **RECOMMENDED**, **MAY**, and **OPTIONAL** in this document are to be interpreted as described in RFC 2119 and RFC 8174.

<br />

# Section 2: Protocol Model

## Section 2.1: Participants and Roles

UREP roles are contextual. A system may act as a provider in one interaction and a consumer in another.

| Role | Description |
| :--- | :--- |
| **Provider** | Participant exposing one or more UREP services and capabilities. Examples include MLSs, brokerages, listing input systems, showing providers, offer systems, and validation services. |
| **Consumer** | Participant discovering and invoking provider capabilities. Examples include apps, AI agents, integration services, and other providers. |
| **Principal** | Person or organization on whose behalf a consumer acts. Examples include listing agent, buyer agent, brokerage, seller, buyer, or MLS operator. |
| **Authorizing Organization** | Organization that grants, scopes, or constrains access. Examples include MLSs, brokerages, offices, and service providers. |
| **Service Actor** | Automated participant that contributes to a workflow. Examples include AI description tools, public-records services, fair-housing checks, showing systems, or offer platforms. |

## Section 2.2: Core Objects

UREP uses the following top-level object families:

- **Profile** - A discovery document published by a provider or consumer.
- **Service** - A group of operations and events for a domain, such as real estate workflows.
- **Capability** - A versioned function within a service, such as listing search or showing request.
- **Extension** - A capability that augments one or more parent capabilities.
- **Session** - A stateful collaboration object, such as a listing workspace or showing request.
- **Activity** - An append-only event in a listing, showing, offer, or transaction thread.

All profile, service, and capability names use reverse-domain naming. RESO-governed names use the `org.reso` namespace. Vendor-specific extensions MUST use a namespace controlled by the vendor or organization.

## Section 2.3: Relationship to Existing RESO Standards

UREP composes existing RESO standards:

- **Data Dictionary** defines canonical fields, lookups, resource names, and identifiers.
- **RESO Common Format** defines JSON payload shape for linked real estate records.
- **Web API Add/Edit** defines interoperable create/update/delete semantics and error format.
- **Validation Expressions** define machine-readable validation rules.
- **EntityEvent and Webhooks** define replication and push event primitives.

UREP payloads that represent RESO resources SHOULD use RESO Common Format with `@reso.context`. UREP-specific wrappers add negotiation metadata, workflow state, and activity threading around those payloads.

<br />

# Section 3: Discovery and Negotiation

## Section 3.1: Discovery Profile

Providers SHOULD publish a JSON discovery profile at:

```text
GET https://{provider-origin}/.well-known/urep
```

The profile MUST include:

- `urep.version`
- `urep.services`
- `urep.capabilities`
- participant identity, including at least one resolvable organization identifier
- supported authorization methods

Providers that support multiple protocol versions SHOULD advertise version-specific profiles through `supported_versions`.

The discovery profile schema is defined in [`artifacts/urep/schemas/urep.json`](../artifacts/urep/schemas/urep.json).

## Section 3.2: Consumer Profile Advertisement

Consumers SHOULD advertise their profile on each request using the `UREP-Consumer` header:

```text
UREP-Consumer: profile="https://consumer.example.com/.well-known/urep"
```

Consumers MAY also use an equivalent transport-native metadata field. MCP tools, for example, MAY include the consumer profile URI in their request metadata.

## Section 3.3: Capability Negotiation

Capability negotiation follows a provider-selected model:

1. The provider loads its own profile and the consumer profile when present.
2. The provider intersects capabilities by name.
3. For each shared capability, the provider selects the highest mutually supported version.
4. Extensions whose parent capabilities are not active are removed.
5. The provider includes active capabilities in each UREP response.

Providers MAY apply policy after intersection. For example, an MLS may support offer submission generally but decline it for a consumer that lacks appropriate authorization for a specific listing.

## Section 3.4: Namespace Governance

Capability and service identifiers MUST use reverse-domain names.

| Namespace | Authority | Examples |
| :--- | :--- | :--- |
| `org.reso.*` | RESO-governed protocol elements | `org.reso.realestate.listing.discovery` |
| `com.example.*` | Vendor-governed extensions | `com.example.valuation.avm` |
| `org.example.*` | Organization-governed extensions | `org.example.compliance.local_review` |

Official UREP artifacts use `org.reso` identifiers. Custom capabilities MUST NOT use the `org.reso` namespace unless ratified by RESO.

<br />

# Section 4: Services, Capabilities, and Transports

## Section 4.1: Real Estate Service

The initial UREP service is:

```text
org.reso.realestate
```

This service covers listing lifecycle workflows, showing coordination, offer workflows, MLS validation, and activity threads.

## Section 4.2: Transport Bindings

UREP defines three initial transport bindings:

| Transport | Artifact | Best fit |
| :--- | :--- | :--- |
| `rest` | [`rest.openapi.json`](../artifacts/urep/services/real-estate/rest.openapi.json) | Server-to-server and application integrations |
| `mcp` | [`mcp.openrpc.json`](../artifacts/urep/services/real-estate/mcp.openrpc.json) | AI agents and tool-calling clients |
| `activitypub` | ActivityStreams objects and inbox/outbox URLs | Federated event threads and distributed notifications |

Providers MAY expose more than one transport for the same service. Consumers SHOULD choose the transport that best fits their execution environment.

## Section 4.3: UREP Response Envelope

UREP responses SHOULD include:

- `urep` - protocol metadata and active capability set
- `data` - payload for successful responses
- `errors` - Add/Edit-compatible error objects for unsuccessful responses
- `warnings` - non-fatal notices, such as ignored extension data
- `activity` - link or embedded ActivityStreams object when the response appends to a thread

<br />

# Section 5: Core Real Estate Capabilities

## Section 5.1: Capability Inventory

The initial official capabilities are:

| Capability | Description |
| :--- | :--- |
| `org.reso.realestate.listing.discovery` | Search, lookup, and inspect listing records or pre-listing records that the principal is authorized to access. |
| `org.reso.realestate.listing.workspace` | Create and update collaborative listing workspaces before, during, and after MLS submission. |
| `org.reso.realestate.listing.validation` | Request MLS, brokerage, compliance, or validation-service review of a listing workspace. |
| `org.reso.realestate.showing` | Request, accept, decline, reschedule, or cancel showing appointments. |
| `org.reso.realestate.offer` | Submit, counter, accept, reject, withdraw, or record offers. |
| `org.reso.realestate.transaction.timeline` | Append and retrieve transaction milestones after a listing enters a transaction workflow. |
| `org.reso.realestate.activity.stream` | Publish ActivityStreams-compatible events into listing, showing, offer, and transaction threads. |

The initial extension candidates are:

| Extension | Extends | Description |
| :--- | :--- | :--- |
| `org.reso.realestate.media.generation` | listing.workspace | Service actor output for photos, descriptions, floor plans, and generated marketing content. |
| `org.reso.realestate.public_records` | listing.workspace | Public-records enrichment for listing preparation and compliance checks. |
| `org.reso.realestate.fair_housing.review` | listing.validation | Fair-housing language and policy checks. |
| `org.reso.realestate.analytics.metrics` | activity.stream | Listing exposure, engagement, syndication, and campaign metrics. |

## Section 5.2: Listing Discovery

Listing discovery supports authorized search and lookup of listings, pre-listings, and listing-workspace snapshots. Implementations SHOULD return RESO Common Format records where the core record is a Data Dictionary resource.

Discovery requests MAY include:

- location filters
- listing status filters
- office, agent, or organization filters
- resource type
- listing lifecycle phase
- pagination

Discovery responses MUST NOT leak records outside the consumer principal's authorization scope.

## Section 5.3: Listing Workspace

A listing workspace is a collaborative pre-publication or post-publication working object. It can contain:

- RESO Common Format record payload
- actor attribution
- data-source links
- validation status
- activity thread URL
- current lifecycle phase

The initial listing advertisement use case from Discussion #162 maps to the workspace capability:

1. An agent creates a workspace from a draft Property payload.
2. The workspace publishes a `#premarketing` activity.
3. Service actors reply with media, public records, compliance checks, and generated content.
4. The listing agent or brokerage merges, rejects, or modifies actor output.
5. The workspace is submitted for MLS validation.

## Section 5.4: Listing Validation

Validation providers MAY include MLSs, brokerages, compliance services, or automated validation engines. Validation requests SHOULD identify the workspace or record under review and MAY link to RESO Validation Expressions.

Validation responses MUST use one of:

- `accepted`
- `rejected`
- `accepted_with_warnings`
- `pending_manual_review`

Rejected responses SHOULD include an Add/Edit-compatible error payload and SHOULD append a validation activity to the workspace thread.

## Section 5.5: Showing

Showing workflows support appointment requests and state changes across showing systems. Showing objects SHOULD include:

- listing reference
- requesting principal
- responding principal or organization
- requested time windows
- selected appointment time
- access notes or secure access references
- state
- activity thread reference

Consumers MUST NOT receive access details unless authorized for the appointment state and principal role.

## Section 5.6: Offer

Offer workflows support offer submission and lifecycle state changes. Offer payloads SHOULD avoid embedding confidential documents directly. Instead, they SHOULD link to secure, authorization-scoped document or package URLs.

Offer states include:

- `draft`
- `submitted`
- `countered`
- `accepted`
- `rejected`
- `withdrawn`
- `expired`

Offer acceptance SHOULD append an activity that identifies the accepted offer and notifies authorized parties such as buyer agent, listing agent, brokerage, MLS, and closing service.

## Section 5.7: Activity Stream

UREP activity streams use ActivityStreams vocabulary and ActivityPub-compatible addressing where federated delivery is desired. The ActivityPub transport is optional; the activity object model can still be used in REST and MCP responses.

Activities SHOULD include:

- `type`
- `id`
- `published`
- `actor`
- `object`
- `to`
- `cc`
- `inReplyTo` when part of a thread
- `attributedTo` when attribution differs from delivery actor

Hashtags such as `#premarketing`, `#marketing`, `#published`, `#rejected`, `#showingrequest`, and `#accepted` MAY be used as human-readable workflow hints. Machine processing MUST use structured fields such as capability, state, object type, and activity type rather than relying only on hashtags.

<br />

# Section 6: Security, Authorization, and Auditability

## Section 6.1: Authorization

UREP providers MUST require authorization for non-public data and actions. Supported authorization mechanisms include:

- OAuth 2.0 bearer tokens
- OpenID Connect user authentication
- mTLS or API key trust for service-to-service integrations
- signed requests for high-assurance workflows

Consumers acting for a principal MUST be able to identify that principal and the authorization scope for the requested action.

## Section 6.2: Identity and Attribution

UREP implementations SHOULD use RESO identifiers where available:

- ULI for licensees
- UOI for organizations
- USI for systems

Activity attribution MUST distinguish between the authenticated caller, the principal, and any automated service actor that generated content.

## Section 6.3: Request Integrity and Idempotency

Mutating operations SHOULD support:

- idempotency keys
- request IDs
- content digests
- detached signatures for high-value events
- durable audit logs

Providers SHOULD return the request ID in responses and activity records so multi-party workflows can be audited across systems.

## Section 6.4: Privacy and Data Minimization

Providers MUST enforce authorization at the record, activity, and attachment level. Offer documents, access instructions, seller data, buyer data, and private comments MUST NOT be placed in public activity payloads. Secure URLs SHOULD be scoped, expiring, and access-controlled.

<br />

# Section 7: Conformance

UREP conformance has three levels.

## Section 7.1: Profile Conformance

Profile-conformant implementations MUST:

- publish or consume a valid UREP profile
- use valid service and capability identifiers
- declare supported transports
- declare supported authorization methods
- pass JSON Schema validation for profile documents

## Section 7.2: Capability Conformance

Capability-conformant implementations MUST:

- implement the required operations for a declared capability
- include active capability metadata in responses
- reject unsupported or unauthorized capability use with structured errors
- pass JSON Schema validation for request and response examples for that capability

## Section 7.3: Workflow Conformance

Workflow-conformant implementations MUST demonstrate end-to-end behavior for at least one complete workflow:

- listing workspace creation through MLS validation
- showing request through acceptance or decline
- offer submission through disposition
- activity stream append and retrieval

Conformance tools SHOULD use the artifacts in `artifacts/urep` as the canonical validation surface for this RCP.

<br />

# Section 8: Contributors

This document was written by [Patrick Coca](mailto:patrick@sourceredb.com), [Josh Darnell](mailto:josh@darnjo.com), [Chris Haran](mailto:chris.haran@mredllc.com), [Rick Herrera](mailto:rick.herrera@constellation1.com), [Ranga Krishnan](mailto:ranga@bei.re), [Mark Lesswing](mailto:mark@lesswing.com), [Catharine Macintosh](mailto:catharine@listed.inc), and [Paul Stusiak](mailto:pstusiak@falcontechnologies.com).

<br />

# Section 9: References

- [ActivityStreams Vocabulary](https://www.w3.org/TR/activitystreams-vocabulary/)
- [ActivityPub Specification](https://www.w3.org/TR/activitypub/)
- [OAuth 2.0 Authorization Framework](https://datatracker.ietf.org/doc/html/rfc6749)
- [HTTP Message Signatures](https://www.rfc-editor.org/rfc/rfc9421)
- [RESO Unique Organization Identifier (UOI)](https://www.reso.org/reso-unique-identifiers/)
- [Uniform Resource Name (URN)](https://datatracker.ietf.org/doc/html/rfc8141)
- [RESO URN Assignment](https://www.iana.org/assignments/urn-formal/reso)

<br />

# Section 10: Appendices

## Appendix A: Initial Listing Advertisement Flow

The listing advertisement flow from Discussion #162 is implemented as a UREP workflow:

1. Consumer discovers provider support for `org.reso.realestate.listing.workspace`, `org.reso.realestate.listing.validation`, and `org.reso.realestate.activity.stream`.
2. Consumer creates a listing workspace with a RESO Common Format Property payload.
3. Provider appends a `#premarketing` activity and returns the active thread URL.
4. Media, public records, compliance, and MLS validation actors append activities or linked payloads.
5. Listing agent or brokerage accepts, modifies, or rejects contributed data.
6. Consumer submits the workspace for validation.
7. MLS validation appends `accepted`, `rejected`, or `accepted_with_warnings` activity state.
8. The workspace transitions to marketing, showing, offer, or transaction workflows as authorized.

## Appendix B: Artifact Inventory

The machine-readable artifact package is located under [`artifacts/urep`](../artifacts/urep/README.md). It includes:

- profile and metadata schemas
- real-estate domain schemas
- REST OpenAPI service definition
- MCP OpenRPC service definition
- example discovery profiles and workflow payloads
- conformance manifest for profile, service, and fixture validation

<br />

# Section 11: License

This document is covered by the [RESO EULA](https://www.reso.org/eula/).

Please [contact RESO](mailto:info@reso.org) if you have any questions.
