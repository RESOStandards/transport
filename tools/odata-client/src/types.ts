/**
 * Core types for the OData client SDK.
 *
 * Inspired by Apache Olingo's ODataClient type system, adapted for
 * TypeScript with discriminated unions and immutable interfaces.
 */

// ---------------------------------------------------------------------------
// Query options
// ---------------------------------------------------------------------------

/** Structured OData system query options (URL Conventions §5.1). */
export interface ODataQueryOptions {
  readonly $filter?: string;
  readonly $select?: string;
  readonly $orderby?: string;
  readonly $top?: number;
  readonly $skip?: number;
  readonly $count?: boolean;
  readonly $expand?: string;
  readonly $search?: string;
  readonly $compute?: string;
  readonly $format?: string;
}

// ---------------------------------------------------------------------------
// HTTP response
// ---------------------------------------------------------------------------

/** Normalized OData HTTP response with lowercase header keys. */
export interface ODataResponse {
  readonly status: number;
  readonly headers: Readonly<Record<string, string>>;
  readonly body: unknown;
  readonly rawBody: string;
}

// ---------------------------------------------------------------------------
// OData JSON format
// ---------------------------------------------------------------------------

/** An OData entity (single record) with optional annotations. */
export type ODataEntity = Readonly<Record<string, unknown>>;

/** An OData entity collection response. */
export interface ODataCollection {
  readonly value: ReadonlyArray<ODataEntity>;
  readonly "@odata.context"?: string;
  readonly "@odata.count"?: number;
  readonly "@odata.nextLink"?: string;
}

/** Extracted OData annotations from an entity. */
export interface ODataAnnotations {
  readonly context?: string;
  readonly id?: string;
  readonly editLink?: string;
  readonly etag?: string;
}

// ---------------------------------------------------------------------------
// OData error format (per RESO Web API spec)
// ---------------------------------------------------------------------------

/** A single error detail entry. */
export interface ODataErrorDetail {
  readonly code?: string;
  readonly target: string;
  readonly message: string;
}

/** OData error response body. */
export interface ODataErrorBody {
  readonly error: {
    readonly code: string;
    readonly message: string;
    readonly details?: ReadonlyArray<ODataErrorDetail>;
  };
}

// ---------------------------------------------------------------------------
// Authentication
// ---------------------------------------------------------------------------

/** Bearer token authentication. */
export interface TokenAuth {
  readonly mode: "token";
  readonly authToken: string;
}

/** OAuth2 Client Credentials authentication. */
export interface ClientCredentialsAuth {
  readonly mode: "client_credentials";
  readonly clientId: string;
  readonly clientSecret: string;
  readonly tokenUrl: string;
}

/** Discriminated union for authentication configuration. */
export type AuthConfig = TokenAuth | ClientCredentialsAuth;

// ---------------------------------------------------------------------------
// Client configuration
// ---------------------------------------------------------------------------

/** Configuration for creating an OData client. */
export interface ClientConfig {
  readonly baseUrl: string;
  readonly auth: AuthConfig;
  readonly defaultHeaders?: Readonly<Record<string, string>>;
}

/** Prefer header options for write operations. */
export type PreferReturn = "representation" | "minimal";

/** Options for CRUD operations that support Prefer header and ETags. */
export interface WriteOptions {
  readonly prefer?: PreferReturn;
  /** If-Match header for optimistic concurrency (ETag value). Use "*" to match any. */
  readonly ifMatch?: string;
  /** If-None-Match header. Use "*" to only create (prevent update). */
  readonly ifNoneMatch?: string;
}

// ---------------------------------------------------------------------------
// OData client object
// ---------------------------------------------------------------------------

/** The OData client object returned by `createClient()`. */
export interface ODataClient {
  readonly baseUrl: string;
  readonly request: (
    method: "GET" | "POST" | "PUT" | "PATCH" | "DELETE",
    url: string,
    options?: {
      readonly body?: unknown;
      readonly headers?: Readonly<Record<string, string>>;
    },
  ) => Promise<ODataResponse>;
}
