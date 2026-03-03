// ── Authentication ──

/** Authentication configuration: either a pre-fetched bearer token or OAuth2 Client Credentials. */
export type AuthConfig =
  | { readonly mode: 'token'; readonly authToken: string }
  | {
      readonly mode: 'client_credentials';
      readonly clientId: string;
      readonly clientSecret: string;
      readonly tokenUrl: string;
    };

// ── Configuration ──

export interface TestConfig {
  /** Server base URL, e.g. "https://api.reso.org" */
  readonly serverUrl: string;
  /** OData resource name, e.g. "Property" */
  readonly resource: string;
  /** Path to directory containing payload JSON files */
  readonly payloadsDir: string;
  /** Authentication configuration (bearer token or OAuth2 Client Credentials) */
  readonly auth: AuthConfig;
  /** Optional path to local XML metadata file (skips $metadata fetch) */
  readonly metadataPath?: string;
  /** If true, start mock server instead of hitting real server */
  readonly useMock?: boolean;
}

// ── Metadata ──

export interface EntityProperty {
  readonly name: string;
  /** e.g. "Edm.String", "Edm.Decimal", "Collection(Edm.String)" */
  readonly type: string;
  readonly nullable?: boolean;
  readonly maxLength?: number;
  readonly precision?: number;
  readonly scale?: number;
  readonly annotations?: Readonly<Record<string, string>>;
}

export interface EntityType {
  readonly name: string;
  readonly keyProperties: ReadonlyArray<string>;
  readonly properties: ReadonlyArray<EntityProperty>;
}

export interface ParsedMetadata {
  readonly namespace: string;
  readonly entityTypes: ReadonlyArray<EntityType>;
}

// ── Test Results ──

export type TestStatus = 'pass' | 'fail' | 'skip' | 'warn';

export interface TestAssertion {
  readonly description: string;
  readonly status: TestStatus;
  readonly expected?: string;
  readonly actual?: string;
  /** Gherkin step text this maps to */
  readonly gherkinStep?: string;
}

export interface ScenarioResult {
  readonly scenario: string;
  readonly tags: ReadonlyArray<string>;
  readonly assertions: ReadonlyArray<TestAssertion>;
  readonly passed: boolean;
  /** Duration in milliseconds */
  readonly duration: number;
}

export interface TestReport {
  readonly serverUrl: string;
  readonly resource: string;
  readonly timestamp: string;
  readonly scenarios: ReadonlyArray<ScenarioResult>;
  readonly summary: {
    readonly total: number;
    readonly passed: number;
    readonly failed: number;
    readonly skipped: number;
  };
}

// ── HTTP Layer ──

export interface ODataResponse {
  readonly status: number;
  readonly headers: Readonly<Record<string, string>>;
  readonly body: unknown;
  readonly rawBody: string;
}

// ── Mock Server ──

export interface MockServerOptions {
  readonly port?: number;
  readonly metadataXml: string;
  readonly resource: string;
}
