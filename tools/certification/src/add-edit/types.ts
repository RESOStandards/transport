// Re-export generic types from test-runner
export type {
  AuthConfig,
  TestConfig,
  EntityProperty,
  EntityType,
  ParsedMetadata,
  TestStatus,
  TestAssertion,
  ScenarioResult,
  TestReport,
  ODataResponse,
  MockServerOptions
} from '../test-runner/types.js';

// ── Add/Edit-specific types ──

export interface PayloadSet {
  readonly createSucceeds: Record<string, unknown>;
  readonly createFails: Record<string, unknown>;
  readonly updateSucceeds: Record<string, unknown>;
  readonly updateFails: Record<string, unknown>;
  readonly deleteSucceeds: DeletePayload;
  readonly deleteFails: DeletePayload;
}

export interface DeletePayload {
  /** Full URL or relative path to the resource to delete, e.g. "Property('12345')" */
  readonly url?: string;
  /** Alternative: just the key value */
  readonly id?: string;
}

export type ScenarioName =
  | 'create-succeeds-representation'
  | 'create-succeeds-minimal'
  | 'create-fails'
  | 'update-succeeds-representation'
  | 'update-succeeds-minimal'
  | 'update-fails'
  | 'delete-succeeds'
  | 'delete-fails';
