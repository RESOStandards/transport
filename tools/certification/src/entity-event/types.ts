/**
 * EntityEvent (RCP-027) compliance testing types.
 */

import type { AuthConfig, TestReport } from '../test-runner/types.js';

// ── Configuration ──

export type EntityEventMode = 'observe' | 'full';

export interface EntityEventConfig {
  /** Service root URL, e.g. "https://api.reso.org" */
  readonly serverUrl: string;
  /** Authentication configuration */
  readonly auth: AuthConfig;
  /** Testing mode: observe (read-only) or full (write access) */
  readonly mode: EntityEventMode;
  /** Canary resource for full mode writes (default: Property) */
  readonly writableResource: string;
  /** Path to directory containing create-succeeds.json for canary writes (full mode only) */
  readonly payloadsDir?: string;
  /** Max EntityEvent records to validate (default: 1000) */
  readonly maxEvents: number;
  /** Keys per batch fetch request (default: 100) */
  readonly batchSize: number;
  /** Time between incremental sync checks in ms (default: 30000) */
  readonly pollIntervalMs: number;
  /** Max time to wait for new events in observe mode in ms (default: 120000) */
  readonly pollTimeoutMs: number;
  /** Unknown fields in fetched records are failures when true, warnings when false */
  readonly strict: boolean;
  /** Optional path to local XML metadata file */
  readonly metadataPath?: string;
}

// ── Scenario Names ──

export type EntityEventScenarioName =
  | 'metadata-valid'
  | 'read-only-enforced'
  | 'event-structure'
  | 'sequence-monotonic'
  | 'query-filter'
  | 'query-orderby-top-skip'
  | 'query-count'
  | 'incremental-sync'
  | 'create-triggers-event'
  | 'update-triggers-event'
  | 'delete-triggers-event';

// ── Data Shapes ──

export interface EntityEventRecord {
  readonly EntityEventSequence: number;
  readonly ResourceName: string;
  readonly ResourceRecordKey: string;
  readonly ResourceRecordUrl?: string;
}

export interface DataValidationResult {
  readonly totalEvents: number;
  readonly uniqueResources: ReadonlyArray<string>;
  readonly sequenceRange: { readonly min: number; readonly max: number } | null;
  readonly eventsValidated: number;
  readonly notFoundCount: number;
  readonly notFoundKeys: ReadonlyArray<{ readonly resource: string; readonly key: string }>;
  readonly pollDurationMs: number;
  readonly newEventsDuringPoll: number;
  readonly warnings: ReadonlyArray<string>;
}

// ── Report ──

export interface EntityEventTestReport extends TestReport {
  readonly mode: EntityEventMode;
  readonly dataValidation: DataValidationResult;
}
