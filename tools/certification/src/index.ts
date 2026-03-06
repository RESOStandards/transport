// ── Test Runner (shared infrastructure) ──

// Validators
export {
  validateODataVersionHeader,
  validateStatusCode,
  validateStatusCodeRange,
  validateEntityIdHeader,
  validateLocationHeader,
  validatePreferenceApplied,
  validateJsonResponse,
  validateEmptyResponse,
  validateODataAnnotation,
  validateResponseContainsPayload,
  validateODataError
} from './test-runner/validators.js';

// Reporter
export { formatConsoleReport, formatJsonReport } from './test-runner/reporter.js';

// Client
export { odataRequest, buildResourceUrl } from './test-runner/client.js';
export type { RequestOptions } from './test-runner/client.js';

// Auth
export { resolveAuthToken, fetchAccessToken } from './test-runner/auth.js';

// Metadata
export {
  fetchMetadata,
  loadMetadataFromFile,
  parseMetadataXml,
  getEntityType,
  validatePayloadAgainstMetadata,
  toResoFields
} from './test-runner/metadata.js';

// Helpers
export { extractPrimaryKey, stripPrimaryKey, makeSchemaAssertion, buildScenarioResult } from './test-runner/helpers.js';

// Edm type validator
export { validateValueAgainstEdm, validateRecordAgainstMetadata } from './test-runner/edm-validator.js';

// Types
export type {
  AuthConfig,
  TestConfig,
  TestReport,
  ScenarioResult,
  TestAssertion,
  TestStatus,
  ParsedMetadata,
  EntityType,
  EntityProperty,
  MockServerOptions,
  ODataResponse
} from './test-runner/types.js';

export type { ValidationFailure } from '@reso/validation';

// ── Add/Edit (RCP-010) ──

export { runAllScenarios } from './add-edit/index.js';
export { generateComplianceReport } from './add-edit/index.js';
export type { ComplianceReport, ScenarioDetail, ReportConfig } from './add-edit/compliance-report.js';
export { startMockServer, stopMockServer } from './add-edit/index.js';
export type { PayloadSet, DeletePayload, ScenarioName } from './add-edit/types.js';

// ── EntityEvent (RCP-027) ──

export { runAllEntityEventScenarios } from './entity-event/index.js';
export { generateEntityEventComplianceReport } from './entity-event/index.js';
export { validateEventData } from './entity-event/index.js';
export type {
  EntityEventComplianceReport,
  EntityEventScenarioDetail,
  EntityEventReportConfig
} from './entity-event/compliance-report.js';
export type {
  EntityEventConfig,
  EntityEventMode,
  EntityEventScenarioName,
  EntityEventRecord,
  DataValidationResult,
  EntityEventTestReport
} from './entity-event/types.js';

// ── Web API Core (stub) ──

export { WEB_API_CORE_VERSION } from './web-api-core/index.js';

// ── Data Dictionary (stub) ──

export { DATA_DICTIONARY_VERSION } from './data-dictionary/index.js';
