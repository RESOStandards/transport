// Types
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
} from './types.js';
export type { ValidationFailure } from '@reso/validation';
export type { RequestOptions } from './client.js';

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
} from './validators.js';

// Reporter
export { formatConsoleReport, formatJsonReport } from './reporter.js';

// Client
export { odataRequest, buildResourceUrl } from './client.js';

// Auth
export { resolveAuthToken, fetchAccessToken } from './auth.js';

// Metadata
export {
  fetchMetadata,
  loadMetadataFromFile,
  parseMetadataXml,
  getEntityType,
  validatePayloadAgainstMetadata,
  toResoFields
} from './metadata.js';

// Helpers
export { extractPrimaryKey, stripPrimaryKey, makeSchemaAssertion, buildScenarioResult } from './helpers.js';
