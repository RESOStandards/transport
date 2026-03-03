// Add/Edit scenario runner
export { runAllScenarios } from './lib/test-runner.js';

// Mock server
export { startMockServer, stopMockServer } from './mock/server.js';

// Re-export generic test infrastructure from test-runner
export {
  parseMetadataXml,
  fetchMetadata,
  loadMetadataFromFile,
  getEntityType,
  validatePayloadAgainstMetadata,
  toResoFields,
  resolveAuthToken,
  fetchAccessToken,
  formatConsoleReport,
  formatJsonReport,
  odataRequest,
  buildResourceUrl,
  extractPrimaryKey,
  stripPrimaryKey,
  makeSchemaAssertion,
  buildScenarioResult
} from '@reso/certification-test-runner';

export type { ValidationFailure } from '@reso/validation';

// Types — generic from test-runner + add-edit-specific
export type {
  TestConfig,
  TestReport,
  ScenarioResult,
  TestAssertion,
  ParsedMetadata,
  EntityType,
  EntityProperty,
  MockServerOptions,
  ODataResponse,
  TestStatus,
  AuthConfig
} from '@reso/certification-test-runner';

export type { PayloadSet, DeletePayload, ScenarioName } from './lib/types.js';
