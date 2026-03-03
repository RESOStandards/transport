export { runAllScenarios } from './lib/test-runner.js';
export {
  parseMetadataXml,
  fetchMetadata,
  loadMetadataFromFile,
  getEntityType,
  validatePayloadAgainstMetadata
} from './lib/metadata.js';
export { resolveAuthToken, fetchAccessToken } from './lib/auth.js';
export { formatConsoleReport, formatJsonReport } from './lib/reporter.js';
export { startMockServer, stopMockServer } from './mock/server.js';

export type {
  TestConfig,
  TestReport,
  ScenarioResult,
  TestAssertion,
  PayloadSet,
  ParsedMetadata,
  EntityType,
  EntityProperty,
  MockServerOptions,
  ODataResponse,
  DeletePayload,
  ScenarioName,
  TestStatus,
  AuthConfig
} from './lib/types.js';
