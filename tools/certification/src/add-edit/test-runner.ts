import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import {
  buildResourceUrl,
  buildScenarioResult,
  extractPrimaryKey,
  fetchMetadata,
  getEntityType,
  loadMetadataFromFile,
  makeSchemaAssertion,
  odataRequest,
  parseMetadataXml,
  resolveAuthToken,
  stripPrimaryKey,
  validateEntityIdHeader,
  validateJsonResponse,
  validateLocationHeader,
  validateODataAnnotation,
  validateODataError,
  validateODataVersionHeader,
  validatePreferenceApplied,
  validateResponseContainsPayload,
  validateStatusCode,
  validateStatusCodeRange
} from '../test-runner/index.js';
import type { EntityType, ScenarioResult, TestAssertion, TestConfig, TestReport } from '../test-runner/types.js';
import type { DeletePayload, PayloadSet, ScenarioName } from './types.js';

// ── Public API ──

/**
 * Runs all 8 Add/Edit certification scenarios against a server sequentially.
 *
 * 1. Loads metadata (from server or local file)
 * 2. Validates the target resource exists in metadata
 * 3. Loads all 6 payload files from the payloads directory
 * 4. Runs each scenario: create (representation, minimal, fails), update (representation,
 *    minimal, fails), delete (succeeds, fails)
 * 5. Returns a structured TestReport with per-scenario results and a summary
 */
export const runAllScenarios = async (config: TestConfig): Promise<TestReport> => {
  const authToken = await resolveAuthToken(config.auth);

  const metadataXml = config.metadataPath
    ? await loadMetadataFromFile(config.metadataPath)
    : await fetchMetadata(config.serverUrl, authToken);

  const metadata = parseMetadataXml(metadataXml);
  const entityType = getEntityType(metadata, config.resource);

  if (!entityType) {
    throw new Error(
      `Entity type "${config.resource}" not found in metadata. Available: ${metadata.entityTypes.map(et => et.name).join(', ')}`
    );
  }

  const payloads = await loadPayloads(config.payloadsDir);

  const scenarios: ScenarioResult[] = [];

  scenarios.push(await runCreateSucceedsRepresentation(config, authToken, payloads.createSucceeds, entityType));
  scenarios.push(await runCreateSucceedsMinimal(config, authToken, payloads.createSucceeds, entityType));
  scenarios.push(await runCreateFails(config, authToken, payloads.createFails, entityType));
  scenarios.push(await runUpdateSucceedsRepresentation(config, authToken, payloads.updateSucceeds, entityType));
  scenarios.push(await runUpdateSucceedsMinimal(config, authToken, payloads.updateSucceeds, entityType));
  scenarios.push(await runUpdateFails(config, authToken, payloads.updateFails, entityType));
  scenarios.push(await runDeleteSucceeds(config, authToken, payloads.deleteSucceeds, entityType));
  scenarios.push(await runDeleteFails(config, authToken, payloads.deleteFails));

  const passed = scenarios.filter(s => s.passed).length;
  const failed = scenarios.filter(s => !s.passed).length;

  return {
    serverUrl: config.serverUrl,
    resource: config.resource,
    timestamp: new Date().toISOString(),
    scenarios,
    summary: {
      total: scenarios.length,
      passed,
      failed,
      skipped: 0
    }
  };
};

// ── Payload Loading ──

/**
 * Loads all 6 payload JSON files from a directory:
 * create-succeeds.json, create-fails.json, update-succeeds.json,
 * update-fails.json, delete-succeeds.json, delete-fails.json
 */
const loadPayloads = async (payloadsDir: string): Promise<PayloadSet> => {
  const loadJson = async (filename: string): Promise<Record<string, unknown>> => {
    const content = await readFile(join(payloadsDir, filename), 'utf-8');
    return JSON.parse(content) as Record<string, unknown>;
  };

  return {
    createSucceeds: await loadJson('create-succeeds.json'),
    createFails: await loadJson('create-fails.json'),
    updateSucceeds: await loadJson('update-succeeds.json'),
    updateFails: await loadJson('update-fails.json'),
    deleteSucceeds: (await loadJson('delete-succeeds.json')) as unknown as DeletePayload,
    deleteFails: (await loadJson('delete-fails.json')) as unknown as DeletePayload
  };
};

// ── Create Scenarios ──

/** POST with Prefer: return=representation. Validates 201 response, OData annotations, and follow-up GET. */
const runCreateSucceedsRepresentation = async (
  config: TestConfig,
  authToken: string,
  payload: Record<string, unknown>,
  entityType: EntityType
): Promise<ScenarioResult> => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];

  assertions.push(makeSchemaAssertion(payload, entityType));

  const url = buildResourceUrl(config.serverUrl, config.resource);
  const response = await odataRequest({
    method: 'POST',
    url,
    body: payload,
    authToken,
    headers: { Prefer: 'return=representation' }
  });

  assertions.push(validateStatusCode(response, [201, 204]));
  assertions.push(validateODataVersionHeader(response));
  assertions.push(validateEntityIdHeader(response));
  assertions.push(...validateLocationHeader(response, config.resource));
  assertions.push(validatePreferenceApplied(response, 'return=representation'));

  if (response.status !== 204) {
    assertions.push(validateJsonResponse(response));
    assertions.push(...validateODataAnnotation(response, '@odata.context', 'MAY'));
    assertions.push(...validateODataAnnotation(response, '@odata.id', 'MAY'));
    assertions.push(...validateODataAnnotation(response, '@odata.editLink', 'MUST'));
    assertions.push(...validateResponseContainsPayload(response.body, payload));
  }

  // Follow-up GET
  const locationUrl = response.headers.location;
  if (locationUrl) {
    const getResponse = await odataRequest({
      method: 'GET',
      url: locationUrl,
      authToken
    });
    assertions.push(validateStatusCode(getResponse, [200]));
    assertions.push(validateODataVersionHeader(getResponse));
    assertions.push(...validateResponseContainsPayload(getResponse.body, payload));
  }

  return buildScenarioResult(
    'create-succeeds-representation',
    ['@create', '@create-succeeds', '@add-edit-endorsement', '@rcp-010', '@2.1.0', '@return-representation'],
    assertions,
    start
  );
};

/** POST with Prefer: return=minimal. Validates 204 response with headers only, and follow-up GET. */
const runCreateSucceedsMinimal = async (
  config: TestConfig,
  authToken: string,
  payload: Record<string, unknown>,
  entityType: EntityType
): Promise<ScenarioResult> => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];

  assertions.push(makeSchemaAssertion(payload, entityType));

  const url = buildResourceUrl(config.serverUrl, config.resource);
  const response = await odataRequest({
    method: 'POST',
    url,
    body: payload,
    authToken,
    headers: { Prefer: 'return=minimal' }
  });

  assertions.push(validateStatusCode(response, [201, 204]));
  assertions.push(validateODataVersionHeader(response));
  assertions.push(validateEntityIdHeader(response));
  assertions.push(...validateLocationHeader(response, config.resource));
  assertions.push(validatePreferenceApplied(response, 'return=minimal'));

  // Follow-up GET
  const locationUrl = response.headers.location;
  if (locationUrl) {
    const getResponse = await odataRequest({
      method: 'GET',
      url: locationUrl,
      authToken
    });
    assertions.push(validateStatusCode(getResponse, [200]));
    assertions.push(validateODataVersionHeader(getResponse));
    assertions.push(...validateResponseContainsPayload(getResponse.body, payload));
  }

  return buildScenarioResult(
    'create-succeeds-minimal',
    ['@create', '@create-succeeds', '@add-edit-endorsement', '@rcp-010', '@2.1.0', '@return-minimal'],
    assertions,
    start
  );
};

/** POST with known-bad payload. Validates 400 response and OData error format with field-level details. */
const runCreateFails = async (
  config: TestConfig,
  authToken: string,
  payload: Record<string, unknown>,
  entityType: EntityType
): Promise<ScenarioResult> => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];

  assertions.push(makeSchemaAssertion(payload, entityType));

  const url = buildResourceUrl(config.serverUrl, config.resource);
  const response = await odataRequest({
    method: 'POST',
    url,
    body: payload,
    authToken,
    headers: { Prefer: 'return=representation' }
  });

  assertions.push(validateStatusCode(response, [400]));
  assertions.push(validateODataVersionHeader(response));
  assertions.push(...validateODataError(response, entityType));

  return buildScenarioResult(
    'create-fails',
    ['@create', '@create-fails', '@add-edit-endorsement', '@rcp-010', '@1.0.2'],
    assertions,
    start
  );
};

// ── Update Scenarios ──

/** PATCH with Prefer: return=representation. Validates 200 response, @odata.etag, and follow-up GET. */
const runUpdateSucceedsRepresentation = async (
  config: TestConfig,
  authToken: string,
  payload: Record<string, unknown>,
  entityType: EntityType
): Promise<ScenarioResult> => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];
  const targetKey = extractPrimaryKey(payload, entityType);
  const sendPayload = stripPrimaryKey(payload, entityType);

  assertions.push(makeSchemaAssertion(payload, entityType));

  const url = buildResourceUrl(config.serverUrl, config.resource, targetKey);
  const response = await odataRequest({
    method: 'PATCH',
    url,
    body: sendPayload,
    authToken,
    headers: { Prefer: 'return=representation' }
  });

  assertions.push(validateStatusCode(response, [200, 204]));
  assertions.push(validateODataVersionHeader(response));
  assertions.push(validateEntityIdHeader(response));
  assertions.push(...validateLocationHeader(response, config.resource));
  assertions.push(validatePreferenceApplied(response, 'return=representation'));

  if (response.status !== 204) {
    assertions.push(validateJsonResponse(response));
    assertions.push(...validateODataAnnotation(response, '@odata.etag', 'MUST'));
    assertions.push(...validateODataAnnotation(response, '@odata.context', 'MAY'));
    assertions.push(...validateODataAnnotation(response, '@odata.id', 'MAY'));
    assertions.push(...validateODataAnnotation(response, '@odata.editLink', 'MUST'));
    assertions.push(...validateResponseContainsPayload(response.body, sendPayload));
  }

  // Follow-up GET
  const locationUrl = response.headers.location;
  if (locationUrl) {
    const getResponse = await odataRequest({
      method: 'GET',
      url: locationUrl,
      authToken
    });
    assertions.push(validateStatusCode(getResponse, [200]));
    assertions.push(validateODataVersionHeader(getResponse));
    assertions.push(...validateResponseContainsPayload(getResponse.body, sendPayload));
  }

  return buildScenarioResult(
    'update-succeeds-representation',
    ['@update', '@update-succeeds', '@add-edit-endorsement', '@rcp-010', '@2.0.0', '@return-representation'],
    assertions,
    start
  );
};

/** PATCH with Prefer: return=minimal. Validates 204 response with headers only, and follow-up GET. */
const runUpdateSucceedsMinimal = async (
  config: TestConfig,
  authToken: string,
  payload: Record<string, unknown>,
  entityType: EntityType
): Promise<ScenarioResult> => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];
  const targetKey = extractPrimaryKey(payload, entityType);
  const sendPayload = stripPrimaryKey(payload, entityType);

  assertions.push(makeSchemaAssertion(payload, entityType));

  const url = buildResourceUrl(config.serverUrl, config.resource, targetKey);
  const response = await odataRequest({
    method: 'PATCH',
    url,
    body: sendPayload,
    authToken,
    headers: { Prefer: 'return=minimal' }
  });

  assertions.push(validateStatusCode(response, [200, 204]));
  assertions.push(validateODataVersionHeader(response));
  assertions.push(validateEntityIdHeader(response));
  assertions.push(...validateLocationHeader(response, config.resource));

  // Follow-up GET
  const locationUrl = response.headers.location;
  if (locationUrl) {
    const getResponse = await odataRequest({
      method: 'GET',
      url: locationUrl,
      authToken
    });
    assertions.push(validateStatusCode(getResponse, [200]));
    assertions.push(validateODataVersionHeader(getResponse));
    assertions.push(...validateResponseContainsPayload(getResponse.body, sendPayload));
  }

  return buildScenarioResult(
    'update-succeeds-minimal',
    ['@update', '@update-succeeds', '@add-edit-endorsement', '@rcp-010', '@2.0.0', '@return-minimal'],
    assertions,
    start
  );
};

/** PATCH with known-bad payload. Validates 400 response and OData error format with field-level details. */
const runUpdateFails = async (
  config: TestConfig,
  authToken: string,
  payload: Record<string, unknown>,
  entityType: EntityType
): Promise<ScenarioResult> => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];
  const targetKey = extractPrimaryKey(payload, entityType);
  const sendPayload = stripPrimaryKey(payload, entityType);

  assertions.push(makeSchemaAssertion(payload, entityType));

  const url = buildResourceUrl(config.serverUrl, config.resource, targetKey);
  const response = await odataRequest({
    method: 'PATCH',
    url,
    body: sendPayload,
    authToken,
    headers: { Prefer: 'return=representation' }
  });

  assertions.push(validateStatusCode(response, [400]));
  assertions.push(validateODataVersionHeader(response));
  assertions.push(...validateODataError(response, entityType));

  return buildScenarioResult(
    'update-fails',
    ['@update', '@update-fails', '@add-edit-endorsement', '@rcp-010', '@1.0.2'],
    assertions,
    start
  );
};

// ── Delete Scenarios ──

/**
 * DELETE to a known-good resource URL. Validates 204 response, then performs
 * a follow-up GET to confirm the resource returns 404 (actually deleted).
 * The delete URL is built from either the payload's `url` field or its `id` + the resource name.
 */
const runDeleteSucceeds = async (
  config: TestConfig,
  authToken: string,
  deletePayload: DeletePayload,
  entityType: EntityType
): Promise<ScenarioResult> => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];

  // First create a record so we have something to delete
  // (For mock server testing, the delete-succeeds.json id won't exist yet)
  const deleteUrl = deletePayload.url
    ? `${config.serverUrl}/${deletePayload.url}`
    : buildResourceUrl(config.serverUrl, config.resource, deletePayload.id);

  const response = await odataRequest({
    method: 'DELETE',
    url: deleteUrl,
    authToken
  });

  assertions.push(validateStatusCode(response, [204]));
  assertions.push(validateODataVersionHeader(response));

  // Follow-up GET should return 404
  const getResponse = await odataRequest({
    method: 'GET',
    url: deleteUrl,
    authToken
  });
  assertions.push(validateStatusCode(getResponse, [404]));
  assertions.push(validateODataVersionHeader(getResponse));

  return buildScenarioResult(
    'delete-succeeds',
    ['@delete', '@delete-succeeds', '@add-edit-endorsement', '@rcp-010', '@2.0.0'],
    assertions,
    start
  );
};

/**
 * DELETE to a non-existent resource URL. Validates that the server returns
 * a 4xx status code (400-499) and the OData-Version header.
 */
const runDeleteFails = async (config: TestConfig, authToken: string, deletePayload: DeletePayload): Promise<ScenarioResult> => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];

  const deleteUrl = deletePayload.url
    ? `${config.serverUrl}/${deletePayload.url}`
    : buildResourceUrl(config.serverUrl, config.resource, deletePayload.id);

  const response = await odataRequest({
    method: 'DELETE',
    url: deleteUrl,
    authToken
  });

  assertions.push(validateStatusCodeRange(response, 400, 499));
  assertions.push(validateODataVersionHeader(response));

  return buildScenarioResult(
    'delete-fails',
    ['@delete', '@delete-fails', '@add-edit-endorsement', '@rcp-010', '@2.0.0'],
    assertions,
    start
  );
};
