import { readFile } from "node:fs/promises";
import { join } from "node:path";
import type {
  TestConfig,
  PayloadSet,
  DeletePayload,
  ScenarioResult,
  ScenarioName,
  TestReport,
  TestAssertion,
  EntityType,
} from "./types.js";
import { odataRequest, buildResourceUrl } from "./client.js";
import { resolveAuthToken } from "./auth.js";
import {
  fetchMetadata,
  loadMetadataFromFile,
  parseMetadataXml,
  getEntityType,
  validatePayloadAgainstMetadata,
} from "./metadata.js";
import * as V from "./validators.js";

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
export async function runAllScenarios(
  config: TestConfig,
): Promise<TestReport> {
  const authToken = await resolveAuthToken(config.auth);

  const metadataXml = config.metadataPath
    ? await loadMetadataFromFile(config.metadataPath)
    : await fetchMetadata(config.serverUrl, authToken);

  const metadata = parseMetadataXml(metadataXml);
  const entityType = getEntityType(metadata, config.resource);

  if (!entityType) {
    throw new Error(
      `Entity type "${config.resource}" not found in metadata. Available: ${metadata.entityTypes.map((et) => et.name).join(", ")}`,
    );
  }

  const payloads = await loadPayloads(config.payloadsDir);

  const scenarios: ScenarioResult[] = [];

  scenarios.push(
    await runCreateSucceedsRepresentation(config, authToken, payloads.createSucceeds, entityType),
  );
  scenarios.push(
    await runCreateSucceedsMinimal(config, authToken, payloads.createSucceeds, entityType),
  );
  scenarios.push(
    await runCreateFails(config, authToken, payloads.createFails, entityType),
  );
  scenarios.push(
    await runUpdateSucceedsRepresentation(config, authToken, payloads.updateSucceeds, entityType),
  );
  scenarios.push(
    await runUpdateSucceedsMinimal(config, authToken, payloads.updateSucceeds, entityType),
  );
  scenarios.push(
    await runUpdateFails(config, authToken, payloads.updateFails, entityType),
  );
  scenarios.push(
    await runDeleteSucceeds(config, authToken, payloads.deleteSucceeds, entityType),
  );
  scenarios.push(
    await runDeleteFails(config, authToken, payloads.deleteFails),
  );

  const passed = scenarios.filter((s) => s.passed).length;
  const failed = scenarios.filter((s) => !s.passed).length;

  return {
    serverUrl: config.serverUrl,
    resource: config.resource,
    timestamp: new Date().toISOString(),
    scenarios,
    summary: {
      total: scenarios.length,
      passed,
      failed,
      skipped: 0,
    },
  };
}

// ── Payload Loading ──

/**
 * Loads all 6 payload JSON files from a directory:
 * create-succeeds.json, create-fails.json, update-succeeds.json,
 * update-fails.json, delete-succeeds.json, delete-fails.json
 */
async function loadPayloads(payloadsDir: string): Promise<PayloadSet> {
  const loadJson = async (filename: string): Promise<Record<string, unknown>> => {
    const content = await readFile(join(payloadsDir, filename), "utf-8");
    return JSON.parse(content) as Record<string, unknown>;
  };

  return {
    createSucceeds: await loadJson("create-succeeds.json"),
    createFails: await loadJson("create-fails.json"),
    updateSucceeds: await loadJson("update-succeeds.json"),
    updateFails: await loadJson("update-fails.json"),
    deleteSucceeds: (await loadJson("delete-succeeds.json")) as unknown as DeletePayload,
    deleteFails: (await loadJson("delete-fails.json")) as unknown as DeletePayload,
  };
}

// ── Helpers ──

/**
 * Extracts the primary key value from a payload using the entity type's key
 * property definition. For example, if the entity type has key "ListingKey"
 * and the payload contains { "ListingKey": "12345", "ListPrice": 100 },
 * this returns "12345".
 */
const extractPrimaryKey = (
  payload: Readonly<Record<string, unknown>>,
  entityType: EntityType,
): string | undefined => {
  if (entityType.keyProperties.length === 0) return undefined;
  const keyProp = entityType.keyProperties[0];
  const value = payload[keyProp];
  return value !== undefined ? String(value) : undefined;
};

/**
 * Returns a copy of the payload without the primary key field.
 * The key is used in the URL for PATCH/DELETE, not in the request body.
 */
const stripPrimaryKey = (
  payload: Readonly<Record<string, unknown>>,
  entityType: EntityType,
): Record<string, unknown> => {
  const keyProps = new Set(entityType.keyProperties);
  return Object.fromEntries(
    Object.entries(payload).filter(([key]) => !keyProps.has(key)),
  );
};

/** Creates a test assertion that validates the payload fields exist in the entity type metadata. */
const makeSchemaAssertion = (
  payload: Record<string, unknown>,
  entityType: EntityType,
): TestAssertion => {
  const check = validatePayloadAgainstMetadata(payload, entityType);
  return {
    description: "Payload schema matches metadata",
    status: check.valid ? "pass" : "fail",
    expected: "(all fields in metadata)",
    actual: check.valid
      ? "(all fields valid)"
      : `Unknown fields: ${check.unknownFields.join(", ")}`,
    gherkinStep: "schema in payload matches the metadata",
  };
};

/** Constructs a ScenarioResult from collected assertions. A scenario passes if all assertions pass, skip, or warn. */
const buildScenarioResult = (
  scenario: ScenarioName,
  tags: ReadonlyArray<string>,
  assertions: ReadonlyArray<TestAssertion>,
  start: number,
): ScenarioResult => ({
  scenario,
  tags,
  assertions,
  passed: assertions.every((a) => a.status === "pass" || a.status === "skip" || a.status === "warn"),
  duration: Date.now() - start,
});

// ── Create Scenarios ──

/** POST with Prefer: return=representation. Validates 201 response, OData annotations, and follow-up GET. */
async function runCreateSucceedsRepresentation(
  config: TestConfig,
  authToken: string,
  payload: Record<string, unknown>,
  entityType: EntityType,
): Promise<ScenarioResult> {
  const start = Date.now();
  const assertions: TestAssertion[] = [];

  assertions.push(makeSchemaAssertion(payload, entityType));

  const url = buildResourceUrl(config.serverUrl, config.resource);
  const response = await odataRequest({
    method: "POST",
    url,
    body: payload,
    authToken,
    headers: { Prefer: "return=representation" },
  });

  assertions.push(V.validateStatusCode(response, [201, 204]));
  assertions.push(V.validateODataVersionHeader(response));
  assertions.push(V.validateEntityIdHeader(response));
  assertions.push(...V.validateLocationHeader(response, config.resource));
  assertions.push(V.validatePreferenceApplied(response, "return=representation"));

  if (response.status !== 204) {
    assertions.push(V.validateJsonResponse(response));
    assertions.push(...V.validateODataAnnotation(response, "@odata.context", "MAY"));
    assertions.push(...V.validateODataAnnotation(response, "@odata.id", "MAY"));
    assertions.push(...V.validateODataAnnotation(response, "@odata.editLink", "MUST"));
    assertions.push(...V.validateResponseContainsPayload(response.body, payload));
  }

  // Follow-up GET
  const locationUrl = response.headers["location"];
  if (locationUrl) {
    const getResponse = await odataRequest({
      method: "GET",
      url: locationUrl,
      authToken,
    });
    assertions.push(V.validateStatusCode(getResponse, [200]));
    assertions.push(V.validateODataVersionHeader(getResponse));
    assertions.push(...V.validateResponseContainsPayload(getResponse.body, payload));
  }

  return buildScenarioResult(
    "create-succeeds-representation",
    ["@create", "@create-succeeds", "@add-edit-endorsement", "@rcp-010", "@2.1.0", "@return-representation"],
    assertions,
    start,
  );
}

/** POST with Prefer: return=minimal. Validates 204 response with headers only, and follow-up GET. */
async function runCreateSucceedsMinimal(
  config: TestConfig,
  authToken: string,
  payload: Record<string, unknown>,
  entityType: EntityType,
): Promise<ScenarioResult> {
  const start = Date.now();
  const assertions: TestAssertion[] = [];

  assertions.push(makeSchemaAssertion(payload, entityType));

  const url = buildResourceUrl(config.serverUrl, config.resource);
  const response = await odataRequest({
    method: "POST",
    url,
    body: payload,
    authToken,
    headers: { Prefer: "return=minimal" },
  });

  assertions.push(V.validateStatusCode(response, [201, 204]));
  assertions.push(V.validateODataVersionHeader(response));
  assertions.push(V.validateEntityIdHeader(response));
  assertions.push(...V.validateLocationHeader(response, config.resource));
  assertions.push(V.validatePreferenceApplied(response, "return=minimal"));

  // Follow-up GET
  const locationUrl = response.headers["location"];
  if (locationUrl) {
    const getResponse = await odataRequest({
      method: "GET",
      url: locationUrl,
      authToken,
    });
    assertions.push(V.validateStatusCode(getResponse, [200]));
    assertions.push(V.validateODataVersionHeader(getResponse));
    assertions.push(...V.validateResponseContainsPayload(getResponse.body, payload));
  }

  return buildScenarioResult(
    "create-succeeds-minimal",
    ["@create", "@create-succeeds", "@add-edit-endorsement", "@rcp-010", "@2.1.0", "@return-minimal"],
    assertions,
    start,
  );
}

/** POST with known-bad payload. Validates 400 response and OData error format with field-level details. */
async function runCreateFails(
  config: TestConfig,
  authToken: string,
  payload: Record<string, unknown>,
  entityType: EntityType,
): Promise<ScenarioResult> {
  const start = Date.now();
  const assertions: TestAssertion[] = [];

  assertions.push(makeSchemaAssertion(payload, entityType));

  const url = buildResourceUrl(config.serverUrl, config.resource);
  const response = await odataRequest({
    method: "POST",
    url,
    body: payload,
    authToken,
    headers: { Prefer: "return=representation" },
  });

  assertions.push(V.validateStatusCode(response, [400]));
  assertions.push(V.validateODataVersionHeader(response));
  assertions.push(...V.validateODataError(response, entityType));

  return buildScenarioResult(
    "create-fails",
    ["@create", "@create-fails", "@add-edit-endorsement", "@rcp-010", "@1.0.2"],
    assertions,
    start,
  );
}

// ── Update Scenarios ──

/** PATCH with Prefer: return=representation. Validates 200 response, @odata.etag, and follow-up GET. */
async function runUpdateSucceedsRepresentation(
  config: TestConfig,
  authToken: string,
  payload: Record<string, unknown>,
  entityType: EntityType,
): Promise<ScenarioResult> {
  const start = Date.now();
  const assertions: TestAssertion[] = [];
  const targetKey = extractPrimaryKey(payload, entityType);
  const sendPayload = stripPrimaryKey(payload, entityType);

  assertions.push(makeSchemaAssertion(payload, entityType));

  const url = buildResourceUrl(config.serverUrl, config.resource, targetKey);
  const response = await odataRequest({
    method: "PATCH",
    url,
    body: sendPayload,
    authToken,
    headers: { Prefer: "return=representation" },
  });

  assertions.push(V.validateStatusCode(response, [200, 204]));
  assertions.push(V.validateODataVersionHeader(response));
  assertions.push(V.validateEntityIdHeader(response));
  assertions.push(...V.validateLocationHeader(response, config.resource));
  assertions.push(V.validatePreferenceApplied(response, "return=representation"));

  if (response.status !== 204) {
    assertions.push(V.validateJsonResponse(response));
    assertions.push(...V.validateODataAnnotation(response, "@odata.etag", "MUST"));
    assertions.push(...V.validateODataAnnotation(response, "@odata.context", "MAY"));
    assertions.push(...V.validateODataAnnotation(response, "@odata.id", "MAY"));
    assertions.push(...V.validateODataAnnotation(response, "@odata.editLink", "MUST"));
    assertions.push(...V.validateResponseContainsPayload(response.body, sendPayload));
  }

  // Follow-up GET
  const locationUrl = response.headers["location"];
  if (locationUrl) {
    const getResponse = await odataRequest({
      method: "GET",
      url: locationUrl,
      authToken,
    });
    assertions.push(V.validateStatusCode(getResponse, [200]));
    assertions.push(V.validateODataVersionHeader(getResponse));
    assertions.push(...V.validateResponseContainsPayload(getResponse.body, sendPayload));
  }

  return buildScenarioResult(
    "update-succeeds-representation",
    ["@update", "@update-succeeds", "@add-edit-endorsement", "@rcp-010", "@2.0.0", "@return-representation"],
    assertions,
    start,
  );
}

/** PATCH with Prefer: return=minimal. Validates 204 response with headers only, and follow-up GET. */
async function runUpdateSucceedsMinimal(
  config: TestConfig,
  authToken: string,
  payload: Record<string, unknown>,
  entityType: EntityType,
): Promise<ScenarioResult> {
  const start = Date.now();
  const assertions: TestAssertion[] = [];
  const targetKey = extractPrimaryKey(payload, entityType);
  const sendPayload = stripPrimaryKey(payload, entityType);

  assertions.push(makeSchemaAssertion(payload, entityType));

  const url = buildResourceUrl(config.serverUrl, config.resource, targetKey);
  const response = await odataRequest({
    method: "PATCH",
    url,
    body: sendPayload,
    authToken,
    headers: { Prefer: "return=minimal" },
  });

  assertions.push(V.validateStatusCode(response, [200, 204]));
  assertions.push(V.validateODataVersionHeader(response));
  assertions.push(V.validateEntityIdHeader(response));
  assertions.push(...V.validateLocationHeader(response, config.resource));

  // Follow-up GET
  const locationUrl = response.headers["location"];
  if (locationUrl) {
    const getResponse = await odataRequest({
      method: "GET",
      url: locationUrl,
      authToken,
    });
    assertions.push(V.validateStatusCode(getResponse, [200]));
    assertions.push(V.validateODataVersionHeader(getResponse));
    assertions.push(...V.validateResponseContainsPayload(getResponse.body, sendPayload));
  }

  return buildScenarioResult(
    "update-succeeds-minimal",
    ["@update", "@update-succeeds", "@add-edit-endorsement", "@rcp-010", "@2.0.0", "@return-minimal"],
    assertions,
    start,
  );
}

/** PATCH with known-bad payload. Validates 400 response and OData error format with field-level details. */
async function runUpdateFails(
  config: TestConfig,
  authToken: string,
  payload: Record<string, unknown>,
  entityType: EntityType,
): Promise<ScenarioResult> {
  const start = Date.now();
  const assertions: TestAssertion[] = [];
  const targetKey = extractPrimaryKey(payload, entityType);
  const sendPayload = stripPrimaryKey(payload, entityType);

  assertions.push(makeSchemaAssertion(payload, entityType));

  const url = buildResourceUrl(config.serverUrl, config.resource, targetKey);
  const response = await odataRequest({
    method: "PATCH",
    url,
    body: sendPayload,
    authToken,
    headers: { Prefer: "return=representation" },
  });

  assertions.push(V.validateStatusCode(response, [400]));
  assertions.push(V.validateODataVersionHeader(response));
  assertions.push(...V.validateODataError(response, entityType));

  return buildScenarioResult(
    "update-fails",
    ["@update", "@update-fails", "@add-edit-endorsement", "@rcp-010", "@1.0.2"],
    assertions,
    start,
  );
}

// ── Delete Scenarios ──

/**
 * DELETE to a known-good resource URL. Validates 204 response, then performs
 * a follow-up GET to confirm the resource returns 404 (actually deleted).
 * The delete URL is built from either the payload's `url` field or its `id` + the resource name.
 */
async function runDeleteSucceeds(
  config: TestConfig,
  authToken: string,
  deletePayload: DeletePayload,
  entityType: EntityType,
): Promise<ScenarioResult> {
  const start = Date.now();
  const assertions: TestAssertion[] = [];

  // First create a record so we have something to delete
  // (For mock server testing, the delete-succeeds.json id won't exist yet)
  const deleteUrl = deletePayload.url
    ? `${config.serverUrl}/${deletePayload.url}`
    : buildResourceUrl(config.serverUrl, config.resource, deletePayload.id);

  const response = await odataRequest({
    method: "DELETE",
    url: deleteUrl,
    authToken,
  });

  assertions.push(V.validateStatusCode(response, [204]));
  assertions.push(V.validateODataVersionHeader(response));

  // Follow-up GET should return 404
  const getResponse = await odataRequest({
    method: "GET",
    url: deleteUrl,
    authToken,
  });
  assertions.push(V.validateStatusCode(getResponse, [404]));
  assertions.push(V.validateODataVersionHeader(getResponse));

  return buildScenarioResult(
    "delete-succeeds",
    ["@delete", "@delete-succeeds", "@add-edit-endorsement", "@rcp-010", "@2.0.0"],
    assertions,
    start,
  );
}

/**
 * DELETE to a non-existent resource URL. Validates that the server returns
 * a 4xx status code (400–499) and the OData-Version header.
 */
async function runDeleteFails(
  config: TestConfig,
  authToken: string,
  deletePayload: DeletePayload,
): Promise<ScenarioResult> {
  const start = Date.now();
  const assertions: TestAssertion[] = [];

  const deleteUrl = deletePayload.url
    ? `${config.serverUrl}/${deletePayload.url}`
    : buildResourceUrl(config.serverUrl, config.resource, deletePayload.id);

  const response = await odataRequest({
    method: "DELETE",
    url: deleteUrl,
    authToken,
  });

  assertions.push(V.validateStatusCodeRange(response, 400, 499));
  assertions.push(V.validateODataVersionHeader(response));

  return buildScenarioResult(
    "delete-fails",
    ["@delete", "@delete-fails", "@add-edit-endorsement", "@rcp-010", "@2.0.0"],
    assertions,
    start,
  );
}
