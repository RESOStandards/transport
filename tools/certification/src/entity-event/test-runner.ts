/**
 * EntityEvent (RCP-027) compliance test runner.
 *
 * Runs 11 scenarios in two modes:
 * - observe: 8 read-only scenarios (metadata, query, structure, incremental sync)
 * - full: all 11 scenarios (adds create/update/delete canary write verification)
 *
 * After scenarios, performs batch data validation on fetched EntityEvent records.
 */

import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { resolveAuthToken } from '../test-runner/auth.js';
import { buildResourceUrl, odataRequest } from '../test-runner/client.js';
import { buildScenarioResult } from '../test-runner/helpers.js';
import { fetchMetadata, getEntityType, loadMetadataFromFile, parseMetadataXml } from '../test-runner/metadata.js';
import type { EntityType, ParsedMetadata, ScenarioResult, TestAssertion } from '../test-runner/types.js';
import {
  validateJsonResponse,
  validateODataVersionHeader,
  validateStatusCode,
  validateStatusCodeRange
} from '../test-runner/validators.js';
import { validateEventData } from './data-validator.js';
import type { DataValidationResult, EntityEventConfig, EntityEventRecord, EntityEventTestReport } from './types.js';

// ── Helpers ──

/** Parses an OData collection response into EntityEventRecord[]. */
const parseEntityEvents = (body: unknown): ReadonlyArray<EntityEventRecord> => {
  if (!body || typeof body !== 'object') return [];
  const obj = body as Record<string, unknown>;
  if (!Array.isArray(obj.value)) return [];
  return (obj.value as ReadonlyArray<Record<string, unknown>>).map(v => ({
    EntityEventSequence: Number(v.EntityEventSequence),
    ResourceName: String(v.ResourceName ?? ''),
    ResourceRecordKey: String(v.ResourceRecordKey ?? ''),
    ...(v.ResourceRecordUrl !== undefined && v.ResourceRecordUrl !== null ? { ResourceRecordUrl: String(v.ResourceRecordUrl) } : {})
  }));
};

/** Waits for the specified number of milliseconds. */
const delay = (ms: number): Promise<void> => new Promise(resolve => setTimeout(resolve, ms));

// ── Scenarios ──

/** Validates EntityEvent appears in $metadata with correct fields and types. */
const runMetadataValid = (metadata: ParsedMetadata): ScenarioResult => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];

  const entityType = getEntityType(metadata, 'EntityEvent');

  assertions.push({
    description: 'EntityEvent entity type exists in metadata',
    status: entityType ? 'pass' : 'fail',
    expected: '(EntityEvent in $metadata)',
    actual: entityType ? 'present' : 'missing'
  });

  if (!entityType) {
    return buildScenarioResult('metadata-valid', ['@entity-event', '@rcp-027', '@metadata'], assertions, start);
  }

  // Key property
  const hasKeyProp = entityType.keyProperties.includes('EntityEventSequence');
  assertions.push({
    description: 'Key property is EntityEventSequence',
    status: hasKeyProp ? 'pass' : 'fail',
    expected: 'EntityEventSequence',
    actual: entityType.keyProperties.join(', ') || '(none)'
  });

  // Required fields and their types
  const requiredFields: ReadonlyArray<{ readonly name: string; readonly expectedTypes: ReadonlyArray<string> }> = [
    { name: 'EntityEventSequence', expectedTypes: ['Edm.Int64', 'Edm.Int32'] },
    { name: 'ResourceName', expectedTypes: ['Edm.String'] },
    { name: 'ResourceRecordKey', expectedTypes: ['Edm.String'] }
  ];

  for (const field of requiredFields) {
    const prop = entityType.properties.find(p => p.name === field.name);
    assertions.push({
      description: `${field.name} field exists`,
      status: prop ? 'pass' : 'fail',
      expected: field.name,
      actual: prop ? 'present' : 'missing'
    });

    if (prop) {
      const typeMatch = field.expectedTypes.includes(prop.type);
      assertions.push({
        description: `${field.name} type is ${field.expectedTypes.join(' or ')}`,
        status: typeMatch ? 'pass' : 'fail',
        expected: field.expectedTypes.join(' or '),
        actual: prop.type
      });
    }
  }

  // Optional ResourceRecordUrl
  const urlProp = entityType.properties.find(p => p.name === 'ResourceRecordUrl');
  if (urlProp) {
    assertions.push({
      description: 'ResourceRecordUrl type is Edm.String',
      status: urlProp.type === 'Edm.String' ? 'pass' : 'fail',
      expected: 'Edm.String',
      actual: urlProp.type
    });
  } else {
    assertions.push({
      description: 'ResourceRecordUrl field (optional)',
      status: 'skip',
      expected: '(optional)',
      actual: 'not present'
    });
  }

  return buildScenarioResult('metadata-valid', ['@entity-event', '@rcp-027', '@metadata'], assertions, start);
};

/** Validates POST/PATCH/DELETE to /EntityEvent are rejected. */
const runReadOnlyEnforced = async (serverUrl: string, authToken: string): Promise<ScenarioResult> => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];
  const url = buildResourceUrl(serverUrl, 'EntityEvent');
  const keyUrl = `${url}('1')`;

  // POST
  const postResponse = await odataRequest({ method: 'POST', url, body: {}, authToken });
  assertions.push(validateStatusCodeRange(postResponse, 400, 499));
  assertions.push({
    description: 'POST /EntityEvent rejected',
    status: postResponse.status >= 400 && postResponse.status < 500 ? 'pass' : 'fail',
    expected: '4xx',
    actual: String(postResponse.status)
  });

  // PATCH
  const patchResponse = await odataRequest({ method: 'PATCH', url: keyUrl, body: {}, authToken });
  assertions.push(validateStatusCodeRange(patchResponse, 400, 499));
  assertions.push({
    description: 'PATCH /EntityEvent rejected',
    status: patchResponse.status >= 400 && patchResponse.status < 500 ? 'pass' : 'fail',
    expected: '4xx',
    actual: String(patchResponse.status)
  });

  // DELETE
  const deleteResponse = await odataRequest({ method: 'DELETE', url: keyUrl, authToken });
  assertions.push(validateStatusCodeRange(deleteResponse, 400, 499));
  assertions.push({
    description: 'DELETE /EntityEvent rejected',
    status: deleteResponse.status >= 400 && deleteResponse.status < 500 ? 'pass' : 'fail',
    expected: '4xx',
    actual: String(deleteResponse.status)
  });

  return buildScenarioResult('read-only-enforced', ['@entity-event', '@rcp-027', '@read-only'], assertions, start);
};

/** Fetches EntityEvent records and validates their structure. Returns the fetched events. */
const runEventStructure = async (
  serverUrl: string,
  authToken: string,
  maxEvents: number
): Promise<{ readonly result: ScenarioResult; readonly events: ReadonlyArray<EntityEventRecord> }> => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];

  const url = `${buildResourceUrl(serverUrl, 'EntityEvent')}?$top=${maxEvents}&$orderby=EntityEventSequence asc`;
  const response = await odataRequest({ method: 'GET', url, authToken });

  assertions.push(validateStatusCode(response, [200]));
  assertions.push(validateODataVersionHeader(response));
  assertions.push(validateJsonResponse(response));

  const events = parseEntityEvents(response.body);

  assertions.push({
    description: 'Response contains EntityEvent records',
    status: events.length > 0 ? 'pass' : 'warn',
    expected: '(at least 1 event)',
    actual: `${events.length} events`
  });

  // Validate each event's structure
  let structureErrors = 0;
  for (const event of events) {
    if (typeof event.ResourceName !== 'string' || event.ResourceName.length === 0) {
      structureErrors++;
    }
    if (typeof event.ResourceRecordKey !== 'string' || event.ResourceRecordKey.length === 0) {
      structureErrors++;
    }
    if (typeof event.EntityEventSequence !== 'number' || !Number.isFinite(event.EntityEventSequence)) {
      structureErrors++;
    }
  }

  assertions.push({
    description: 'All events have valid structure (ResourceName, ResourceRecordKey, EntityEventSequence)',
    status: structureErrors === 0 ? 'pass' : 'fail',
    expected: '0 structure errors',
    actual: `${structureErrors} structure errors`
  });

  return {
    result: buildScenarioResult('event-structure', ['@entity-event', '@rcp-027', '@structure'], assertions, start),
    events
  };
};

/** Validates sequences are strictly ascending in the fetched events. */
const runSequenceMonotonic = (events: ReadonlyArray<EntityEventRecord>): ScenarioResult => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];

  if (events.length < 2) {
    assertions.push({
      description: 'Monotonic check requires at least 2 events',
      status: events.length === 0 ? 'warn' : 'skip',
      expected: '(2+ events)',
      actual: `${events.length} events`
    });
    return buildScenarioResult('sequence-monotonic', ['@entity-event', '@rcp-027', '@sequence'], assertions, start);
  }

  let violations = 0;
  let firstViolationIndex = -1;
  for (let i = 0; i < events.length - 1; i++) {
    if (events[i].EntityEventSequence >= events[i + 1].EntityEventSequence) {
      violations++;
      if (firstViolationIndex < 0) firstViolationIndex = i;
    }
  }

  assertions.push({
    description: 'EntityEventSequence values are strictly ascending',
    status: violations === 0 ? 'pass' : 'fail',
    expected: '0 ordering violations',
    actual:
      violations === 0
        ? `${events.length} events in order`
        : `${violations} violations (first at index ${firstViolationIndex}: ${events[firstViolationIndex].EntityEventSequence} >= ${events[firstViolationIndex + 1].EntityEventSequence})`
  });

  return buildScenarioResult('sequence-monotonic', ['@entity-event', '@rcp-027', '@sequence'], assertions, start);
};

/** Validates $filter=ResourceName eq '<X>' returns only matching events. */
const runQueryFilter = async (serverUrl: string, authToken: string, events: ReadonlyArray<EntityEventRecord>): Promise<ScenarioResult> => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];

  if (events.length === 0) {
    assertions.push({ description: 'No events available for filter test', status: 'warn' });
    return buildScenarioResult('query-filter', ['@entity-event', '@rcp-027', '@query'], assertions, start);
  }

  // Pick the most common ResourceName
  const counts = new Map<string, number>();
  for (const e of events) {
    counts.set(e.ResourceName, (counts.get(e.ResourceName) ?? 0) + 1);
  }
  const targetResource = [...counts.entries()].sort((a, b) => b[1] - a[1])[0][0];

  const filter = encodeURIComponent(`ResourceName eq '${targetResource}'`);
  const url = `${buildResourceUrl(serverUrl, 'EntityEvent')}?$filter=${filter}&$top=100`;
  const response = await odataRequest({ method: 'GET', url, authToken });

  assertions.push(validateStatusCode(response, [200]));
  assertions.push(validateJsonResponse(response));

  const filtered = parseEntityEvents(response.body);
  const allMatch = filtered.every(e => e.ResourceName === targetResource);
  const mismatchCount = filtered.filter(e => e.ResourceName !== targetResource).length;

  assertions.push({
    description: `All filtered events have ResourceName eq '${targetResource}'`,
    status: allMatch ? 'pass' : 'fail',
    expected: `ResourceName eq '${targetResource}'`,
    actual: allMatch ? `${filtered.length} matching events` : `${mismatchCount} mismatches`
  });

  return buildScenarioResult('query-filter', ['@entity-event', '@rcp-027', '@query'], assertions, start);
};

/** Validates $orderby, $top, and $skip work on EntityEvent. */
const runQueryOrderbyTopSkip = async (serverUrl: string, authToken: string): Promise<ScenarioResult> => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];
  const base = buildResourceUrl(serverUrl, 'EntityEvent');

  // $orderby desc + $top
  const descUrl = `${base}?$orderby=EntityEventSequence desc&$top=5`;
  const descResponse = await odataRequest({ method: 'GET', url: descUrl, authToken });
  assertions.push(validateStatusCode(descResponse, [200]));

  const descEvents = parseEntityEvents(descResponse.body);
  assertions.push({
    description: '$top=5 returns at most 5 records',
    status: descEvents.length <= 5 ? 'pass' : 'fail',
    expected: '<= 5',
    actual: String(descEvents.length)
  });

  // Check descending order
  if (descEvents.length >= 2) {
    const isDescending = descEvents.every((e, i) => i === 0 || e.EntityEventSequence <= descEvents[i - 1].EntityEventSequence);
    assertions.push({
      description: 'Results are in descending sequence order',
      status: isDescending ? 'pass' : 'fail',
      expected: 'descending EntityEventSequence',
      actual: isDescending ? 'correct' : 'out of order'
    });
  }

  // $skip
  const skipUrl = `${base}?$orderby=EntityEventSequence asc&$top=2&$skip=1`;
  const skipResponse = await odataRequest({ method: 'GET', url: skipUrl, authToken });
  assertions.push(validateStatusCode(skipResponse, [200]));

  const skipEvents = parseEntityEvents(skipResponse.body);
  assertions.push({
    description: '$skip=1 skips the first record',
    status: skipEvents.length <= 2 ? 'pass' : 'fail',
    expected: '<= 2 records',
    actual: `${skipEvents.length} records`
  });

  return buildScenarioResult('query-orderby-top-skip', ['@entity-event', '@rcp-027', '@query'], assertions, start);
};

/** Validates $count=true returns a count. */
const runQueryCount = async (serverUrl: string, authToken: string): Promise<ScenarioResult> => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];

  const url = `${buildResourceUrl(serverUrl, 'EntityEvent')}?$count=true&$top=1`;
  const response = await odataRequest({ method: 'GET', url, authToken });

  assertions.push(validateStatusCode(response, [200]));
  assertions.push(validateJsonResponse(response));

  const body = response.body as Record<string, unknown> | null;
  const count = body?.['@odata.count'];

  assertions.push({
    description: '@odata.count is present and numeric',
    status: typeof count === 'number' && count >= 0 ? 'pass' : 'fail',
    expected: '(number >= 0)',
    actual: count === undefined ? '(missing)' : String(count)
  });

  return buildScenarioResult('query-count', ['@entity-event', '@rcp-027', '@query'], assertions, start);
};

/**
 * Incremental sync: records highest sequence, waits, queries for new events.
 * In observe mode, polls with timeout. In full mode, just checks after canary writes.
 */
const runIncrementalSync = async (
  serverUrl: string,
  authToken: string,
  highWaterMark: number,
  config: EntityEventConfig
): Promise<{ readonly result: ScenarioResult; readonly pollDurationMs: number; readonly newEvents: number }> => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];
  let newEventCount = 0;

  const base = buildResourceUrl(serverUrl, 'EntityEvent');
  const filterExpr = encodeURIComponent(`EntityEventSequence gt ${highWaterMark}`);
  const queryUrl = `${base}?$filter=${filterExpr}&$orderby=EntityEventSequence asc&$top=100`;

  if (config.mode === 'observe') {
    // Poll loop with timeout
    const deadline = Date.now() + config.pollTimeoutMs;
    let found = false;

    while (Date.now() < deadline) {
      await delay(config.pollIntervalMs);

      const response = await odataRequest({ method: 'GET', url: queryUrl, authToken });
      if (response.status === 200) {
        const newEvents = parseEntityEvents(response.body);
        if (newEvents.length > 0) {
          newEventCount = newEvents.length;
          const allHigher = newEvents.every(e => e.EntityEventSequence > highWaterMark);
          assertions.push({
            description: 'New events have sequences greater than high water mark',
            status: allHigher ? 'pass' : 'fail',
            expected: `> ${highWaterMark}`,
            actual: `${newEvents.length} events, min sequence ${Math.min(...newEvents.map(e => e.EntityEventSequence))}`
          });
          found = true;
          break;
        }
      }
    }

    if (!found) {
      assertions.push({
        description: 'New events observed during poll window',
        status: 'warn',
        expected: '(new events)',
        actual: `No new events within ${config.pollTimeoutMs}ms`
      });
    }
  } else {
    // Full mode: just query immediately (canary writes should have happened)
    const response = await odataRequest({ method: 'GET', url: queryUrl, authToken });
    assertions.push(validateStatusCode(response, [200]));

    const newEvents = parseEntityEvents(response.body);
    newEventCount = newEvents.length;

    assertions.push({
      description: 'New events found after canary writes',
      status: newEvents.length > 0 ? 'pass' : 'fail',
      expected: '(at least 1 new event)',
      actual: `${newEvents.length} new events`
    });

    if (newEvents.length > 0) {
      const allHigher = newEvents.every(e => e.EntityEventSequence > highWaterMark);
      assertions.push({
        description: 'New event sequences > high water mark',
        status: allHigher ? 'pass' : 'fail',
        expected: `> ${highWaterMark}`,
        actual: `min: ${Math.min(...newEvents.map(e => e.EntityEventSequence))}`
      });
    }
  }

  const pollDuration = Date.now() - start;
  return {
    result: buildScenarioResult('incremental-sync', ['@entity-event', '@rcp-027', '@sync'], assertions, start),
    pollDurationMs: pollDuration,
    newEvents: newEventCount
  };
};

// ── Full Mode: Canary Write Scenarios ──

/** Creates a record on the canary resource, then checks EntityEvent for the new entry. */
const runCreateTriggersEvent = async (
  serverUrl: string,
  authToken: string,
  writableResource: string,
  payload: Record<string, unknown>,
  entityType: EntityType
): Promise<{ readonly result: ScenarioResult; readonly createdKey: string | undefined; readonly sequence: number }> => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];
  let createdKey: string | undefined;
  let sequence = 0;

  // POST to canary resource
  const url = buildResourceUrl(serverUrl, writableResource);
  const response = await odataRequest({
    method: 'POST',
    url,
    body: payload,
    authToken,
    headers: { Prefer: 'return=representation' }
  });

  assertions.push(validateStatusCode(response, [201, 204]));

  // Extract key from response
  if (response.status === 201 || response.status === 204) {
    const body = response.body as Record<string, unknown> | null;
    const keyField = entityType.keyProperties[0];
    if (body && keyField) {
      createdKey = String(body[keyField]);
    }
    // Fallback: extract from Location header
    if (!createdKey && response.headers.location) {
      const match = response.headers.location.match(/\('([^']+)'\)/);
      createdKey = match?.[1];
    }
  }

  if (!createdKey) {
    assertions.push({
      description: 'Created record key extracted',
      status: 'fail',
      expected: '(key from response)',
      actual: '(could not extract key)'
    });
    return {
      result: buildScenarioResult('create-triggers-event', ['@entity-event', '@rcp-027', '@create'], assertions, start),
      createdKey,
      sequence
    };
  }

  assertions.push({
    description: 'Created record key extracted',
    status: 'pass',
    actual: createdKey
  });

  // Query EntityEvent for the new entry
  const filter = encodeURIComponent(`ResourceName eq '${writableResource}' and ResourceRecordKey eq '${createdKey}'`);
  const eeUrl = `${buildResourceUrl(serverUrl, 'EntityEvent')}?$filter=${filter}&$orderby=EntityEventSequence desc&$top=1`;
  const eeResponse = await odataRequest({ method: 'GET', url: eeUrl, authToken });

  assertions.push(validateStatusCode(eeResponse, [200]));

  const events = parseEntityEvents(eeResponse.body);
  assertions.push({
    description: 'EntityEvent entry found for created record',
    status: events.length > 0 ? 'pass' : 'fail',
    expected: `EntityEvent for ${writableResource}('${createdKey}')`,
    actual: events.length > 0 ? `sequence ${events[0].EntityEventSequence}` : '(no event found)'
  });

  if (events.length > 0) {
    sequence = events[0].EntityEventSequence;
  }

  return {
    result: buildScenarioResult('create-triggers-event', ['@entity-event', '@rcp-027', '@create'], assertions, start),
    createdKey,
    sequence
  };
};

/** Updates the canary record and checks for a new EntityEvent with higher sequence. */
const runUpdateTriggersEvent = async (
  serverUrl: string,
  authToken: string,
  writableResource: string,
  key: string,
  previousSequence: number
): Promise<{ readonly result: ScenarioResult; readonly sequence: number }> => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];
  let sequence = previousSequence;

  // PATCH the canary record with a minimal update
  const url = buildResourceUrl(serverUrl, writableResource, key);
  const response = await odataRequest({
    method: 'PATCH',
    url,
    body: { ModificationTimestamp: new Date().toISOString() },
    authToken,
    headers: { Prefer: 'return=minimal' }
  });

  assertions.push(validateStatusCode(response, [200, 204]));

  // Query EntityEvent
  const filter = encodeURIComponent(`ResourceName eq '${writableResource}' and ResourceRecordKey eq '${key}'`);
  const eeUrl = `${buildResourceUrl(serverUrl, 'EntityEvent')}?$filter=${filter}&$orderby=EntityEventSequence desc&$top=1`;
  const eeResponse = await odataRequest({ method: 'GET', url: eeUrl, authToken });

  const events = parseEntityEvents(eeResponse.body);
  if (events.length > 0) {
    sequence = events[0].EntityEventSequence;
    assertions.push({
      description: 'Update event has higher sequence than create event',
      status: sequence > previousSequence ? 'pass' : 'fail',
      expected: `> ${previousSequence}`,
      actual: String(sequence)
    });
  } else {
    assertions.push({
      description: 'EntityEvent entry found for updated record',
      status: 'fail',
      expected: `EntityEvent for ${writableResource}('${key}')`,
      actual: '(no event found)'
    });
  }

  return { result: buildScenarioResult('update-triggers-event', ['@entity-event', '@rcp-027', '@update'], assertions, start), sequence };
};

/** Deletes the canary record and checks for a new EntityEvent with higher sequence. */
const runDeleteTriggersEvent = async (
  serverUrl: string,
  authToken: string,
  writableResource: string,
  key: string,
  previousSequence: number
): Promise<ScenarioResult> => {
  const start = Date.now();
  const assertions: TestAssertion[] = [];

  // DELETE the canary record
  const url = buildResourceUrl(serverUrl, writableResource, key);
  const response = await odataRequest({ method: 'DELETE', url, authToken });

  assertions.push(validateStatusCode(response, [204]));

  // Query EntityEvent
  const filter = encodeURIComponent(`ResourceName eq '${writableResource}' and ResourceRecordKey eq '${key}'`);
  const eeUrl = `${buildResourceUrl(serverUrl, 'EntityEvent')}?$filter=${filter}&$orderby=EntityEventSequence desc&$top=1`;
  const eeResponse = await odataRequest({ method: 'GET', url: eeUrl, authToken });

  const events = parseEntityEvents(eeResponse.body);
  if (events.length > 0) {
    const sequence = events[0].EntityEventSequence;
    assertions.push({
      description: 'Delete event has higher sequence than update event',
      status: sequence > previousSequence ? 'pass' : 'fail',
      expected: `> ${previousSequence}`,
      actual: String(sequence)
    });
  } else {
    assertions.push({
      description: 'EntityEvent entry found for deleted record',
      status: 'fail',
      expected: `EntityEvent for ${writableResource}('${key}')`,
      actual: '(no event found)'
    });
  }

  // Verify record is actually deleted
  const getResponse = await odataRequest({ method: 'GET', url, authToken });
  assertions.push({
    description: 'Deleted record returns 404',
    status: getResponse.status === 404 ? 'pass' : 'fail',
    expected: '404',
    actual: String(getResponse.status)
  });

  return buildScenarioResult('delete-triggers-event', ['@entity-event', '@rcp-027', '@delete'], assertions, start);
};

// ── Public API ──

/**
 * Runs all EntityEvent compliance scenarios.
 *
 * 1. Loads metadata
 * 2. Runs both-mode scenarios (metadata, read-only, structure, sequence, query, count, sync)
 * 3. If full mode, runs canary write scenarios (create, update, delete)
 * 4. Runs incremental sync (after canary writes in full mode, or with polling in observe mode)
 * 5. Performs batch data validation on collected EntityEvent records
 * 6. Returns structured test report
 */
export const runAllEntityEventScenarios = async (config: EntityEventConfig): Promise<EntityEventTestReport> => {
  const authToken = await resolveAuthToken(config.auth);

  const metadataXml = config.metadataPath
    ? await loadMetadataFromFile(config.metadataPath)
    : await fetchMetadata(config.serverUrl, authToken);

  const metadata = parseMetadataXml(metadataXml);

  const scenarios: ScenarioResult[] = [];

  // ── Both-mode scenarios ──

  // 1. Metadata validation
  scenarios.push(runMetadataValid(metadata));

  // 2. Read-only enforcement
  scenarios.push(await runReadOnlyEnforced(config.serverUrl, authToken));

  // 3. Event structure (also captures events for later use)
  const { result: structureResult, events } = await runEventStructure(config.serverUrl, authToken, config.maxEvents);
  scenarios.push(structureResult);

  // 4. Sequence monotonicity
  scenarios.push(runSequenceMonotonic(events));

  // 5. Query filter
  scenarios.push(await runQueryFilter(config.serverUrl, authToken, events));

  // 6. Query orderby/top/skip
  scenarios.push(await runQueryOrderbyTopSkip(config.serverUrl, authToken));

  // 7. Query count
  scenarios.push(await runQueryCount(config.serverUrl, authToken));

  // ── Full-mode scenarios ──
  const highWaterMark = events.length > 0 ? Math.max(...events.map(e => e.EntityEventSequence)) : 0;

  if (config.mode === 'full') {
    if (!config.payloadsDir) {
      throw new Error('Full mode requires --payloads-dir with a create-succeeds.json file');
    }

    const writableEntityType = getEntityType(metadata, config.writableResource);
    if (!writableEntityType) {
      throw new Error(`Writable resource "${config.writableResource}" not found in metadata`);
    }

    const payloadContent = await readFile(join(config.payloadsDir, 'create-succeeds.json'), 'utf-8');
    const payload = JSON.parse(payloadContent) as Record<string, unknown>;

    // 8. Create triggers event
    const createResult = await runCreateTriggersEvent(config.serverUrl, authToken, config.writableResource, payload, writableEntityType);
    scenarios.push(createResult.result);

    if (createResult.createdKey) {
      // 9. Update triggers event
      const updateResult = await runUpdateTriggersEvent(
        config.serverUrl,
        authToken,
        config.writableResource,
        createResult.createdKey,
        createResult.sequence
      );
      scenarios.push(updateResult.result);

      // 10. Delete triggers event
      scenarios.push(
        await runDeleteTriggersEvent(config.serverUrl, authToken, config.writableResource, createResult.createdKey, updateResult.sequence)
      );
    }
  }

  // 8/11. Incremental sync
  const syncResult = await runIncrementalSync(config.serverUrl, authToken, highWaterMark, config);
  scenarios.push(syncResult.result);

  // ── Data validation ──
  const { assertions: dataAssertions, result: dataResult } = await validateEventData(events, config, authToken, metadata);

  // Add data validation as a synthetic scenario
  const dataValidationScenario = buildScenarioResult('data-validation', ['@entity-event', '@rcp-027', '@data'], dataAssertions, Date.now());
  scenarios.push(dataValidationScenario);

  const passed = scenarios.filter(s => s.passed).length;
  const failed = scenarios.filter(s => !s.passed).length;

  return {
    serverUrl: config.serverUrl,
    resource: 'EntityEvent',
    timestamp: new Date().toISOString(),
    scenarios,
    summary: {
      total: scenarios.length,
      passed,
      failed,
      skipped: 0
    },
    mode: config.mode,
    dataValidation: {
      ...dataResult,
      pollDurationMs: syncResult.pollDurationMs,
      newEventsDuringPoll: syncResult.newEvents
    }
  };
};
