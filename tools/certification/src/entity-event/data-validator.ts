/**
 * EntityEvent data validator — batch-fetches records referenced by EntityEvent
 * entries and validates them against server metadata.
 *
 * Groups events by ResourceName, batches keys (up to batchSize per request),
 * fetches records using OData `in` operator, and validates each record's
 * fields against the Edm types declared in $metadata.
 */

import { buildResourceUrl, odataRequest } from '../test-runner/client.js';
import { validateRecordAgainstMetadata } from '../test-runner/edm-validator.js';
import type { EntityType, ParsedMetadata, TestAssertion } from '../test-runner/types.js';
import type { DataValidationResult, EntityEventConfig, EntityEventRecord } from './types.js';

// ── Helpers ──

/**
 * Groups events by ResourceName and deduplicates keys within each group.
 * Runtime: O(n) where n = number of events.
 */
const groupEventsByResource = (events: ReadonlyArray<EntityEventRecord>): ReadonlyMap<string, ReadonlyArray<string>> => {
  const groups = new Map<string, Set<string>>();
  for (const event of events) {
    const existing = groups.get(event.ResourceName);
    if (existing) {
      existing.add(event.ResourceRecordKey);
    } else {
      groups.set(event.ResourceName, new Set([event.ResourceRecordKey]));
    }
  }
  return new Map([...groups.entries()].map(([k, v]) => [k, [...v]]));
};

/** Splits an array into chunks of the specified size. */
const chunk = <T>(arr: ReadonlyArray<T>, size: number): ReadonlyArray<ReadonlyArray<T>> => {
  const result: T[][] = [];
  for (let i = 0; i < arr.length; i += size) {
    result.push(arr.slice(i, i + size) as T[]);
  }
  return result;
};

/** Finds the key property name for an entity type. Returns the first key property. */
const getKeyFieldName = (entityType: EntityType): string | undefined => entityType.keyProperties[0];

/**
 * Builds an OData `in` filter expression for a batch of keys.
 * Example: `ListingKey in ('key1','key2','key3')`
 */
const buildInFilter = (keyField: string, keys: ReadonlyArray<string>): string => {
  const quotedKeys = keys.map(k => `'${k.replace(/'/g, "''")}'`).join(',');
  return `${keyField} in (${quotedKeys})`;
};

/**
 * Fetches a batch of records using the OData `in` operator.
 * Returns the parsed value array from the OData response, or an empty array on error.
 */
const fetchBatch = async (
  serverUrl: string,
  resource: string,
  keyField: string,
  keys: ReadonlyArray<string>,
  authToken: string
): Promise<ReadonlyArray<Record<string, unknown>>> => {
  const filter = buildInFilter(keyField, keys);
  const url = `${buildResourceUrl(serverUrl, resource)}?$filter=${encodeURIComponent(filter)}`;

  const response = await odataRequest({ method: 'GET', url, authToken });

  if (response.status !== 200) return [];

  const body = response.body as Record<string, unknown> | null;
  if (!body || !Array.isArray(body.value)) return [];

  return body.value as ReadonlyArray<Record<string, unknown>>;
};

/**
 * Fetches a single record by its ResourceRecordUrl.
 * Returns the record or null if not found / error.
 */
const fetchByUrl = async (url: string, authToken: string): Promise<Record<string, unknown> | null> => {
  const response = await odataRequest({ method: 'GET', url, authToken });
  if (response.status !== 200) return null;
  const body = response.body as Record<string, unknown> | null;
  return body && typeof body === 'object' && !Array.isArray(body) ? body : null;
};

// ── Public API ──

/**
 * Validates records referenced by EntityEvent entries.
 *
 * 1. Groups events by ResourceName, deduplicates keys
 * 2. Batch-fetches records using OData `in` operator (up to batchSize keys per request)
 * 3. Validates each record against metadata using the lightweight Edm type checker
 * 4. Also fetches via ResourceRecordUrl when present
 * 5. Tracks 404s (keys not returned by batch fetch)
 *
 * Runtime: O(e + r*b) where e = events, r = unique resources, b = batches per resource.
 */
export const validateEventData = async (
  events: ReadonlyArray<EntityEventRecord>,
  config: EntityEventConfig,
  authToken: string,
  metadata: ParsedMetadata
): Promise<{
  readonly assertions: ReadonlyArray<TestAssertion>;
  readonly result: DataValidationResult;
}> => {
  const assertions: TestAssertion[] = [];
  const notFoundKeys: Array<{ readonly resource: string; readonly key: string }> = [];
  const warnings: string[] = [];
  let eventsValidated = 0;

  const grouped = groupEventsByResource(events);
  const uniqueResources = [...grouped.keys()];

  // Calculate sequence range
  const sequences = events.map(e => e.EntityEventSequence);
  const sequenceRange = sequences.length > 0 ? { min: Math.min(...sequences), max: Math.max(...sequences) } : null;

  for (const [resourceName, keys] of grouped) {
    // Find entity type in metadata
    const entityType = metadata.entityTypes.find(et => et.name === resourceName);
    if (!entityType) {
      warnings.push(`Resource "${resourceName}" not found in metadata — skipping validation`);
      assertions.push({
        description: `${resourceName}: not found in metadata`,
        status: 'warn',
        expected: '(entity type in metadata)',
        actual: resourceName
      });
      continue;
    }

    const keyField = getKeyFieldName(entityType);
    if (!keyField) {
      warnings.push(`Resource "${resourceName}" has no key property — skipping validation`);
      continue;
    }

    // Batch fetch and validate
    const batches = chunk(keys, config.batchSize);
    for (const batch of batches) {
      const records = await fetchBatch(config.serverUrl, resourceName, keyField, batch, authToken);

      // Build a set of returned keys for 404 detection
      const returnedKeys = new Set(records.map(r => String(r[keyField])));

      // Track not-found keys
      for (const key of batch) {
        if (!returnedKeys.has(key)) {
          notFoundKeys.push({ resource: resourceName, key });
        }
      }

      // Validate each returned record
      for (const record of records) {
        const recordAssertions = validateRecordAgainstMetadata(record, entityType, config.strict);
        assertions.push(...recordAssertions);
        eventsValidated++;
      }
    }
  }

  // Validate ResourceRecordUrl where present
  const eventsWithUrl = events.filter(e => e.ResourceRecordUrl);
  for (const event of eventsWithUrl) {
    const record = await fetchByUrl(event.ResourceRecordUrl!, authToken);
    if (record) {
      const entityType = metadata.entityTypes.find(et => et.name === event.ResourceName);
      if (entityType) {
        assertions.push({
          description: `ResourceRecordUrl for ${event.ResourceName}('${event.ResourceRecordKey}'): fetchable`,
          status: 'pass'
        });
      }
    } else {
      assertions.push({
        description: `ResourceRecordUrl for ${event.ResourceName}('${event.ResourceRecordKey}'): not fetchable`,
        status: 'warn',
        expected: '(200 response)',
        actual: '(not found or error)'
      });
    }
  }

  return {
    assertions,
    result: {
      totalEvents: events.length,
      uniqueResources,
      sequenceRange,
      eventsValidated,
      notFoundCount: notFoundKeys.length,
      notFoundKeys,
      pollDurationMs: 0,
      newEventsDuringPoll: 0,
      warnings
    }
  };
};
