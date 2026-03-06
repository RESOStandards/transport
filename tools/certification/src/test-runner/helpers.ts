/**
 * Generic test helpers used by certification scenario implementations.
 */

import { validatePayloadAgainstMetadata } from './metadata.js';
import type { EntityType, ScenarioResult, TestAssertion } from './types.js';

/**
 * Extracts the primary key value from a payload using the entity type's key
 * property definition. For example, if the entity type has key "ListingKey"
 * and the payload contains { "ListingKey": "12345", "ListPrice": 100 },
 * this returns "12345".
 */
export const extractPrimaryKey = (payload: Readonly<Record<string, unknown>>, entityType: EntityType): string | undefined => {
  if (entityType.keyProperties.length === 0) return undefined;
  const keyProp = entityType.keyProperties[0];
  const value = payload[keyProp];
  return value !== undefined ? String(value) : undefined;
};

/**
 * Returns a copy of the payload without the primary key field.
 * The key is used in the URL for PATCH/DELETE, not in the request body.
 */
export const stripPrimaryKey = (payload: Readonly<Record<string, unknown>>, entityType: EntityType): Record<string, unknown> => {
  const keyProps = new Set(entityType.keyProperties);
  return Object.fromEntries(Object.entries(payload).filter(([key]) => !keyProps.has(key)));
};

/**
 * Creates a test assertion that validates the payload's field names exist in the entity type metadata.
 * This is a pre-flight check on the test data — value validation is the server's responsibility
 * and is tested by the failure scenarios. Full type/value validation is available via
 * validatePayloadAgainstMetadata().failures for other use cases.
 */
export const makeSchemaAssertion = (payload: Record<string, unknown>, entityType: EntityType): TestAssertion => {
  const check = validatePayloadAgainstMetadata(payload, entityType);
  const hasUnknownFields = check.unknownFields.length > 0;
  return {
    description: 'Payload fields exist in metadata',
    status: hasUnknownFields ? 'fail' : 'pass',
    expected: '(all fields in metadata)',
    actual: hasUnknownFields ? `Unknown fields: ${check.unknownFields.join(', ')}` : '(all fields valid)',
    gherkinStep: 'schema in payload matches the metadata'
  };
};

/** Constructs a ScenarioResult from collected assertions. A scenario passes if all assertions pass, skip, or warn. */
export const buildScenarioResult = (
  scenario: string,
  tags: ReadonlyArray<string>,
  assertions: ReadonlyArray<TestAssertion>,
  start: number
): ScenarioResult => ({
  scenario,
  tags,
  assertions,
  passed: assertions.every(a => a.status === 'pass' || a.status === 'skip' || a.status === 'warn'),
  duration: Date.now() - start
});
