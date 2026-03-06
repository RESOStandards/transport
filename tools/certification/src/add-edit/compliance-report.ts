/**
 * Generates a structured compliance report for the RESO Web API Add/Edit endorsement.
 *
 * The report includes:
 * - Top-level outcome (passed/failed) and human-friendly remarks
 * - Per-scenario details with fields, enumerations, expansions, and failure info
 */

import type { EntityProperty, EntityType, ScenarioResult, TestReport } from '../test-runner/types.js';

// ── Public Types ──

export interface ComplianceReport {
  readonly description: 'Web API Add/Edit';
  readonly version: string;
  readonly generatedOn: string;
  readonly outcome: 'passed' | 'failed';
  readonly remarks: string;
  readonly scenarios: ReadonlyArray<ScenarioDetail>;
}

export interface ScenarioDetail {
  readonly name: string;
  readonly operation: 'Create' | 'Update' | 'Delete';
  readonly preference?: 'return=representation' | 'return=minimal';
  readonly resource: string;
  readonly status: 'passed' | 'failed';
  readonly durationMs: number;
  readonly fields: FieldSummary;
  readonly enumerations: EnumerationSummary;
  readonly expansions: ReadonlyArray<string>;
  readonly failures: ReadonlyArray<FailureDetail>;
}

export interface FieldSummary {
  readonly count: number;
  readonly names: ReadonlyArray<string>;
}

export interface EnumerationSummary {
  readonly count: number;
  readonly values: ReadonlyArray<EnumerationDetail>;
}

export interface EnumerationDetail {
  readonly fieldName: string;
  readonly value: unknown;
}

export interface FailureDetail {
  readonly assertion: string;
  readonly expected?: string;
  readonly actual?: string;
}

// ── Report Configuration ──

export interface ReportConfig {
  readonly version: string;
  readonly payloads: Readonly<Record<string, Record<string, unknown>>>;
  readonly entityType: EntityType;
}

// ── Internal Helpers ──

/** Maps a scenario name to its operation type. */
const deriveOperation = (tags: ReadonlyArray<string>): 'Create' | 'Update' | 'Delete' => {
  if (tags.includes('@create')) return 'Create';
  if (tags.includes('@update')) return 'Update';
  return 'Delete';
};

/** Extracts the Prefer response mode from scenario tags, if present. */
const derivePreference = (tags: ReadonlyArray<string>): 'return=representation' | 'return=minimal' | undefined => {
  if (tags.includes('@return-representation')) return 'return=representation';
  if (tags.includes('@return-minimal')) return 'return=minimal';
  return undefined;
};

/** Checks whether an EntityProperty is an enumeration type. */
const isEnumProperty = (prop: EntityProperty): boolean =>
  prop.type.startsWith('org.reso.metadata.enums.') ||
  prop.type.startsWith('Collection(org.reso.metadata.enums.') ||
  prop.annotations?.['RESO.OData.Metadata.LookupName'] !== undefined;

/** Maps scenario names to their payload keys. */
const SCENARIO_PAYLOAD_MAP: Readonly<Record<string, string>> = {
  'create-succeeds-representation': 'createSucceeds',
  'create-succeeds-minimal': 'createSucceeds',
  'create-fails': 'createFails',
  'update-succeeds-representation': 'updateSucceeds',
  'update-succeeds-minimal': 'updateSucceeds',
  'update-fails': 'updateFails',
  'delete-succeeds': 'deleteSucceeds',
  'delete-fails': 'deleteFails'
};

/** Extracts field names from a payload, excluding OData annotations and key fields. */
const extractPayloadFields = (payload: Record<string, unknown>, entityType: EntityType): ReadonlyArray<string> => {
  const keySet = new Set(entityType.keyProperties);
  return Object.keys(payload).filter(k => !k.startsWith('@') && !keySet.has(k));
};

/** Extracts enumeration field names and their values from a payload. */
const extractEnumerations = (payload: Record<string, unknown>, entityType: EntityType): ReadonlyArray<EnumerationDetail> => {
  const propMap = new Map(entityType.properties.map(p => [p.name, p]));
  const results: EnumerationDetail[] = [];

  for (const [fieldName, value] of Object.entries(payload)) {
    if (fieldName.startsWith('@')) continue;
    const prop = propMap.get(fieldName);
    if (prop && isEnumProperty(prop) && value !== null && value !== undefined) {
      results.push({ fieldName, value });
    }
  }

  return results;
};

/** Extracts expansion (navigation property) field names from a payload. */
const extractExpansions = (payload: Record<string, unknown>, entityType: EntityType): ReadonlyArray<string> => {
  const propNames = new Set(entityType.properties.map(p => p.name));
  return Object.keys(payload).filter(
    k => !k.startsWith('@') && !propNames.has(k) && (Array.isArray(payload[k]) || typeof payload[k] === 'object')
  );
};

/** Builds a ScenarioDetail from a ScenarioResult and its payload. */
const buildScenarioDetail = (
  scenario: ScenarioResult,
  resource: string,
  payloads: Readonly<Record<string, Record<string, unknown>>>,
  entityType: EntityType
): ScenarioDetail => {
  const payloadKey = SCENARIO_PAYLOAD_MAP[scenario.scenario];
  const payload = payloadKey ? (payloads[payloadKey] ?? {}) : {};
  const operation = deriveOperation(scenario.tags);
  const preference = derivePreference(scenario.tags);

  const fieldNames = operation === 'Delete' ? [] : extractPayloadFields(payload, entityType);

  const enumerations = operation === 'Delete' ? [] : extractEnumerations(payload, entityType);

  const expansions = operation === 'Delete' ? [] : extractExpansions(payload, entityType);

  const failures: ReadonlyArray<FailureDetail> = scenario.assertions
    .filter(a => a.status === 'fail')
    .map(a => ({
      assertion: a.description,
      expected: a.expected,
      actual: a.actual
    }));

  return {
    name: scenario.scenario,
    operation,
    preference,
    resource,
    status: scenario.passed ? 'passed' : 'failed',
    durationMs: scenario.duration,
    fields: { count: fieldNames.length, names: fieldNames },
    enumerations: { count: enumerations.length, values: enumerations },
    expansions,
    failures
  };
};

// ── Remarks Generation ──

/** Builds a human-friendly remarks string from the test report. */
const generateRemarks = (report: TestReport, entityType: EntityType, scenarios: ReadonlyArray<ScenarioDetail>): string => {
  const allPassed = report.summary.failed === 0;

  // Collect unique operations and preferences
  const operations = new Map<string, Set<string>>();
  for (const s of scenarios) {
    if (!operations.has(s.operation)) operations.set(s.operation, new Set());
    if (s.preference) operations.get(s.operation)!.add(s.preference);
  }

  // Build operation descriptions
  const operationParts: string[] = [];
  for (const [op, prefs] of operations) {
    if (prefs.size > 0) {
      const prefList = [...prefs].map(p => p.replace('return=', '')).join(', ');
      operationParts.push(`${op} (${prefList})`);
    } else {
      operationParts.push(op);
    }
  }

  const operationText = joinWithAnd(operationParts);
  const totalFields = entityType.properties.length;

  // Count unique fields across all create/update payloads
  const allFieldNames = new Set(scenarios.flatMap(s => s.fields.names));
  const testedFieldCount = allFieldNames.size;

  const fieldSummary = `${testedFieldCount} of ${totalFields} fields`;

  if (allPassed) {
    return `Tested ${operationText} operations on ${report.resource} with ${fieldSummary}.`;
  }

  // Failure summary
  const failedScenarios = scenarios.filter(s => s.status === 'failed');
  const failedNames = failedScenarios.map(s => s.name).join(', ');
  return (
    `Tested ${operationText} operations on ${report.resource} with ${fieldSummary}. ` +
    `${report.summary.failed} of ${report.summary.total} scenarios failed: ${failedNames}.`
  );
};

/** Joins an array of strings with commas and "and" for the last item. */
const joinWithAnd = (items: ReadonlyArray<string>): string => {
  if (items.length === 0) return '';
  if (items.length === 1) return items[0];
  if (items.length === 2) return `${items[0]} and ${items[1]}`;
  return `${items.slice(0, -1).join(', ')}, and ${items[items.length - 1]}`;
};

// ── Public API ──

/** Generates a structured compliance report from test results and payloads. */
export const generateComplianceReport = (report: TestReport, config: ReportConfig): ComplianceReport => {
  const scenarios = report.scenarios.map(s => buildScenarioDetail(s, report.resource, config.payloads, config.entityType));

  const outcome = report.summary.failed === 0 ? 'passed' : 'failed';
  const remarks = generateRemarks(report, config.entityType, scenarios);

  return {
    description: 'Web API Add/Edit',
    version: config.version,
    generatedOn: new Date().toISOString(),
    outcome,
    remarks,
    scenarios
  };
};
