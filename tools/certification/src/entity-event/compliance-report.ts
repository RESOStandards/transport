/**
 * EntityEvent (RCP-027) compliance report generator.
 *
 * Produces a structured JSON report with per-scenario details,
 * data validation summary, and human-friendly remarks.
 */

import type { DataValidationResult, EntityEventMode, EntityEventTestReport } from './types.js';

// ── Public Types ──

export interface EntityEventComplianceReport {
  readonly description: 'EntityEvent (RCP-027)';
  readonly version: string;
  readonly generatedOn: string;
  readonly outcome: 'passed' | 'failed';
  readonly remarks: string;
  readonly mode: EntityEventMode;
  readonly scenarios: ReadonlyArray<EntityEventScenarioDetail>;
  readonly dataValidation: DataValidationResult;
}

export interface EntityEventScenarioDetail {
  readonly name: string;
  readonly mode: 'both' | 'full';
  readonly status: 'passed' | 'failed' | 'skipped';
  readonly durationMs: number;
  readonly failures: ReadonlyArray<FailureDetail>;
}

export interface FailureDetail {
  readonly assertion: string;
  readonly expected?: string;
  readonly actual?: string;
}

export interface EntityEventReportConfig {
  readonly version: string;
}

// ── Scenario Mode Classification ──

const FULL_ONLY_SCENARIOS = new Set(['create-triggers-event', 'update-triggers-event', 'delete-triggers-event']);

const deriveScenarioMode = (name: string): 'both' | 'full' => (FULL_ONLY_SCENARIOS.has(name) ? 'full' : 'both');

// ── Remarks Generation ──

/** Joins an array of strings with commas and "and" for the last item. */
const joinWithAnd = (items: ReadonlyArray<string>): string => {
  if (items.length === 0) return '';
  if (items.length === 1) return items[0];
  if (items.length === 2) return `${items[0]} and ${items[1]}`;
  return `${items.slice(0, -1).join(', ')}, and ${items[items.length - 1]}`;
};

const generateRemarks = (report: EntityEventTestReport, scenarios: ReadonlyArray<EntityEventScenarioDetail>): string => {
  const { dataValidation: dv } = report;
  const parts: string[] = [];

  parts.push(`Tested EntityEvent (RCP-027) in ${report.mode} mode.`);

  if (dv.totalEvents > 0) {
    parts.push(
      `Observed ${dv.totalEvents} events across ${dv.uniqueResources.length} resource${dv.uniqueResources.length === 1 ? '' : 's'} (${joinWithAnd([...dv.uniqueResources])}).`
    );
    if (dv.sequenceRange) {
      parts.push(`Sequence range: ${dv.sequenceRange.min}–${dv.sequenceRange.max}.`);
    }
  } else {
    parts.push('No EntityEvent records observed.');
  }

  if (dv.eventsValidated > 0) {
    parts.push(`Validated ${dv.eventsValidated} records against metadata.`);
  }
  if (dv.notFoundCount > 0) {
    parts.push(`${dv.notFoundCount} referenced records not found (404).`);
  }
  if (dv.newEventsDuringPoll > 0) {
    parts.push(`${dv.newEventsDuringPoll} new events observed during ${dv.pollDurationMs}ms poll window.`);
  }

  if (report.summary.failed > 0) {
    const failedNames = scenarios.filter(s => s.status === 'failed').map(s => s.name);
    parts.push(`${report.summary.failed} of ${report.summary.total} scenarios failed: ${failedNames.join(', ')}.`);
  }

  return parts.join(' ');
};

// ── Public API ──

/** Generates a structured compliance report from EntityEvent test results. */
export const generateEntityEventComplianceReport = (
  report: EntityEventTestReport,
  config: EntityEventReportConfig
): EntityEventComplianceReport => {
  const scenarios: EntityEventScenarioDetail[] = report.scenarios.map(s => ({
    name: s.scenario,
    mode: deriveScenarioMode(s.scenario),
    status: s.passed ? 'passed' : 'failed',
    durationMs: s.duration,
    failures: s.assertions
      .filter(a => a.status === 'fail')
      .map(a => ({
        assertion: a.description,
        expected: a.expected,
        actual: a.actual
      }))
  }));

  const outcome = report.summary.failed === 0 ? 'passed' : 'failed';

  return {
    description: 'EntityEvent (RCP-027)',
    version: config.version,
    generatedOn: new Date().toISOString(),
    outcome,
    remarks: generateRemarks(report, scenarios),
    mode: report.mode,
    scenarios,
    dataValidation: report.dataValidation
  };
};
