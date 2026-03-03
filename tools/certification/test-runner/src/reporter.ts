import type { ScenarioResult, TestAssertion, TestReport, TestStatus } from './types.js';

/** Maps a test assertion status to a single-character icon for console output. */
const statusIcon = (status: TestStatus): string => {
  switch (status) {
    case 'pass':
      return '\u2713';
    case 'fail':
      return '\u2717';
    case 'warn':
      return '!';
    case 'skip':
      return '-';
  }
};

const scenarioIcon = (passed: boolean): string => (passed ? '\u2713' : '\u2717');

/** Formats a single assertion as an indented line. Failed assertions include expected/actual details. */
const formatAssertion = (a: TestAssertion): string => {
  const icon = statusIcon(a.status);
  const base = `    ${icon} ${a.description}`;
  if (a.status === 'fail' && a.expected && a.actual) {
    return `${base}\n        expected: ${a.expected}\n        actual:   ${a.actual}`;
  }
  return base;
};

/** Formats a scenario result with its pass/fail icon, tags, and all assertion lines. */
const formatScenario = (s: ScenarioResult): string => {
  const icon = scenarioIcon(s.passed);
  const header = `  ${icon} ${s.scenario} (${s.duration}ms)`;
  const tags = s.tags.length > 0 ? `    tags: ${s.tags.join(' ')}` : '';
  const assertions = s.assertions.map(formatAssertion).join('\n');
  return [header, tags, assertions].filter(Boolean).join('\n');
};

/**
 * Formats a test report as a human-readable console string.
 * Includes a header with server/resource info, each scenario with its assertions,
 * and a summary footer with pass/fail/skip counts.
 */
export const formatConsoleReport = (report: TestReport): string => {
  const header = [
    'RESO Certification Compliance Test Report',
    '='.repeat(43),
    `Server:    ${report.serverUrl}`,
    `Resource:  ${report.resource}`,
    `Timestamp: ${report.timestamp}`,
    ''
  ].join('\n');

  const scenarios = report.scenarios.map(formatScenario).join('\n\n');

  const summary = [
    '',
    '-'.repeat(43),
    `Total: ${report.summary.total}  Passed: ${report.summary.passed}  Failed: ${report.summary.failed}  Skipped: ${report.summary.skipped}`
  ].join('\n');

  return `${header}${scenarios}${summary}\n`;
};

/** Formats a test report as pretty-printed JSON. */
export const formatJsonReport = (report: TestReport): string => JSON.stringify(report, null, 2);
