import { describe, expect, it } from 'vitest';
import { generateEntityEventComplianceReport } from '../src/entity-event/compliance-report.js';
import type { EntityEventTestReport } from '../src/entity-event/types.js';

const makeReport = (overrides?: Partial<EntityEventTestReport>): EntityEventTestReport => ({
  serverUrl: 'http://localhost:8080',
  resource: 'EntityEvent',
  timestamp: '2025-01-15T10:00:00Z',
  scenarios: [
    { scenario: 'metadata-valid', tags: ['@entity-event'], assertions: [], passed: true, duration: 10 },
    { scenario: 'read-only-enforced', tags: ['@entity-event'], assertions: [], passed: true, duration: 15 },
    { scenario: 'event-structure', tags: ['@entity-event'], assertions: [], passed: true, duration: 20 },
    { scenario: 'sequence-monotonic', tags: ['@entity-event'], assertions: [], passed: true, duration: 5 }
  ],
  summary: { total: 4, passed: 4, failed: 0, skipped: 0 },
  mode: 'observe',
  dataValidation: {
    totalEvents: 50,
    uniqueResources: ['Property', 'Member'],
    sequenceRange: { min: 1, max: 50 },
    eventsValidated: 45,
    notFoundCount: 5,
    notFoundKeys: [{ resource: 'Property', key: 'deleted-1' }],
    pollDurationMs: 30000,
    newEventsDuringPoll: 3,
    warnings: []
  },
  ...overrides
});

describe('generateEntityEventComplianceReport', () => {
  it('generates a passing report', () => {
    const report = makeReport();
    const result = generateEntityEventComplianceReport(report, { version: 'RCP-027' });

    expect(result.description).toBe('EntityEvent (RCP-027)');
    expect(result.version).toBe('RCP-027');
    expect(result.outcome).toBe('passed');
    expect(result.mode).toBe('observe');
    expect(result.generatedOn).toBeTruthy();
    expect(result.scenarios.length).toBe(4);
  });

  it('generates a failing report when scenarios fail', () => {
    const report = makeReport({
      scenarios: [
        { scenario: 'metadata-valid', tags: [], assertions: [{ description: 'test', status: 'fail' }], passed: false, duration: 10 },
        { scenario: 'event-structure', tags: [], assertions: [], passed: true, duration: 20 }
      ],
      summary: { total: 2, passed: 1, failed: 1, skipped: 0 }
    });

    const result = generateEntityEventComplianceReport(report, { version: 'RCP-027' });

    expect(result.outcome).toBe('failed');
    expect(result.scenarios.find(s => s.name === 'metadata-valid')?.status).toBe('failed');
    expect(result.scenarios.find(s => s.name === 'metadata-valid')?.failures.length).toBe(1);
  });

  it('includes data validation in report', () => {
    const report = makeReport();
    const result = generateEntityEventComplianceReport(report, { version: 'RCP-027' });

    expect(result.dataValidation.totalEvents).toBe(50);
    expect(result.dataValidation.uniqueResources).toEqual(['Property', 'Member']);
    expect(result.dataValidation.sequenceRange?.min).toBe(1);
    expect(result.dataValidation.sequenceRange?.max).toBe(50);
  });

  it('classifies full-only scenarios correctly', () => {
    const report = makeReport({
      scenarios: [
        { scenario: 'metadata-valid', tags: [], assertions: [], passed: true, duration: 10 },
        { scenario: 'create-triggers-event', tags: [], assertions: [], passed: true, duration: 50 },
        { scenario: 'update-triggers-event', tags: [], assertions: [], passed: true, duration: 30 },
        { scenario: 'delete-triggers-event', tags: [], assertions: [], passed: true, duration: 20 }
      ],
      summary: { total: 4, passed: 4, failed: 0, skipped: 0 },
      mode: 'full'
    });

    const result = generateEntityEventComplianceReport(report, { version: 'RCP-027' });

    expect(result.scenarios.find(s => s.name === 'metadata-valid')?.mode).toBe('both');
    expect(result.scenarios.find(s => s.name === 'create-triggers-event')?.mode).toBe('full');
    expect(result.scenarios.find(s => s.name === 'update-triggers-event')?.mode).toBe('full');
    expect(result.scenarios.find(s => s.name === 'delete-triggers-event')?.mode).toBe('full');
  });

  it('generates human-readable remarks', () => {
    const report = makeReport();
    const result = generateEntityEventComplianceReport(report, { version: 'RCP-027' });

    expect(result.remarks).toContain('observe mode');
    expect(result.remarks).toContain('50 events');
    expect(result.remarks).toContain('Property');
    expect(result.remarks).toContain('Member');
  });

  it('includes failure info in remarks when scenarios fail', () => {
    const report = makeReport({
      scenarios: [
        { scenario: 'query-filter', tags: [], assertions: [{ description: 'test', status: 'fail' }], passed: false, duration: 10 }
      ],
      summary: { total: 1, passed: 0, failed: 1, skipped: 0 }
    });

    const result = generateEntityEventComplianceReport(report, { version: 'RCP-027' });

    expect(result.remarks).toContain('1 of 1 scenarios failed');
    expect(result.remarks).toContain('query-filter');
  });
});
