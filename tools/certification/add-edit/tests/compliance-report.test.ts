import type { EntityType, ScenarioResult, TestReport } from '@reso/certification-test-runner';
import { describe, expect, it } from 'vitest';
import { generateComplianceReport } from '../src/lib/compliance-report.js';

const makeEntityType = (): EntityType => ({
  name: 'Property',
  keyProperties: ['ListingKey'],
  properties: [
    { name: 'ListingKey', type: 'Edm.String' },
    { name: 'ListPrice', type: 'Edm.Decimal' },
    { name: 'BedroomsTotal', type: 'Edm.Int32' },
    { name: 'BathroomsTotalInteger', type: 'Edm.Int32' },
    { name: 'City', type: 'Edm.String' },
    { name: 'StateOrProvince', type: 'Edm.String' },
    { name: 'PostalCode', type: 'Edm.String' },
    { name: 'Country', type: 'Edm.String' },
    { name: 'StandardStatus', type: 'Edm.String', annotations: { 'RESO.OData.Metadata.LookupName': 'StandardStatus' } },
    {
      name: 'AccessibilityFeatures',
      type: 'Collection(Edm.String)',
      annotations: { 'RESO.OData.Metadata.LookupName': 'AccessibilityFeatures' }
    }
  ]
});

const makeScenario = (name: string, tags: string[], passed: boolean, duration = 10): ScenarioResult => ({
  scenario: name,
  tags,
  passed,
  duration,
  assertions: passed
    ? [{ description: 'Status code is 200', status: 'pass' }]
    : [{ description: 'Status code is one of [200, 204]', status: 'fail', expected: '200 or 204', actual: '400' }]
});

const makePayloads = (): Record<string, Record<string, unknown>> => ({
  createSucceeds: {
    ListPrice: 350000,
    BedroomsTotal: 4,
    BathroomsTotalInteger: 3,
    City: 'Test City',
    StateOrProvince: 'CA',
    PostalCode: '90210',
    Country: 'US'
  },
  createFails: {
    ListPrice: -99999,
    BedroomsTotal: 3,
    BathroomsTotalInteger: 2
  },
  updateSucceeds: {
    ListingKey: 'abc-123',
    ListPrice: 375000
  },
  updateFails: {
    ListingKey: 'abc-123',
    ListPrice: -1
  },
  deleteSucceeds: { id: 'def-456' },
  deleteFails: { id: '00000000-0000-0000-0000-000000000000' }
});

const makeReport = (allPassed: boolean): TestReport => {
  const scenarios = [
    makeScenario('create-succeeds-representation', ['@create', '@create-succeeds', '@return-representation'], allPassed),
    makeScenario('create-succeeds-minimal', ['@create', '@create-succeeds', '@return-minimal'], allPassed),
    makeScenario('create-fails', ['@create', '@create-fails'], true),
    makeScenario('update-succeeds-representation', ['@update', '@update-succeeds', '@return-representation'], allPassed),
    makeScenario('update-succeeds-minimal', ['@update', '@update-succeeds', '@return-minimal'], allPassed),
    makeScenario('update-fails', ['@update', '@update-fails'], true),
    makeScenario('delete-succeeds', ['@delete', '@delete-succeeds'], true),
    makeScenario('delete-fails', ['@delete', '@delete-fails'], true)
  ];

  const passed = scenarios.filter(s => s.passed).length;
  return {
    serverUrl: 'http://localhost:8080',
    resource: 'Property',
    timestamp: '2026-03-06T00:00:00Z',
    scenarios,
    summary: { total: 8, passed, failed: 8 - passed, skipped: 0 }
  };
};

describe('generateComplianceReport', () => {
  it('produces correct top-level fields when all pass', () => {
    const report = generateComplianceReport(makeReport(true), {
      version: '2.0.0',
      payloads: makePayloads(),
      entityType: makeEntityType()
    });

    expect(report.description).toBe('Web API Add/Edit');
    expect(report.version).toBe('2.0.0');
    expect(report.outcome).toBe('passed');
    expect(report.generatedOn).toMatch(/^\d{4}-\d{2}-\d{2}T/);
  });

  it('sets outcome to failed when any scenario fails', () => {
    const report = generateComplianceReport(makeReport(false), {
      version: '2.0.0',
      payloads: makePayloads(),
      entityType: makeEntityType()
    });

    expect(report.outcome).toBe('failed');
  });

  it('generates correct scenario count', () => {
    const report = generateComplianceReport(makeReport(true), {
      version: '2.0.0',
      payloads: makePayloads(),
      entityType: makeEntityType()
    });

    expect(report.scenarios).toHaveLength(8);
  });

  it('derives operation and preference from tags', () => {
    const report = generateComplianceReport(makeReport(true), {
      version: '2.0.0',
      payloads: makePayloads(),
      entityType: makeEntityType()
    });

    const createRep = report.scenarios.find(s => s.name === 'create-succeeds-representation')!;
    expect(createRep.operation).toBe('Create');
    expect(createRep.preference).toBe('return=representation');

    const updateMin = report.scenarios.find(s => s.name === 'update-succeeds-minimal')!;
    expect(updateMin.operation).toBe('Update');
    expect(updateMin.preference).toBe('return=minimal');

    const deleteSucc = report.scenarios.find(s => s.name === 'delete-succeeds')!;
    expect(deleteSucc.operation).toBe('Delete');
    expect(deleteSucc.preference).toBeUndefined();
  });

  it('extracts field names from create payloads (excluding key fields)', () => {
    const report = generateComplianceReport(makeReport(true), {
      version: '2.0.0',
      payloads: makePayloads(),
      entityType: makeEntityType()
    });

    const createRep = report.scenarios.find(s => s.name === 'create-succeeds-representation')!;
    expect(createRep.fields.count).toBe(7);
    expect(createRep.fields.names).toContain('ListPrice');
    expect(createRep.fields.names).toContain('City');
    expect(createRep.fields.names).not.toContain('ListingKey');
  });

  it('extracts field names from update payloads (excluding key fields)', () => {
    const report = generateComplianceReport(makeReport(true), {
      version: '2.0.0',
      payloads: makePayloads(),
      entityType: makeEntityType()
    });

    const updateRep = report.scenarios.find(s => s.name === 'update-succeeds-representation')!;
    expect(updateRep.fields.count).toBe(1);
    expect(updateRep.fields.names).toEqual(['ListPrice']);
  });

  it('delete scenarios have empty fields', () => {
    const report = generateComplianceReport(makeReport(true), {
      version: '2.0.0',
      payloads: makePayloads(),
      entityType: makeEntityType()
    });

    const del = report.scenarios.find(s => s.name === 'delete-succeeds')!;
    expect(del.fields.count).toBe(0);
    expect(del.fields.names).toEqual([]);
  });

  it('detects enumeration fields and includes their values', () => {
    const payloads = makePayloads();
    payloads.createSucceeds = {
      ...payloads.createSucceeds,
      StandardStatus: 'Active',
      AccessibilityFeatures: ['Elevator', 'Ramp']
    };

    const report = generateComplianceReport(makeReport(true), {
      version: '2.0.0',
      payloads,
      entityType: makeEntityType()
    });

    const createRep = report.scenarios.find(s => s.name === 'create-succeeds-representation')!;
    expect(createRep.enumerations.count).toBe(2);
    expect(createRep.enumerations.values).toContainEqual({ fieldName: 'StandardStatus', value: 'Active' });
    expect(createRep.enumerations.values).toContainEqual({ fieldName: 'AccessibilityFeatures', value: ['Elevator', 'Ramp'] });
  });

  it('includes failure details for failed scenarios', () => {
    const report = generateComplianceReport(makeReport(false), {
      version: '2.0.0',
      payloads: makePayloads(),
      entityType: makeEntityType()
    });

    const createRep = report.scenarios.find(s => s.name === 'create-succeeds-representation')!;
    expect(createRep.status).toBe('failed');
    expect(createRep.failures).toHaveLength(1);
    expect(createRep.failures[0].assertion).toBe('Status code is one of [200, 204]');
    expect(createRep.failures[0].expected).toBe('200 or 204');
    expect(createRep.failures[0].actual).toBe('400');
  });

  it('remarks mention all operations when all pass', () => {
    const report = generateComplianceReport(makeReport(true), {
      version: '2.0.0',
      payloads: makePayloads(),
      entityType: makeEntityType()
    });

    expect(report.remarks).toContain('Create');
    expect(report.remarks).toContain('Update');
    expect(report.remarks).toContain('Delete');
    expect(report.remarks).toContain('Property');
    expect(report.remarks).toContain('of 10 fields');
  });

  it('remarks mention failures when some fail', () => {
    const report = generateComplianceReport(makeReport(false), {
      version: '2.0.0',
      payloads: makePayloads(),
      entityType: makeEntityType()
    });

    expect(report.remarks).toContain('failed');
    expect(report.remarks).toContain('create-succeeds-representation');
  });
});
