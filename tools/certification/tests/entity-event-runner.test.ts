import type { Server } from 'node:http';
import { afterAll, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { resetStore, seedEntityEvents } from '../src/entity-event/mock/handlers.js';
import { startMockEntityEventServer, stopMockEntityEventServer } from '../src/entity-event/mock/server.js';
import { runAllEntityEventScenarios } from '../src/entity-event/test-runner.js';
import type { EntityEventConfig } from '../src/entity-event/types.js';

// ── Sample metadata XML with EntityEvent + Property ──
const METADATA_XML = `<?xml version="1.0" encoding="utf-8"?>
<edmx:Edmx Version="4.0" xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx">
  <edmx:DataServices>
    <Schema Namespace="org.reso.metadata" xmlns="http://docs.oasis-open.org/odata/ns/edm">
      <EntityType Name="EntityEvent">
        <Key><PropertyRef Name="EntityEventSequence"/></Key>
        <Property Name="EntityEventSequence" Type="Edm.Int64" Nullable="false"/>
        <Property Name="ResourceName" Type="Edm.String"/>
        <Property Name="ResourceRecordKey" Type="Edm.String"/>
        <Property Name="ResourceRecordUrl" Type="Edm.String"/>
      </EntityType>
      <EntityType Name="Property">
        <Key><PropertyRef Name="ListingKey"/></Key>
        <Property Name="ListingKey" Type="Edm.String" Nullable="false"/>
        <Property Name="ListPrice" Type="Edm.Decimal"/>
        <Property Name="City" Type="Edm.String"/>
        <Property Name="ModificationTimestamp" Type="Edm.DateTimeOffset"/>
      </EntityType>
      <EntityContainer Name="Default">
        <EntitySet Name="EntityEvent" EntityType="org.reso.metadata.EntityEvent"/>
        <EntitySet Name="Property" EntityType="org.reso.metadata.Property"/>
      </EntityContainer>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>`;

let server: Server;
let serverUrl: string;

beforeAll(async () => {
  const mock = await startMockEntityEventServer({
    metadataXml: METADATA_XML,
    canaryResource: 'Property',
    port: 0
  });
  server = mock.server;
  serverUrl = mock.url;
});

afterAll(async () => {
  await stopMockEntityEventServer(server);
});

beforeEach(() => {
  resetStore();
});

const makeConfig = (overrides?: Partial<EntityEventConfig>): EntityEventConfig => ({
  serverUrl,
  auth: { mode: 'token', authToken: 'test-token' },
  mode: 'observe',
  writableResource: 'Property',
  maxEvents: 1000,
  batchSize: 100,
  pollIntervalMs: 100,
  pollTimeoutMs: 500,
  strict: false,
  ...overrides
});

describe('observe mode', () => {
  it('passes all both-mode scenarios with seeded events', async () => {
    // Seed some events
    seedEntityEvents([
      { ResourceName: 'Property', ResourceRecordKey: 'p1' },
      { ResourceName: 'Property', ResourceRecordKey: 'p2' },
      { ResourceName: 'Member', ResourceRecordKey: 'm1' }
    ]);

    const report = await runAllEntityEventScenarios(makeConfig());

    expect(report.mode).toBe('observe');
    expect(report.resource).toBe('EntityEvent');

    // metadata-valid should pass
    const metadataScenario = report.scenarios.find(s => s.scenario === 'metadata-valid');
    expect(metadataScenario?.passed).toBe(true);

    // read-only-enforced should pass
    const readOnlyScenario = report.scenarios.find(s => s.scenario === 'read-only-enforced');
    expect(readOnlyScenario?.passed).toBe(true);

    // event-structure should pass
    const structureScenario = report.scenarios.find(s => s.scenario === 'event-structure');
    expect(structureScenario?.passed).toBe(true);

    // sequence-monotonic should pass
    const sequenceScenario = report.scenarios.find(s => s.scenario === 'sequence-monotonic');
    expect(sequenceScenario?.passed).toBe(true);

    // query-filter should pass
    const filterScenario = report.scenarios.find(s => s.scenario === 'query-filter');
    expect(filterScenario?.passed).toBe(true);

    // query-count should pass
    const countScenario = report.scenarios.find(s => s.scenario === 'query-count');
    expect(countScenario?.passed).toBe(true);
  });

  it('does not include full-mode scenarios in observe mode', async () => {
    seedEntityEvents([{ ResourceName: 'Property', ResourceRecordKey: 'p1' }]);

    const report = await runAllEntityEventScenarios(makeConfig());

    const scenarioNames = report.scenarios.map(s => s.scenario);
    expect(scenarioNames).not.toContain('create-triggers-event');
    expect(scenarioNames).not.toContain('update-triggers-event');
    expect(scenarioNames).not.toContain('delete-triggers-event');
  });

  it('handles empty EntityEvent table gracefully', async () => {
    const report = await runAllEntityEventScenarios(makeConfig());

    // Should not crash, metadata and read-only should still pass
    const metadataScenario = report.scenarios.find(s => s.scenario === 'metadata-valid');
    expect(metadataScenario?.passed).toBe(true);

    const readOnlyScenario = report.scenarios.find(s => s.scenario === 'read-only-enforced');
    expect(readOnlyScenario?.passed).toBe(true);
  });

  it('reports data validation results', async () => {
    seedEntityEvents([
      { ResourceName: 'Property', ResourceRecordKey: 'p1' },
      { ResourceName: 'Property', ResourceRecordKey: 'p2' }
    ]);

    const report = await runAllEntityEventScenarios(makeConfig());

    expect(report.dataValidation.totalEvents).toBe(2);
    expect(report.dataValidation.uniqueResources).toContain('Property');
    expect(report.dataValidation.sequenceRange).toBeTruthy();
    expect(report.dataValidation.sequenceRange!.min).toBeLessThanOrEqual(report.dataValidation.sequenceRange!.max);
  });
});

describe('full mode', () => {
  it('runs canary write scenarios and verifies EntityEvent entries', async () => {
    // Seed some initial events
    seedEntityEvents([{ ResourceName: 'Property', ResourceRecordKey: 'p1' }]);

    const report = await runAllEntityEventScenarios(
      makeConfig({
        mode: 'full',
        payloadsDir: `${import.meta.dirname}/../sample-payloads`
      })
    );

    expect(report.mode).toBe('full');

    // Create scenario should pass
    const createScenario = report.scenarios.find(s => s.scenario === 'create-triggers-event');
    expect(createScenario?.passed).toBe(true);

    // Update scenario should pass
    const updateScenario = report.scenarios.find(s => s.scenario === 'update-triggers-event');
    expect(updateScenario?.passed).toBe(true);

    // Delete scenario should pass
    const deleteScenario = report.scenarios.find(s => s.scenario === 'delete-triggers-event');
    expect(deleteScenario?.passed).toBe(true);
  });

  it('incremental sync finds new events after canary writes', async () => {
    const report = await runAllEntityEventScenarios(
      makeConfig({
        mode: 'full',
        payloadsDir: `${import.meta.dirname}/../sample-payloads`
      })
    );

    const syncScenario = report.scenarios.find(s => s.scenario === 'incremental-sync');
    expect(syncScenario?.passed).toBe(true);
  });
});
