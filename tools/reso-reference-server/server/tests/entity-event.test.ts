import { describe, expect, it, vi } from 'vitest';
import type { CollectionResult, DataAccessLayer, EntityRecord, ResourceContext, SingleResult } from '../src/db/data-access.js';
import type { CompactionRunner } from '../src/db/entity-event-compaction.js';
import { startCompaction, stopCompaction } from '../src/db/entity-event-compaction.js';
import type { EntityEventWriter } from '../src/db/entity-event-dal.js';
import { createEntityEventDal } from '../src/db/entity-event-dal.js';

// ---------------------------------------------------------------------------
// Mock DAL
// ---------------------------------------------------------------------------

const makeResourceCtx = (resource: string, keyField = 'TestKey'): ResourceContext => ({
  resource,
  keyField,
  fields: [],
  navigationBindings: []
});

const makeMockDal = (): DataAccessLayer & {
  readonly insertCalls: Array<{ ctx: ResourceContext; record: Record<string, unknown> }>;
  readonly updateCalls: Array<{ ctx: ResourceContext; keyValue: string }>;
  readonly deleteCalls: Array<{ ctx: ResourceContext; keyValue: string }>;
  setUpdateResult: (result: SingleResult) => void;
  setDeleteResult: (result: boolean) => void;
} => {
  const insertCalls: Array<{ ctx: ResourceContext; record: Record<string, unknown> }> = [];
  const updateCalls: Array<{ ctx: ResourceContext; keyValue: string }> = [];
  const deleteCalls: Array<{ ctx: ResourceContext; keyValue: string }> = [];
  let updateResult: SingleResult = { TestKey: 'key1', Name: 'updated' };
  let deleteResult = true;

  return {
    insertCalls,
    updateCalls,
    deleteCalls,
    setUpdateResult: (r: SingleResult) => {
      updateResult = r;
    },
    setDeleteResult: (r: boolean) => {
      deleteResult = r;
    },
    queryCollection: async (): Promise<CollectionResult> => ({ value: [] }),
    readByKey: async (): Promise<SingleResult> => undefined,
    insert: async (ctx: ResourceContext, record: Readonly<Record<string, unknown>>): Promise<EntityRecord> => {
      insertCalls.push({ ctx, record: { ...record } });
      return record;
    },
    update: async (ctx: ResourceContext, keyValue: string): Promise<SingleResult> => {
      updateCalls.push({ ctx, keyValue });
      return updateResult;
    },
    deleteByKey: async (ctx: ResourceContext, keyValue: string): Promise<boolean> => {
      deleteCalls.push({ ctx, keyValue });
      return deleteResult;
    }
  };
};

const makeMockWriter = (): EntityEventWriter & {
  readonly events: Array<{ resourceName: string; resourceRecordKey: string; resourceRecordUrl?: string }>;
} => {
  const events: Array<{ resourceName: string; resourceRecordKey: string; resourceRecordUrl?: string }> = [];
  return {
    events,
    writeEvent: async (resourceName: string, resourceRecordKey: string, resourceRecordUrl?: string): Promise<void> => {
      events.push({ resourceName, resourceRecordKey, resourceRecordUrl });
    }
  };
};

// ---------------------------------------------------------------------------
// DAL Decorator Tests
// ---------------------------------------------------------------------------

describe('createEntityEventDal', () => {
  it('writes an EntityEvent after a successful insert', async () => {
    const inner = makeMockDal();
    const writer = makeMockWriter();
    const dal = createEntityEventDal(inner, writer, { baseUrl: 'http://localhost', includeResourceRecordUrl: false });

    const ctx = makeResourceCtx('Property', 'ListingKey');
    await dal.insert(ctx, { ListingKey: 'abc-123', City: 'Austin' });

    expect(writer.events).toHaveLength(1);
    expect(writer.events[0]).toEqual({ resourceName: 'Property', resourceRecordKey: 'abc-123', resourceRecordUrl: undefined });
  });

  it('writes an EntityEvent after a successful update', async () => {
    const inner = makeMockDal();
    const writer = makeMockWriter();
    const dal = createEntityEventDal(inner, writer, { baseUrl: 'http://localhost', includeResourceRecordUrl: false });

    const ctx = makeResourceCtx('Property', 'ListingKey');
    await dal.update(ctx, 'abc-123', { City: 'Dallas' });

    expect(writer.events).toHaveLength(1);
    expect(writer.events[0]).toEqual({ resourceName: 'Property', resourceRecordKey: 'abc-123', resourceRecordUrl: undefined });
  });

  it('writes an EntityEvent after a successful delete', async () => {
    const inner = makeMockDal();
    const writer = makeMockWriter();
    const dal = createEntityEventDal(inner, writer, { baseUrl: 'http://localhost', includeResourceRecordUrl: false });

    const ctx = makeResourceCtx('Property', 'ListingKey');
    await dal.deleteByKey(ctx, 'abc-123');

    expect(writer.events).toHaveLength(1);
    expect(writer.events[0]).toEqual({ resourceName: 'Property', resourceRecordKey: 'abc-123', resourceRecordUrl: undefined });
  });

  it('does NOT write an EntityEvent when update returns undefined (not found)', async () => {
    const inner = makeMockDal();
    inner.setUpdateResult(undefined);
    const writer = makeMockWriter();
    const dal = createEntityEventDal(inner, writer, { baseUrl: 'http://localhost', includeResourceRecordUrl: false });

    const ctx = makeResourceCtx('Property', 'ListingKey');
    await dal.update(ctx, 'nonexistent', { City: 'Dallas' });

    expect(writer.events).toHaveLength(0);
  });

  it('does NOT write an EntityEvent when delete returns false (not found)', async () => {
    const inner = makeMockDal();
    inner.setDeleteResult(false);
    const writer = makeMockWriter();
    const dal = createEntityEventDal(inner, writer, { baseUrl: 'http://localhost', includeResourceRecordUrl: false });

    const ctx = makeResourceCtx('Property', 'ListingKey');
    await dal.deleteByKey(ctx, 'nonexistent');

    expect(writer.events).toHaveLength(0);
  });

  it('does NOT write an EntityEvent for writes to the EntityEvent resource itself', async () => {
    const inner = makeMockDal();
    const writer = makeMockWriter();
    const dal = createEntityEventDal(inner, writer, { baseUrl: 'http://localhost', includeResourceRecordUrl: false });

    const ctx = makeResourceCtx('EntityEvent', 'EntityEventSequence');
    await dal.insert(ctx, { EntityEventSequence: 1, ResourceName: 'Property', ResourceRecordKey: 'abc-123' });

    expect(writer.events).toHaveLength(0);
  });

  it('passes queryCollection and readByKey through unchanged', async () => {
    const inner = makeMockDal();
    const writer = makeMockWriter();
    const dal = createEntityEventDal(inner, writer, { baseUrl: 'http://localhost', includeResourceRecordUrl: false });

    expect(dal.queryCollection).toBe(inner.queryCollection);
    expect(dal.readByKey).toBe(inner.readByKey);
  });

  it('includes ResourceRecordUrl when configured', async () => {
    const inner = makeMockDal();
    const writer = makeMockWriter();
    const dal = createEntityEventDal(inner, writer, { baseUrl: 'http://localhost:8080', includeResourceRecordUrl: true });

    const ctx = makeResourceCtx('Property', 'ListingKey');
    await dal.insert(ctx, { ListingKey: 'abc-123', City: 'Austin' });

    expect(writer.events).toHaveLength(1);
    expect(writer.events[0].resourceRecordUrl).toBe("http://localhost:8080/Property('abc-123')");
  });

  it('omits ResourceRecordUrl when not configured', async () => {
    const inner = makeMockDal();
    const writer = makeMockWriter();
    const dal = createEntityEventDal(inner, writer, { baseUrl: 'http://localhost:8080', includeResourceRecordUrl: false });

    const ctx = makeResourceCtx('Property', 'ListingKey');
    await dal.insert(ctx, { ListingKey: 'abc-123' });

    expect(writer.events[0].resourceRecordUrl).toBeUndefined();
  });

  it('URL-encodes special characters in ResourceRecordKey', async () => {
    const inner = makeMockDal();
    const writer = makeMockWriter();
    const dal = createEntityEventDal(inner, writer, { baseUrl: 'http://localhost:8080', includeResourceRecordUrl: true });

    const ctx = makeResourceCtx('Property', 'ListingKey');
    await dal.insert(ctx, { ListingKey: "key with spaces & 'quotes'" });

    expect(writer.events[0].resourceRecordUrl).toBe("http://localhost:8080/Property('key%20with%20spaces%20%26%20'quotes'')");
  });
});

// ---------------------------------------------------------------------------
// Compaction Tests
// ---------------------------------------------------------------------------

describe('compaction', () => {
  it('startCompaction calls compact on the interval', async () => {
    vi.useFakeTimers();

    const compactFn = vi.fn().mockResolvedValue(3);
    const runner: CompactionRunner = { compact: compactFn };

    const timer = startCompaction(runner, 1000);

    await vi.advanceTimersByTimeAsync(1000);
    expect(compactFn).toHaveBeenCalledTimes(1);

    await vi.advanceTimersByTimeAsync(1000);
    expect(compactFn).toHaveBeenCalledTimes(2);

    stopCompaction(timer);
    vi.useRealTimers();
  });

  it('stopCompaction prevents further compact calls', async () => {
    vi.useFakeTimers();

    const compactFn = vi.fn().mockResolvedValue(0);
    const runner: CompactionRunner = { compact: compactFn };

    const timer = startCompaction(runner, 1000);
    stopCompaction(timer);

    await vi.advanceTimersByTimeAsync(5000);
    expect(compactFn).not.toHaveBeenCalled();

    vi.useRealTimers();
  });

  it('compaction errors are caught and logged (no crash)', async () => {
    vi.useFakeTimers();
    const consoleSpy = vi.spyOn(console, 'error').mockImplementation(() => {});

    const compactFn = vi.fn().mockRejectedValue(new Error('DB unavailable'));
    const runner: CompactionRunner = { compact: compactFn };

    const timer = startCompaction(runner, 1000);

    await vi.advanceTimersByTimeAsync(1000);
    expect(consoleSpy).toHaveBeenCalledWith('EntityEvent compaction error:', 'DB unavailable');

    stopCompaction(timer);
    consoleSpy.mockRestore();
    vi.useRealTimers();
  });
});

// ---------------------------------------------------------------------------
// Schema Tests
// ---------------------------------------------------------------------------

describe('EntityEvent schema generation', () => {
  it('PostgreSQL uses BIGSERIAL for EntityEventSequence', async () => {
    const { generateCreateTable } = await import('../src/db/schema-generator.js');
    const fields = [
      { resourceName: 'EntityEvent', fieldName: 'EntityEventSequence', type: 'Edm.Int64', annotations: [] },
      { resourceName: 'EntityEvent', fieldName: 'ResourceName', type: 'Edm.String', annotations: [] },
      { resourceName: 'EntityEvent', fieldName: 'ResourceRecordKey', type: 'Edm.String', annotations: [] }
    ];
    const ddl = generateCreateTable('EntityEvent', 'EntityEventSequence', fields);
    expect(ddl).toContain('BIGSERIAL NOT NULL PRIMARY KEY');
    expect(ddl).not.toContain('BIGINT');
  });

  it('PostgreSQL uses BIGINT for non-EntityEvent Int64 keys', async () => {
    const { generateCreateTable } = await import('../src/db/schema-generator.js');
    const fields = [{ resourceName: 'Other', fieldName: 'OtherKey', type: 'Edm.Int64', annotations: [] }];
    const ddl = generateCreateTable('Other', 'OtherKey', fields);
    expect(ddl).toContain('BIGINT');
    expect(ddl).not.toContain('BIGSERIAL');
  });

  it('SQLite uses INTEGER PRIMARY KEY AUTOINCREMENT for EntityEventSequence', async () => {
    const { generateCreateTable } = await import('../src/db/sqlite-schema-generator.js');
    const fields = [
      { resourceName: 'EntityEvent', fieldName: 'EntityEventSequence', type: 'Edm.Int64', annotations: [] },
      { resourceName: 'EntityEvent', fieldName: 'ResourceName', type: 'Edm.String', annotations: [] }
    ];
    const ddl = generateCreateTable('EntityEvent', 'EntityEventSequence', fields);
    expect(ddl).toContain('INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT');
  });

  it('PostgreSQL schema includes EntityEvent resource index', async () => {
    const { generateSchema } = await import('../src/db/schema-generator.js');
    const specs = [
      {
        resourceName: 'EntityEvent',
        keyField: 'EntityEventSequence',
        fields: [
          { resourceName: 'EntityEvent', fieldName: 'EntityEventSequence', type: 'Edm.Int64', annotations: [] },
          { resourceName: 'EntityEvent', fieldName: 'ResourceName', type: 'Edm.String', annotations: [] },
          { resourceName: 'EntityEvent', fieldName: 'ResourceRecordKey', type: 'Edm.String', annotations: [] }
        ]
      }
    ];
    const statements = generateSchema(specs);
    expect(statements.some(s => s.includes('idx_EntityEvent_resource'))).toBe(true);
  });
});
