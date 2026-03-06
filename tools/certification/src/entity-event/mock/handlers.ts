/**
 * Mock OData server handlers for EntityEvent compliance testing.
 *
 * Maintains in-memory EntityEvent and canary resource stores.
 * EntityEvent entries are auto-generated when canary records are created/updated/deleted.
 */
import type { Request, Response } from 'express';

// ── State ──

/** In-memory EntityEvent store, ordered by sequence. */
let entityEvents: Array<{
  EntityEventSequence: number;
  ResourceName: string;
  ResourceRecordKey: string;
  ResourceRecordUrl?: string;
}> = [];

let nextSequence = 1;

/** In-memory canary resource store, keyed by record key. */
const canaryStore = new Map<string, Record<string, unknown>>();

/** Base URL, set when the server starts. */
let baseUrl = 'http://localhost:8800';

// ── Helpers ──

const extractKey = (path: string): string | undefined => {
  const match = path.match(/\('([^']+)'\)/);
  return match?.[1];
};

const addEntityEvent = (resourceName: string, resourceRecordKey: string): void => {
  entityEvents.push({
    EntityEventSequence: nextSequence++,
    ResourceName: resourceName,
    ResourceRecordKey: resourceRecordKey,
    ResourceRecordUrl: `${baseUrl}/${resourceName}('${resourceRecordKey}')`
  });
};

/**
 * Parses a simple OData $filter string. Supports:
 * - `FieldName eq 'value'` (string equality)
 * - `FieldName eq value` (numeric equality)
 * - `FieldName gt value` (numeric greater than)
 * - `FieldName in ('v1','v2','v3')` (set membership)
 * - Combined with `and`
 */
const matchesFilter = (record: Record<string, unknown>, filter: string): boolean => {
  // Split on ' and ' (simple — doesn't handle nested parens in general, but works for our cases)
  const clauses = filter.split(' and ');
  return clauses.every(clause => {
    const trimmed = clause.trim();

    // Handle `in` operator: FieldName in ('v1','v2')
    const inMatch = trimmed.match(/^(\w+)\s+in\s+\((.+)\)$/);
    if (inMatch) {
      const field = inMatch[1];
      const values = inMatch[2].split(',').map(v => v.trim().replace(/^'|'$/g, ''));
      return values.includes(String(record[field]));
    }

    // Handle `gt` operator: FieldName gt value
    const gtMatch = trimmed.match(/^(\w+)\s+gt\s+(\d+)$/);
    if (gtMatch) {
      const field = gtMatch[1];
      const threshold = Number(gtMatch[2]);
      return Number(record[field]) > threshold;
    }

    // Handle `eq` operator: FieldName eq 'value' or FieldName eq value
    const eqMatch = trimmed.match(/^(\w+)\s+eq\s+'?([^']*)'?$/);
    if (eqMatch) {
      const field = eqMatch[1];
      const expected = eqMatch[2];
      return String(record[field]) === expected;
    }

    return true;
  });
};

// ── Public Configuration ──

export const setBaseUrl = (url: string): void => {
  baseUrl = url;
};

export const resetStore = (): void => {
  entityEvents = [];
  nextSequence = 1;
  canaryStore.clear();
};

export const seedEntityEvents = (
  events: ReadonlyArray<{
    ResourceName: string;
    ResourceRecordKey: string;
  }>
): void => {
  for (const e of events) {
    addEntityEvent(e.ResourceName, e.ResourceRecordKey);
  }
};

export const seedCanaryRecord = (key: string, record: Record<string, unknown>): void => {
  canaryStore.set(key, record);
};

// ── Route Handlers ──

export const handleMetadata =
  (metadataXml: string) =>
  (_req: Request, res: Response): void => {
    res.type('application/xml').send(metadataXml);
  };

export const handleTokenEndpoint =
  () =>
  (_req: Request, res: Response): void => {
    res.json({ access_token: 'mock-access-token', token_type: 'Bearer', expires_in: 3600 });
  };

/** GET /EntityEvent — collection query with $filter, $orderby, $top, $skip, $count. */
export const handleEntityEventQuery =
  () =>
  (req: Request, res: Response): void => {
    let results = [...entityEvents];

    // $filter
    const filter = req.query.$filter as string | undefined;
    if (filter) {
      results = results.filter(r => matchesFilter(r as unknown as Record<string, unknown>, filter));
    }

    // $orderby
    const orderby = req.query.$orderby as string | undefined;
    if (orderby) {
      const desc = orderby.includes('desc');
      results.sort((a, b) => (desc ? b.EntityEventSequence - a.EntityEventSequence : a.EntityEventSequence - b.EntityEventSequence));
    }

    // $count
    const includeCount = req.query.$count === 'true';
    const totalCount = results.length;

    // $skip
    const skip = Number(req.query.$skip) || 0;
    if (skip > 0) {
      results = results.slice(skip);
    }

    // $top
    const top = Number(req.query.$top);
    if (top > 0) {
      results = results.slice(0, top);
    }

    const body: Record<string, unknown> = {
      '@odata.context': `${baseUrl}/$metadata#EntityEvent`,
      value: results
    };

    if (includeCount) {
      body['@odata.count'] = totalCount;
    }

    res.set('OData-Version', '4.01');
    res.json(body);
  };

/** GET /EntityEvent('key') — single entity by key. */
export const handleEntityEventByKey =
  () =>
  (req: Request, res: Response): void => {
    const key = extractKey(req.path);
    const event = entityEvents.find(e => String(e.EntityEventSequence) === key);

    res.set('OData-Version', '4.01');
    if (event) {
      res.json({ '@odata.context': `${baseUrl}/$metadata#EntityEvent/$entity`, ...event });
    } else {
      res.status(404).send();
    }
  };

/** POST/PATCH/DELETE /EntityEvent — always 405 (read-only resource). */
export const handleEntityEventWrite =
  () =>
  (_req: Request, res: Response): void => {
    res.set('OData-Version', '4.01');
    res.status(405).json({
      error: { code: '405', message: 'EntityEvent is a read-only resource' }
    });
  };

/** POST /{canaryResource} — create a record. */
export const handleCanaryCreate =
  (resource: string) =>
  (req: Request, res: Response): void => {
    const body = req.body as Record<string, unknown>;
    const key = String(body.ListingKey ?? Date.now());
    const record = { ...body, ListingKey: key, ModificationTimestamp: new Date().toISOString() };
    canaryStore.set(key, record);
    addEntityEvent(resource, key);

    const locationUrl = `${baseUrl}/${resource}('${key}')`;
    res.set({ 'OData-Version': '4.01', Location: locationUrl, EntityId: key, 'Preference-Applied': 'return=representation' });
    res.status(201).json({
      '@odata.context': `${baseUrl}/$metadata#${resource}/$entity`,
      '@odata.id': locationUrl,
      '@odata.editLink': locationUrl,
      ...record
    });
  };

/** PATCH /{canaryResource}('key') — update a record. */
export const handleCanaryUpdate =
  (resource: string) =>
  (req: Request, res: Response): void => {
    const key = extractKey(req.path);
    if (!key || !canaryStore.has(key)) {
      res.set('OData-Version', '4.01');
      res.status(404).send();
      return;
    }
    const existing = canaryStore.get(key)!;
    const updated = { ...existing, ...(req.body as Record<string, unknown>), ModificationTimestamp: new Date().toISOString() };
    canaryStore.set(key, updated);
    addEntityEvent(resource, key);

    res.set({ 'OData-Version': '4.01', 'Preference-Applied': 'return=minimal' });
    res.status(204).send();
  };

/** DELETE /{canaryResource}('key') — delete a record. */
export const handleCanaryDelete =
  (resource: string) =>
  (req: Request, res: Response): void => {
    const key = extractKey(req.path);
    if (!key || !canaryStore.has(key)) {
      res.set('OData-Version', '4.01');
      res.status(404).send();
      return;
    }
    canaryStore.delete(key);
    addEntityEvent(resource, key);

    res.set('OData-Version', '4.01');
    res.status(204).send();
  };

/** GET /{canaryResource}('key') — read a single record. */
export const handleCanaryGet =
  (resource: string) =>
  (req: Request, res: Response): void => {
    const key = extractKey(req.path);
    if (!key || !canaryStore.has(key)) {
      res.set('OData-Version', '4.01');
      res.status(404).send();
      return;
    }
    const record = canaryStore.get(key)!;
    res.set('OData-Version', '4.01');
    res.json({
      '@odata.context': `${baseUrl}/$metadata#${resource}/$entity`,
      ...record
    });
  };

/** GET /{canaryResource} — collection query with $filter support. */
export const handleCanaryQuery =
  (resource: string) =>
  (req: Request, res: Response): void => {
    let records = [...canaryStore.values()];

    const filter = req.query.$filter as string | undefined;
    if (filter) {
      records = records.filter(r => matchesFilter(r, filter));
    }

    res.set('OData-Version', '4.01');
    res.json({
      '@odata.context': `${baseUrl}/$metadata#${resource}`,
      value: records
    });
  };
