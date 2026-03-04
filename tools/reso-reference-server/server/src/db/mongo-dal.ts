/**
 * MongoDB Data Access Layer implementation.
 *
 * Key differences from the PostgreSQL implementation:
 *
 * 1. No JOINs — uses batch queries per navigation property for $expand.
 *    For each parent entity that matches the query, we collect parent keys,
 *    then issue a single query per navigation property to fetch all related
 *    documents, then stitch results together in application code.
 *
 * 2. $filter translation targets MongoDB query operators ($eq, $gt, $regex)
 *    via the filterToMongo() translator in filter-to-mongo.ts.
 *
 * 3. Collection fields are stored natively as arrays (no JSON serialization).
 *
 * 4. Pagination is naturally correct — $skip/$limit apply to the parent
 *    cursor first, then batchExpandNavigation() resolves navigation properties
 *    against the already-paginated parent set. No CTE needed (unlike PostgreSQL).
 */

import type { Db } from 'mongodb';
import type {
  CollectionQueryOptions,
  CollectionResult,
  DataAccessLayer,
  EntityRecord,
  NavigationPropertyBinding,
  ResourceContext,
  SingleResult
} from './data-access.js';
import { filterToMongo } from './filter-to-mongo.js';

// ---------------------------------------------------------------------------
// _id suppression helper
// ---------------------------------------------------------------------------

/** Remove MongoDB's _id field from a document. */
const stripId = (doc: Record<string, unknown>): Record<string, unknown> => {
  const { _id: _, ...rest } = doc;
  return rest;
};

// ---------------------------------------------------------------------------
// Batch expand resolution (document store pattern)
// ---------------------------------------------------------------------------

/**
 * Resolve navigation properties for a set of parent entities using batch
 * queries. Unlike the PostgreSQL LEFT JOIN approach, this issues one query
 * per navigation property, collecting all parent keys first.
 *
 * For example, if we have 10 Property entities and need to expand Media:
 * 1. Collect all 10 ListingKey values
 * 2. Query Media WHERE ResourceName = "Property" AND ResourceRecordKey IN (keys)
 * 3. Group results by ResourceRecordKey and attach to parent entities
 *
 * This avoids the N+1 problem without requiring JOIN support.
 */
const batchExpandNavigation = async (
  db: Db,
  parentResource: string,
  parentKeyField: string,
  parents: ReadonlyArray<EntityRecord>,
  bindings: ReadonlyArray<NavigationPropertyBinding>
): Promise<ReadonlyArray<EntityRecord>> => {
  if (bindings.length === 0 || parents.length === 0) return parents;

  // Collect all parent key values
  const parentKeys = parents.map(p => String(p[parentKeyField] ?? ''));

  // For each navigation property, fetch all related documents in one query
  const navResults = new Map<string, Map<string, Record<string, unknown>[]>>();

  for (const binding of bindings) {
    const collection = db.collection(binding.targetResource);
    const fk = binding.foreignKey;

    let filter: Record<string, unknown>;
    if (fk.strategy === 'resource-record-key') {
      filter = {
        ResourceName: parentResource,
        ResourceRecordKey: { $in: parentKeys }
      };
    } else {
      const targetCol = fk.targetColumn ?? parentKeyField;
      filter = { [targetCol]: { $in: parentKeys } };
    }

    const docs = await collection.find(filter, { projection: { _id: 0 } }).toArray();

    // Group by parent key
    const grouped = new Map<string, Record<string, unknown>[]>();
    for (const doc of docs) {
      const parentKey =
        fk.strategy === 'resource-record-key' ? String(doc.ResourceRecordKey ?? '') : String(doc[fk.targetColumn ?? parentKeyField] ?? '');
      if (!grouped.has(parentKey)) {
        grouped.set(parentKey, []);
      }
      grouped.get(parentKey)!.push(doc);
    }

    navResults.set(binding.name, grouped);
  }

  // Stitch navigation results into parent entities
  return parents.map(parent => {
    const parentKey = String(parent[parentKeyField] ?? '');
    const expanded: Record<string, unknown> = { ...parent };

    for (const binding of bindings) {
      const grouped = navResults.get(binding.name);
      const related = grouped?.get(parentKey) ?? [];

      if (binding.isCollection) {
        expanded[binding.name] = related;
      } else {
        expanded[binding.name] = related[0] ?? null;
      }
    }

    return expanded;
  });
};

// ---------------------------------------------------------------------------
// MongoDB DAL implementation
// ---------------------------------------------------------------------------

/**
 * Creates a MongoDB Data Access Layer implementation.
 *
 * @param db - MongoDB Db instance (from MongoClient.db())
 */
export const createMongoDal = (db: Db): DataAccessLayer => {
  const queryCollection = async (ctx: ResourceContext, options?: CollectionQueryOptions): Promise<CollectionResult> => {
    const collection = db.collection(ctx.resource);

    // Build MongoDB filter from $filter
    const filter = options?.$filter ? filterToMongo(options.$filter, ctx.fields).query : {};

    // Build projection from $select (always suppress _id)
    const projection: Record<string, number> = { _id: 0 };
    if (options?.$select) {
      const selectFields = options.$select.split(',').map(s => s.trim());
      for (const f of selectFields) projection[f] = 1;
      projection[ctx.keyField] = 1; // always include key
    }

    // Start cursor
    let cursor = collection.find(filter);

    // Always apply projection (at minimum to suppress _id)
    cursor = cursor.project(projection);

    // $orderby — validate field names and build sort spec
    if (options?.$orderby) {
      const fieldNames = new Set(ctx.fields.map(f => f.fieldName));
      const sort: Record<string, 1 | -1> = {};
      for (const part of options.$orderby.split(',')) {
        const [fieldName, dir] = part.trim().split(/\s+/);
        if (fieldName) {
          if (!fieldNames.has(fieldName)) {
            throw new Error(`Unknown field in $orderby: ${fieldName}`);
          }
          sort[fieldName] = dir?.toLowerCase() === 'desc' ? -1 : 1;
        }
      }
      cursor = cursor.sort(sort);
    }

    // $skip / $top — pagination applies to parent cursor (naturally correct)
    if (options?.$skip !== undefined) cursor = cursor.skip(options.$skip);
    if (options?.$top !== undefined) cursor = cursor.limit(options.$top);

    const docs = (await cursor.toArray()) as Record<string, unknown>[];

    // $count — uses the same filter for accurate count
    let count: number | undefined;
    if (options?.$count) {
      count = await collection.countDocuments(filter);
    }

    // $expand — batch query per navigation property
    let entities: ReadonlyArray<EntityRecord> = docs;
    if (options?.$expand) {
      const expandNames = new Set(options.$expand.split(',').map(s => s.trim()));
      const bindings = ctx.navigationBindings.filter(b => expandNames.has(b.name));
      entities = await batchExpandNavigation(db, ctx.resource, ctx.keyField, docs, bindings);
    }

    return { value: entities, ...(count !== undefined ? { count } : {}) };
  };

  const readByKey = async (
    ctx: ResourceContext,
    keyValue: string,
    options?: { readonly $select?: string; readonly $expand?: string }
  ): Promise<SingleResult> => {
    const collection = db.collection(ctx.resource);
    const doc = await collection.findOne({ [ctx.keyField]: keyValue }, { projection: { _id: 0 } });
    if (!doc) return undefined;

    // Apply $select
    let entity: EntityRecord = doc;
    if (options?.$select) {
      const selectFields = new Set(options.$select.split(',').map(s => s.trim()));
      selectFields.add(ctx.keyField);
      entity = Object.fromEntries(Object.entries(doc).filter(([k]) => selectFields.has(k)));
    }

    // Apply $expand
    if (options?.$expand) {
      const expandNames = new Set(options.$expand.split(',').map(s => s.trim()));
      const bindings = ctx.navigationBindings.filter(b => expandNames.has(b.name));
      const [expanded] = await batchExpandNavigation(db, ctx.resource, ctx.keyField, [entity], bindings);
      return expanded;
    }

    return entity;
  };

  const insert = async (ctx: ResourceContext, record: Readonly<Record<string, unknown>>): Promise<EntityRecord> => {
    const collection = db.collection(ctx.resource);
    await collection.insertOne({ ...record });
    // Return the record without _id
    return stripId(record as Record<string, unknown>);
  };

  const update = async (ctx: ResourceContext, keyValue: string, updates: Readonly<Record<string, unknown>>): Promise<SingleResult> => {
    const collection = db.collection(ctx.resource);
    const result = await collection.updateOne({ [ctx.keyField]: keyValue }, { $set: updates });
    if (result.matchedCount === 0) return undefined;
    const updated = await collection.findOne({ [ctx.keyField]: keyValue }, { projection: { _id: 0 } });
    return updated as SingleResult;
  };

  const deleteByKey = async (ctx: ResourceContext, keyValue: string): Promise<boolean> => {
    const collection = db.collection(ctx.resource);
    const result = await collection.deleteOne({ [ctx.keyField]: keyValue });
    return result.deletedCount > 0;
  };

  return { queryCollection, readByKey, insert, update, deleteByKey };
};
