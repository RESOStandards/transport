/**
 * MongoDB Data Access Layer — EXAMPLE IMPLEMENTATION SKETCH
 *
 * This file demonstrates how the DataAccessLayer interface would be
 * implemented for a document store like MongoDB. It is NOT production-ready
 * and is not imported by any server code.
 *
 * Key differences from the PostgreSQL implementation:
 *
 * 1. No JOINs — instead, uses batch queries per navigation property.
 *    For each parent entity that matches the query, we collect parent keys,
 *    then issue a single query per navigation property to fetch all related
 *    documents, then stitch results together in application code.
 *
 * 2. $filter translation targets MongoDB query operators ($eq, $gt, $regex)
 *    instead of SQL WHERE clauses.
 *
 * 3. Collection fields are stored natively as arrays (no JSON serialization).
 *
 * @example
 * ```ts
 * import { MongoClient } from "mongodb";
 * import { createMongoDal } from "./mongo-dal.example.js";
 *
 * const client = new MongoClient("mongodb://localhost:27017");
 * const db = client.db("reso_reference");
 * const dal = createMongoDal(db);
 * ```
 */

/* eslint-disable @typescript-eslint/no-unused-vars */

import type {
  DataAccessLayer,
  ResourceContext,
  CollectionQueryOptions,
  CollectionResult,
  SingleResult,
  EntityRecord,
  NavigationPropertyBinding,
} from "./data-access.js";

// ---------------------------------------------------------------------------
// MongoDB type stubs (would come from the "mongodb" package)
// ---------------------------------------------------------------------------

/** Stub for mongodb.Db — replace with actual import in production. */
interface MongoDb {
  readonly collection: (name: string) => MongoCollection;
}

/** Stub for mongodb.Collection */
interface MongoCollection {
  readonly find: (filter: Record<string, unknown>, options?: Record<string, unknown>) => MongoCursor;
  readonly findOne: (filter: Record<string, unknown>, options?: Record<string, unknown>) => Promise<Record<string, unknown> | null>;
  readonly insertOne: (doc: Record<string, unknown>) => Promise<{ readonly insertedId: unknown }>;
  readonly updateOne: (
    filter: Record<string, unknown>,
    update: Record<string, unknown>,
  ) => Promise<{ readonly matchedCount: number }>;
  readonly deleteOne: (filter: Record<string, unknown>) => Promise<{ readonly deletedCount: number }>;
  readonly countDocuments: (filter: Record<string, unknown>) => Promise<number>;
}

/** Stub for mongodb.Cursor */
interface MongoCursor {
  readonly sort: (spec: Record<string, unknown>) => MongoCursor;
  readonly skip: (n: number) => MongoCursor;
  readonly limit: (n: number) => MongoCursor;
  readonly project: (spec: Record<string, unknown>) => MongoCursor;
  readonly toArray: () => Promise<Record<string, unknown>[]>;
}

// ---------------------------------------------------------------------------
// $filter → MongoDB query translation (sketch)
// ---------------------------------------------------------------------------

/**
 * Would translate an OData $filter AST to a MongoDB query document.
 *
 * Examples:
 *   "ListPrice gt 200000"        → { ListPrice: { $gt: 200000 } }
 *   "City eq 'Austin'"           → { City: "Austin" }
 *   "contains(City, 'Aus')"      → { City: { $regex: "Aus", $options: "i" } }
 *   "ListPrice gt 100 and City eq 'Austin'"
 *     → { $and: [{ ListPrice: { $gt: 100 } }, { City: "Austin" }] }
 */
const filterToMongoQuery = (
  _filterString: string,
): Record<string, unknown> => {
  // In a real implementation, this would:
  // 1. Parse the filter string using parseFilter() from @reso/odata-filter-parser
  // 2. Walk the AST and build a MongoDB query document
  // 3. Map OData operators to MongoDB: eq→$eq, ne→$ne, gt→$gt, etc.
  // 4. Map functions: contains→$regex, startswith→$regex with ^, etc.
  throw new Error("Not implemented — this is an example sketch");
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
  db: MongoDb,
  parentResource: string,
  parentKeyField: string,
  parents: ReadonlyArray<EntityRecord>,
  bindings: ReadonlyArray<NavigationPropertyBinding>,
): Promise<ReadonlyArray<EntityRecord>> => {
  if (bindings.length === 0 || parents.length === 0) return parents;

  // Collect all parent key values
  const parentKeys = parents.map((p) => String(p[parentKeyField] ?? ""));

  // For each navigation property, fetch all related documents in one query
  const navResults = new Map<string, Map<string, Record<string, unknown>[]>>();

  for (const binding of bindings) {
    const collection = db.collection(binding.targetResource);
    const fk = binding.foreignKey;

    let filter: Record<string, unknown>;
    if (fk.strategy === "resource-record-key") {
      filter = {
        ResourceName: parentResource,
        ResourceRecordKey: { $in: parentKeys },
      };
    } else {
      const targetCol = fk.targetColumn ?? parentKeyField;
      filter = { [targetCol]: { $in: parentKeys } };
    }

    const docs = await collection.find(filter).toArray();

    // Group by parent key
    const grouped = new Map<string, Record<string, unknown>[]>();
    for (const doc of docs) {
      const parentKey =
        fk.strategy === "resource-record-key"
          ? String(doc["ResourceRecordKey"] ?? "")
          : String(doc[fk.targetColumn ?? parentKeyField] ?? "");
      if (!grouped.has(parentKey)) {
        grouped.set(parentKey, []);
      }
      grouped.get(parentKey)!.push(doc);
    }

    navResults.set(binding.name, grouped);
  }

  // Stitch navigation results into parent entities
  return parents.map((parent) => {
    const parentKey = String(parent[parentKeyField] ?? "");
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
// MongoDB DAL implementation (sketch)
// ---------------------------------------------------------------------------

/**
 * Creates a MongoDB Data Access Layer implementation.
 *
 * NOTE: This is an example sketch. In production you would:
 * - Implement filterToMongoQuery() to translate $filter ASTs
 * - Add proper error handling and connection management
 * - Handle MongoDB _id field mapping
 * - Add indexes for ResourceName + ResourceRecordKey on child collections
 *
 * Pagination note: Unlike the PostgreSQL adapter (which requires a CTE to
 * paginate parent rows before LEFT JOINing expanded navprops), the MongoDB
 * adapter naturally handles this correctly — $skip/$limit apply to the parent
 * cursor first, then batchExpandNavigation() resolves navigation properties
 * against the already-paginated parent set. The @odata.nextLink is generated
 * at the handler layer (handlers.ts) using the CollectionResult from the DAL,
 * so no nextLink logic is needed here.
 *
 * TODO: Implement filterToMongoQuery() using @reso/odata-filter-parser AST
 * TODO: Add $orderby validation against field definitions
 * TODO: Ensure countDocuments uses the same filter for accurate $count
 * TODO: Handle _id suppression in projection when no $select is specified
 */
export const createMongoDal = (db: MongoDb): DataAccessLayer => {
  const queryCollection = async (
    ctx: ResourceContext,
    options?: CollectionQueryOptions,
  ): Promise<CollectionResult> => {
    const collection = db.collection(ctx.resource);

    // Build MongoDB filter from $filter
    const filter = options?.$filter
      ? filterToMongoQuery(options.$filter)
      : {};

    // Build projection from $select
    const projection: Record<string, number> = { _id: 0 };
    if (options?.$select) {
      const fields = options.$select.split(",").map((s) => s.trim());
      for (const f of fields) projection[f] = 1;
      projection[ctx.keyField] = 1; // always include key
    }

    // Start cursor
    let cursor = collection.find(filter);

    if (Object.keys(projection).length > 1) {
      cursor = cursor.project(projection);
    }

    // $orderby
    if (options?.$orderby) {
      const sort: Record<string, unknown> = {};
      for (const part of options.$orderby.split(",")) {
        const [field, dir] = part.trim().split(/\s+/);
        if (field) sort[field] = dir?.toLowerCase() === "desc" ? -1 : 1;
      }
      cursor = cursor.sort(sort);
    }

    // $skip / $top
    if (options?.$skip !== undefined) cursor = cursor.skip(options.$skip);
    if (options?.$top !== undefined) cursor = cursor.limit(options.$top);

    const docs = await cursor.toArray();

    // $count
    let count: number | undefined;
    if (options?.$count) {
      count = await collection.countDocuments(filter);
    }

    // $expand — batch query per navigation property
    let entities: ReadonlyArray<EntityRecord> = docs;
    if (options?.$expand) {
      const expandNames = new Set(
        options.$expand.split(",").map((s) => s.trim()),
      );
      const bindings = ctx.navigationBindings.filter((b) =>
        expandNames.has(b.name),
      );
      entities = await batchExpandNavigation(
        db,
        ctx.resource,
        ctx.keyField,
        docs,
        bindings,
      );
    }

    return { value: entities, ...(count !== undefined ? { count } : {}) };
  };

  const readByKey = async (
    ctx: ResourceContext,
    keyValue: string,
    options?: { readonly $select?: string; readonly $expand?: string },
  ): Promise<SingleResult> => {
    const collection = db.collection(ctx.resource);
    const doc = await collection.findOne({ [ctx.keyField]: keyValue });
    if (!doc) return undefined;

    // Apply $select
    let entity: EntityRecord = doc;
    if (options?.$select) {
      const fields = new Set(options.$select.split(",").map((s) => s.trim()));
      fields.add(ctx.keyField);
      entity = Object.fromEntries(
        Object.entries(doc).filter(([k]) => fields.has(k)),
      );
    }

    // Apply $expand
    if (options?.$expand) {
      const expandNames = new Set(
        options.$expand.split(",").map((s) => s.trim()),
      );
      const bindings = ctx.navigationBindings.filter((b) =>
        expandNames.has(b.name),
      );
      const [expanded] = await batchExpandNavigation(
        db,
        ctx.resource,
        ctx.keyField,
        [entity],
        bindings,
      );
      return expanded;
    }

    return entity;
  };

  const insert = async (
    ctx: ResourceContext,
    record: Readonly<Record<string, unknown>>,
  ): Promise<EntityRecord> => {
    const collection = db.collection(ctx.resource);
    await collection.insertOne({ ...record });
    // Return the record as-is (MongoDB adds _id but we project it out)
    return record;
  };

  const update = async (
    ctx: ResourceContext,
    keyValue: string,
    updates: Readonly<Record<string, unknown>>,
  ): Promise<SingleResult> => {
    const collection = db.collection(ctx.resource);
    const result = await collection.updateOne(
      { [ctx.keyField]: keyValue },
      { $set: updates },
    );
    if (result.matchedCount === 0) return undefined;
    return collection.findOne({ [ctx.keyField]: keyValue }) as Promise<SingleResult>;
  };

  const deleteByKey = async (
    ctx: ResourceContext,
    keyValue: string,
  ): Promise<boolean> => {
    const collection = db.collection(ctx.resource);
    const result = await collection.deleteOne({ [ctx.keyField]: keyValue });
    return result.deletedCount > 0;
  };

  return { queryCollection, readByKey, insert, update, deleteByKey };
};
