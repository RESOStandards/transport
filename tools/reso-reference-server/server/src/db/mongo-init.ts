/**
 * MongoDB collection initialization — creates indexes for efficient queries.
 *
 * Analogous to PostgreSQL's migrate.ts + schema-generator.ts, but MongoDB
 * is schemaless so we only create indexes (no DDL).
 */

import type { Db } from 'mongodb';

/** Resource specification for MongoDB index creation. */
export interface MongoResourceSpec {
  readonly resourceName: string;
  readonly keyField: string;
  /** Whether this resource has ResourceRecordKey (child collections like Media). */
  readonly hasResourceRecordKey: boolean;
}

/**
 * Ensures MongoDB collections exist and creates required indexes.
 *
 * - Unique index on key field for each resource (e.g., ListingKey for Property)
 * - Compound index on (ResourceName, ResourceRecordKey) for child collections
 *   used by the RESO FK convention in $expand resolution
 */
export const initializeMongoCollections = async (db: Db, resourceSpecs: ReadonlyArray<MongoResourceSpec>): Promise<void> => {
  for (const spec of resourceSpecs) {
    const collection = db.collection(spec.resourceName);

    // Unique index on primary key
    await collection.createIndex({ [spec.keyField]: 1 }, { unique: true, name: `idx_${spec.resourceName}_pk` });

    // Compound index for RESO FK convention (child collections)
    if (spec.hasResourceRecordKey) {
      await collection.createIndex({ ResourceName: 1, ResourceRecordKey: 1 }, { name: `idx_${spec.resourceName}_fk` });
    }
  }
};
