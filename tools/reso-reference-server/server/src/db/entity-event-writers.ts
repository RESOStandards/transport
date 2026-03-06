/**
 * Backend-specific EntityEventWriter implementations.
 *
 * Each factory creates a writer that handles sequence generation and
 * persistence for its database backend. The writer is consumed by the
 * EntityEvent DAL decorator (entity-event-dal.ts).
 */

import type Database from 'better-sqlite3';
import type { Db, Document } from 'mongodb';
import type pg from 'pg';
import type { EntityEventWriter } from './entity-event-dal.js';

// ---------------------------------------------------------------------------
// PostgreSQL
// ---------------------------------------------------------------------------

/**
 * Creates a PostgreSQL EntityEventWriter.
 *
 * Uses BIGSERIAL auto-increment on the EntityEventSequence column —
 * the database assigns the next sequence number atomically.
 */
export const createPostgresEntityEventWriter = (pool: pg.Pool): EntityEventWriter => ({
  writeEvent: async (resourceName: string, resourceRecordKey: string, resourceRecordUrl?: string): Promise<void> => {
    const columns = ['"ResourceName"', '"ResourceRecordKey"'];
    const placeholders = ['$1', '$2'];
    const values: unknown[] = [resourceName, resourceRecordKey];

    if (resourceRecordUrl !== undefined) {
      columns.push('"ResourceRecordUrl"');
      placeholders.push('$3');
      values.push(resourceRecordUrl);
    }

    await pool.query(`INSERT INTO "EntityEvent" (${columns.join(', ')}) VALUES (${placeholders.join(', ')})`, values);
  }
});

// ---------------------------------------------------------------------------
// SQLite
// ---------------------------------------------------------------------------

/**
 * Creates a SQLite EntityEventWriter.
 *
 * Uses INTEGER PRIMARY KEY AUTOINCREMENT on EntityEventSequence —
 * the database assigns the next sequence number.
 */
export const createSqliteEntityEventWriter = (db: Database.Database): EntityEventWriter => ({
  writeEvent: async (resourceName: string, resourceRecordKey: string, resourceRecordUrl?: string): Promise<void> => {
    if (resourceRecordUrl !== undefined) {
      db.prepare('INSERT INTO "EntityEvent" ("ResourceName", "ResourceRecordKey", "ResourceRecordUrl") VALUES (?, ?, ?)').run(
        resourceName,
        resourceRecordKey,
        resourceRecordUrl
      );
    } else {
      db.prepare('INSERT INTO "EntityEvent" ("ResourceName", "ResourceRecordKey") VALUES (?, ?)').run(resourceName, resourceRecordKey);
    }
  }
});

// ---------------------------------------------------------------------------
// MongoDB
// ---------------------------------------------------------------------------

/**
 * Creates a MongoDB EntityEventWriter.
 *
 * Uses an atomic counter in the `counters` collection to generate
 * monotonically increasing EntityEventSequence values.
 */
export const createMongoEntityEventWriter = (db: Db): EntityEventWriter => ({
  writeEvent: async (resourceName: string, resourceRecordKey: string, resourceRecordUrl?: string): Promise<void> => {
    // Atomically increment and retrieve the next sequence number
    const counterResult = await db
      .collection('counters')
      .findOneAndUpdate({ _id: 'EntityEventSequence' } as Document, { $inc: { seq: 1 } }, { returnDocument: 'after', upsert: true });

    const seq = (counterResult as Document | null)?.seq ?? 1;

    const doc: Document = {
      EntityEventSequence: seq,
      ResourceName: resourceName,
      ResourceRecordKey: resourceRecordKey
    };

    if (resourceRecordUrl !== undefined) {
      doc.ResourceRecordUrl = resourceRecordUrl;
    }

    await db.collection('EntityEvent').insertOne(doc);
  }
});
