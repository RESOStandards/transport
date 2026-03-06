/**
 * EntityEvent compaction — removes duplicate EntityEvent records, keeping
 * only the entry with the highest EntityEventSequence for each unique
 * (ResourceName, ResourceRecordKey) pair.
 *
 * Compaction is safe because consumers only need the latest event per
 * resource+key to know which records have changed. Earlier events for
 * the same record are redundant once a later event exists.
 *
 * Runtime: O(n) — single pass through the table using the
 * (ResourceName, ResourceRecordKey) index.
 */

import type Database from 'better-sqlite3';
import type { Db, Document } from 'mongodb';
import type pg from 'pg';

/** Runs a compaction pass and returns the number of removed records. */
export interface CompactionRunner {
  readonly compact: () => Promise<number>;
}

/**
 * Starts periodic compaction on a timer.
 * Returns the timer handle for cleanup.
 */
export const startCompaction = (runner: CompactionRunner, intervalMs: number): NodeJS.Timeout =>
  setInterval(async () => {
    try {
      const removed = await runner.compact();
      if (removed > 0) {
        console.log(`EntityEvent compaction: removed ${removed} superseded records.`);
      }
    } catch (err) {
      console.error('EntityEvent compaction error:', err instanceof Error ? err.message : err);
    }
  }, intervalMs);

/** Stops a running compaction timer. */
export const stopCompaction = (timer: NodeJS.Timeout): void => {
  clearInterval(timer);
};

// ---------------------------------------------------------------------------
// Backend-specific compaction runners
// ---------------------------------------------------------------------------

const COMPACTION_SQL = `
  DELETE FROM "EntityEvent"
  WHERE "EntityEventSequence" NOT IN (
    SELECT MAX("EntityEventSequence")
    FROM "EntityEvent"
    GROUP BY "ResourceName", "ResourceRecordKey"
  )
`;

/**
 * PostgreSQL compaction runner.
 *
 * Deletes all EntityEvent rows whose sequence is not the maximum for
 * their (ResourceName, ResourceRecordKey) group.
 */
export const createPostgresCompactionRunner = (pool: pg.Pool): CompactionRunner => ({
  compact: async (): Promise<number> => {
    const result = await pool.query(COMPACTION_SQL);
    return result.rowCount ?? 0;
  }
});

/**
 * SQLite compaction runner.
 *
 * Same SQL pattern as PostgreSQL — SQLite supports subqueries with NOT IN.
 */
export const createSqliteCompactionRunner = (db: Database.Database): CompactionRunner => ({
  compact: async (): Promise<number> => {
    const result = db.prepare(COMPACTION_SQL).run();
    return result.changes;
  }
});

/**
 * MongoDB compaction runner.
 *
 * Aggregates to find the max sequence per (ResourceName, ResourceRecordKey),
 * then deletes all documents not in that set.
 */
export const createMongoCompactionRunner = (db: Db): CompactionRunner => ({
  compact: async (): Promise<number> => {
    const latestEvents = await db
      .collection('EntityEvent')
      .aggregate([
        {
          $group: {
            _id: { rn: '$ResourceName', rk: '$ResourceRecordKey' },
            maxSeq: { $max: '$EntityEventSequence' }
          }
        }
      ])
      .toArray();

    if (latestEvents.length === 0) return 0;

    const keepSequences = (latestEvents as ReadonlyArray<Document>).map(e => e.maxSeq as number);
    const result = await db.collection('EntityEvent').deleteMany({
      EntityEventSequence: { $nin: keepSequences }
    });

    return result.deletedCount;
  }
});
