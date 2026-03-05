/**
 * SQLite database handle factory.
 *
 * Creates a better-sqlite3 database with WAL journal mode for concurrent
 * read performance and registers a REGEXP function for OData matchesPattern.
 */

import Database from 'better-sqlite3';

/** Creates and configures a SQLite database handle. */
export const createSqliteDb = (dbPath: string): Database.Database => {
  const db = new Database(dbPath);
  db.pragma('journal_mode = WAL');
  db.pragma('foreign_keys = ON');

  // Register REGEXP function for OData matchesPattern() support.
  // SQLite supports the "col REGEXP pattern" syntax when this function is defined.
  db.function('regexp', (pattern: unknown, str: unknown) => {
    if (typeof pattern !== 'string' || typeof str !== 'string') return 0;
    return new RegExp(pattern).test(str) ? 1 : 0;
  });

  return db;
};
