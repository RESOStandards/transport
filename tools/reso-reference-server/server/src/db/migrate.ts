import type pg from 'pg';

/** Runs an array of DDL statements against the database. Idempotent via CREATE TABLE IF NOT EXISTS. */
export const runMigrations = async (pool: pg.Pool, ddlStatements: ReadonlyArray<string>): Promise<void> => {
  for (const ddl of ddlStatements) {
    await pool.query(ddl);
  }
};
