import pg from "pg";

/** Creates a new PostgreSQL connection pool from a connection string. */
export const createPool = (connectionString: string): pg.Pool =>
  new pg.Pool({ connectionString });
