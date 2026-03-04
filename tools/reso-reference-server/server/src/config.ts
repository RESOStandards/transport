import { resolve } from 'node:path';

/** Database backend type. */
export type DbBackend = 'postgres' | 'mongodb';

/** Server configuration derived from environment variables with sensible defaults. */
export interface ServerConfig {
  readonly port: number;
  readonly dbBackend: DbBackend;
  readonly databaseUrl: string;
  readonly mongodbUrl: string;
  readonly metadataPath: string;
  readonly baseUrl: string;
}

/** Reads configuration from environment variables. */
export const loadConfig = (): ServerConfig => {
  const port = Number(process.env.PORT ?? 8080);
  const dbBackend = (process.env.DB_BACKEND ?? 'postgres') as DbBackend;
  const databaseUrl = process.env.DATABASE_URL ?? 'postgresql://reso:reso@localhost:5432/reso_reference';
  const mongodbUrl = process.env.MONGODB_URL ?? 'mongodb://localhost:27017/reso_reference';
  const metadataPath = process.env.METADATA_PATH ?? resolve(import.meta.dirname, '../server-metadata.json');
  const baseUrl = process.env.BASE_URL ?? `http://localhost:${port}`;

  if (dbBackend !== 'postgres' && dbBackend !== 'mongodb') {
    throw new Error(`Invalid DB_BACKEND: ${dbBackend}. Must be "postgres" or "mongodb".`);
  }

  return { port, dbBackend, databaseUrl, mongodbUrl, metadataPath, baseUrl };
};
