import { resolve } from 'node:path';

/** Database backend type. */
export type DbBackend = 'postgres' | 'mongodb';

/** Enumeration mode: string enums with Lookup Resource, or OData EnumType definitions. */
export type EnumMode = 'string' | 'enum-type';

/** Server configuration derived from environment variables with sensible defaults. */
export interface ServerConfig {
  readonly port: number;
  readonly dbBackend: DbBackend;
  readonly enumMode: EnumMode;
  readonly databaseUrl: string;
  readonly mongodbUrl: string;
  readonly metadataPath: string;
  readonly baseUrl: string;
}

/** Reads configuration from environment variables. */
export const loadConfig = (): ServerConfig => {
  const port = Number(process.env.PORT ?? 8080);
  const dbBackend = (process.env.DB_BACKEND ?? 'postgres') as DbBackend;
  const enumMode = (process.env.ENUM_MODE ?? 'string') as EnumMode;
  const databaseUrl = process.env.DATABASE_URL ?? 'postgresql://reso:reso@localhost:5432/reso_reference';
  const mongodbUrl = process.env.MONGODB_URL ?? 'mongodb://localhost:27017/reso_reference';
  const metadataPath = process.env.METADATA_PATH ?? resolve(import.meta.dirname, '../server-metadata.json');
  const baseUrl = process.env.BASE_URL ?? `http://localhost:${port}`;

  if (dbBackend !== 'postgres' && dbBackend !== 'mongodb') {
    throw new Error(`Invalid DB_BACKEND: ${dbBackend}. Must be "postgres" or "mongodb".`);
  }

  if (enumMode !== 'string' && enumMode !== 'enum-type') {
    throw new Error(`Invalid ENUM_MODE: ${enumMode}. Must be "string" or "enum-type".`);
  }

  return { port, dbBackend, enumMode, databaseUrl, mongodbUrl, metadataPath, baseUrl };
};
