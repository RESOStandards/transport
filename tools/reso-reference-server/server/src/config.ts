import { resolve } from 'node:path';

/** Server configuration derived from environment variables with sensible defaults. */
export interface ServerConfig {
  readonly port: number;
  readonly databaseUrl: string;
  readonly metadataPath: string;
  readonly baseUrl: string;
}

/** Reads configuration from environment variables. */
export const loadConfig = (): ServerConfig => {
  const port = Number(process.env.PORT ?? 8080);
  const databaseUrl = process.env.DATABASE_URL ?? 'postgresql://reso:reso@localhost:5432/reso_reference';
  const metadataPath = process.env.METADATA_PATH ?? resolve(import.meta.dirname, '../server-metadata.json');
  const baseUrl = process.env.BASE_URL ?? `http://localhost:${port}`;

  return { port, databaseUrl, metadataPath, baseUrl };
};
