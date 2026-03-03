import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import express from 'express';
import { createMockOAuthRouter } from './auth/mock-oauth.js';
import { loadConfig } from './config.js';
import { runMigrations } from './db/migrate.js';
import { createPool } from './db/pool.js';
import { createPostgresDal } from './db/postgres-dal.js';
import { generateSchema } from './db/schema-generator.js';
import { createSwaggerRouter } from './docs/swagger.js';
import { generateEdmx } from './metadata/edmx-generator.js';
import { getFieldsForResource, getKeyFieldForResource, getLookupsForType, isEnumType, loadMetadata } from './metadata/loader.js';
import { generateOpenApiSpec } from './metadata/openapi-generator.js';
import { TARGET_RESOURCES } from './metadata/types.js';
import { createODataRouter } from './odata/router.js';

const main = async (): Promise<void> => {
  const config = loadConfig();
  console.log('RESO Reference Server starting...');
  console.log(`  Port: ${config.port}`);
  console.log(`  Database: ${config.databaseUrl.replace(/\/\/.*@/, '//***@')}`);
  console.log(`  Metadata: ${config.metadataPath}`);

  // Load metadata
  const metadata = await loadMetadata(config.metadataPath);
  console.log(`Loaded RESO metadata v${metadata.version}: ${metadata.fields.length} fields, ${metadata.lookups.length} lookups`);

  // Create database pool and run migrations
  const pool = createPool(config.databaseUrl);

  const resourceSpecs = TARGET_RESOURCES.map(resource => ({
    resourceName: resource,
    keyField: getKeyFieldForResource(resource)!,
    fields: getFieldsForResource(metadata, resource)
  })).filter(spec => spec.keyField && spec.fields.length > 0);

  const ddl = generateSchema(resourceSpecs);
  console.log(`Running migrations for ${resourceSpecs.length} resources...`);
  await runMigrations(pool, ddl);
  console.log('Database migrations complete.');

  // Create data access layer
  const dal = createPostgresDal(pool);

  // Generate EDMX metadata
  const edmxXml = generateEdmx(metadata, TARGET_RESOURCES);

  // Generate OpenAPI spec
  const openApiSpec = generateOpenApiSpec(metadata, TARGET_RESOURCES, config.baseUrl);

  // Build Express app
  const app = express();
  app.use(express.json({ limit: '10mb' }));

  // CORS middleware
  app.use((_req, res, next) => {
    res.set('Access-Control-Allow-Origin', '*');
    res.set('Access-Control-Allow-Methods', 'GET, POST, PATCH, DELETE, OPTIONS');
    res.set('Access-Control-Allow-Headers', 'Content-Type, Authorization, Prefer, OData-Version');
    if (_req.method === 'OPTIONS') {
      res.status(204).send();
      return;
    }
    next();
  });

  // $metadata endpoint — regex because Express treats $ as special
  app.get(/^\/\$metadata$/, (_req, res) => {
    res.type('application/xml').send(edmxXml);
  });

  // OAuth2 mock token endpoint
  app.use(createMockOAuthRouter());

  // OData CRUD + collection routes (using DAL instead of direct pool access)
  const odataRouter = createODataRouter(metadata, dal, config.baseUrl, TARGET_RESOURCES);
  app.use(odataRouter);

  // Swagger UI
  app.use(createSwaggerRouter(openApiSpec));

  // Static file serving (mock images for UI media carousel)
  const publicDir = resolve(import.meta.dirname, '../public');
  app.use(express.static(publicDir));

  // UI config endpoint — serves summary field configuration for the UI
  const uiConfigPath = resolve(import.meta.dirname, './ui-config.json');
  const uiConfig = JSON.parse(await readFile(uiConfigPath, 'utf-8'));
  app.get('/ui-config', (_req, res) => {
    res.json(uiConfig);
  });

  // Field groups endpoint — serves RESO Data Dictionary field groupings for the UI
  const fieldGroupsPath = resolve(import.meta.dirname, './field-groups.json');
  const fieldGroups = JSON.parse(await readFile(fieldGroupsPath, 'utf-8'));
  app.get('/field-groups', (_req, res) => {
    res.json(fieldGroups);
  });

  // Metadata JSON API — lightweight JSON alternatives to EDMX XML for UI consumption
  app.get('/api/metadata/fields', (req, res) => {
    const resource = req.query.resource as string | undefined;
    if (!resource) {
      res.status(400).json({ error: 'Missing required query parameter: resource' });
      return;
    }
    const fields = getFieldsForResource(metadata, resource);
    res.json(fields);
  });

  app.get('/api/metadata/lookups', (req, res) => {
    const type = req.query.type as string | undefined;
    if (!type) {
      res.status(400).json({ error: 'Missing required query parameter: type' });
      return;
    }
    const lookups = getLookupsForType(metadata, type);
    res.json(lookups);
  });

  app.get('/api/metadata/lookups-for-resource', (req, res) => {
    const resource = req.query.resource as string | undefined;
    if (!resource) {
      res.status(400).json({ error: 'Missing required query parameter: resource' });
      return;
    }
    const fields = getFieldsForResource(metadata, resource);
    const enumFields = fields.filter(f => isEnumType(f.type));
    const result: Record<string, unknown> = {};
    for (const field of enumFields) {
      const lookupName = field.type;
      if (!result[lookupName]) {
        result[lookupName] = getLookupsForType(metadata, lookupName);
      }
    }
    res.json(result);
  });

  // Health check
  app.get('/health', (_req, res) => {
    res.json({ status: 'ok', version: metadata.version });
  });

  // Start server
  app.listen(config.port, () => {
    console.log(`\nRESO Reference Server running at ${config.baseUrl}`);
    console.log(`  API docs: ${config.baseUrl}/api-docs`);
    console.log(`  Metadata: ${config.baseUrl}/$metadata`);
    console.log(`  Health:   ${config.baseUrl}/health`);
  });
};

main().catch(err => {
  console.error('Failed to start server:', err);
  process.exit(1);
});
