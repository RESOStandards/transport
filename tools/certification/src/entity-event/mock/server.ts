/**
 * Mock OData server for EntityEvent compliance testing.
 *
 * Starts an Express server with:
 * - /$metadata endpoint
 * - EntityEvent collection (read-only, with query support)
 * - Canary resource CRUD (triggers EntityEvent writes)
 * - OAuth2 token endpoint
 */
import type { Server } from 'node:http';
import express from 'express';
import {
  handleCanaryCreate,
  handleCanaryDelete,
  handleCanaryGet,
  handleCanaryQuery,
  handleCanaryUpdate,
  handleEntityEventByKey,
  handleEntityEventQuery,
  handleEntityEventWrite,
  handleMetadata,
  handleTokenEndpoint,
  setBaseUrl
} from './handlers.js';

export interface EntityEventMockServerOptions {
  readonly metadataXml: string;
  readonly canaryResource: string;
  readonly port?: number;
}

export const startMockEntityEventServer = async (
  options: EntityEventMockServerOptions
): Promise<{ readonly server: Server; readonly url: string }> => {
  const app = express();
  app.use(express.json());

  const { canaryResource, metadataXml } = options;
  const port = options.port ?? 0;

  // Metadata
  app.get(/^\/\$metadata$/, handleMetadata(metadataXml));

  // OAuth2
  app.post('/oauth/token', handleTokenEndpoint());

  // EntityEvent — read-only
  const eeKeyPattern = /^\/EntityEvent\('([^']+)'\)$/;
  app.get('/EntityEvent', handleEntityEventQuery());
  app.get(eeKeyPattern, handleEntityEventByKey());
  app.post('/EntityEvent', handleEntityEventWrite());
  app.patch(eeKeyPattern, handleEntityEventWrite());
  app.delete(eeKeyPattern, handleEntityEventWrite());

  // Canary resource — full CRUD
  const canaryKeyPattern = new RegExp(`^/${canaryResource}\\('([^']+)'\\)$`);
  app.get(`/${canaryResource}`, handleCanaryQuery(canaryResource));
  app.post(`/${canaryResource}`, handleCanaryCreate(canaryResource));
  app.patch(canaryKeyPattern, handleCanaryUpdate(canaryResource));
  app.delete(canaryKeyPattern, handleCanaryDelete(canaryResource));
  app.get(canaryKeyPattern, handleCanaryGet(canaryResource));

  return new Promise(resolve => {
    const server = app.listen(port, () => {
      const address = server.address();
      const assignedPort = typeof address === 'object' && address ? address.port : port;
      const url = `http://localhost:${assignedPort}`;
      setBaseUrl(url);
      resolve({ server, url });
    });
  });
};

export const stopMockEntityEventServer = async (server: Server): Promise<void> =>
  new Promise((resolve, reject) => {
    server.close(err => (err ? reject(err) : resolve()));
  });
