/**
 * Mock OData server for offline Add/Edit compliance testing.
 *
 * Starts an Express server with CRUD endpoints for a configurable OData resource,
 * plus a /$metadata endpoint that serves the provided XML metadata document.
 * Uses port 0 for OS-assigned ports in tests, or a fixed port (default 8800) for CLI usage.
 */
import express from "express";
import type { Server } from "node:http";
import type { MockServerOptions } from "../lib/types.js";
import {
  handleMetadata,
  handleCreate,
  handleUpdate,
  handleDelete,
  handleGet,
  handleTokenEndpoint,
} from "./handlers.js";

/**
 * Starts a mock OData server with CRUD routes for the specified resource.
 * Returns the HTTP server instance and the resolved URL (including the actual port).
 * Use port 0 for an OS-assigned ephemeral port (recommended for tests).
 */
export const startMockServer = async (
  options: MockServerOptions,
): Promise<{ readonly server: Server; readonly url: string }> => {
  const app = express();
  app.use(express.json());

  const { resource, metadataXml } = options;
  const port = options.port ?? 0;

  // Metadata endpoint — use regex because Express treats $ as special in strings
  app.get(/^\/\$metadata$/, handleMetadata(metadataXml));

  // OAuth2 token endpoint for Client Credentials grant
  app.post("/oauth/token", handleTokenEndpoint());

  // CRUD endpoints using regex for OData key syntax
  const keyPattern = new RegExp(`^/${resource}\\('([^']+)'\\)$`);

  app.post(`/${resource}`, handleCreate(resource));
  app.patch(keyPattern, handleUpdate(resource));
  app.delete(keyPattern, handleDelete());
  app.get(keyPattern, handleGet(resource));

  return new Promise((resolve) => {
    const server = app.listen(port, () => {
      const address = server.address();
      const assignedPort =
        typeof address === "object" && address ? address.port : port;
      resolve({
        server,
        url: `http://localhost:${assignedPort}`,
      });
    });
  });
};

/** Gracefully shuts down the mock server and releases the port. */
export const stopMockServer = async (server: Server): Promise<void> =>
  new Promise((resolve, reject) => {
    server.close((err) => (err ? reject(err) : resolve()));
  });
