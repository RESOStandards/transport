import { randomUUID } from 'node:crypto';
import express, { Router } from 'express';
import type { AuthRole } from './config.js';
import { registerDynamicToken } from './config.js';

/**
 * Creates an Express router with a mock OAuth2 Client Credentials endpoint.
 * Accepts any client_id/client_secret and returns a mock access token.
 *
 * Supports an optional `role` query parameter to assign the token a specific
 * auth role (default: "write"). The token is registered in the dynamic token
 * store so the auth middleware can resolve it.
 */
export const createMockOAuthRouter = (): Router => {
  const router = Router();

  // OAuth2 Client Credentials uses application/x-www-form-urlencoded
  router.use(express.urlencoded({ extended: false }));

  router.post('/oauth/token', (req, res) => {
    const role = (req.query.role as AuthRole | undefined) ?? 'write';
    const token = `mock-access-token-${randomUUID()}`;

    // Register the token with the requested role
    registerDynamicToken(token, role);

    res.json({
      access_token: token,
      token_type: 'Bearer',
      expires_in: 3600
    });
  });

  return router;
};
