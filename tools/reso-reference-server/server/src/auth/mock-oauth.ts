import { Router } from "express";
import { randomUUID } from "node:crypto";

/**
 * Creates an Express router with a mock OAuth2 Client Credentials endpoint.
 * Accepts any client_id/client_secret and returns a mock access token.
 */
export const createMockOAuthRouter = (): Router => {
  const router = Router();

  router.post("/oauth/token", (_req, res) => {
    res.json({
      access_token: `mock-access-token-${randomUUID()}`,
      token_type: "Bearer",
      expires_in: 3600,
    });
  });

  return router;
};
