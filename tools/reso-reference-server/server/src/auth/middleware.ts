import type { RequestHandler } from 'express';
import type { AuthRole, AuthTokenConfig } from './config.js';
import { hasRole, resolveRole } from './config.js';

/**
 * Creates Express middleware that requires a minimum auth role.
 *
 * When `authRequired` is false (default for development), all requests
 * pass through with write-level access for backward compatibility.
 */
export const requireAuth =
  (minimumRole: AuthRole, config: AuthTokenConfig): RequestHandler =>
  (req, res, next) => {
    // When auth is not required (dev mode), allow all requests
    if (!config.authRequired) {
      next();
      return;
    }

    const authHeader = req.headers.authorization;
    if (!authHeader || !authHeader.startsWith('Bearer ')) {
      res.status(401).json({
        error: {
          code: '40100',
          message: 'Missing or invalid Authorization header. Use: Authorization: Bearer <token>',
          details: []
        }
      });
      return;
    }

    const token = authHeader.slice(7);
    const role = resolveRole(token, config);

    if (!role) {
      res.status(401).json({
        error: {
          code: '40101',
          message: 'Invalid or expired bearer token.',
          details: []
        }
      });
      return;
    }

    if (!hasRole(role, minimumRole)) {
      res.status(403).json({
        error: {
          code: '40300',
          message: `Insufficient permissions. This endpoint requires "${minimumRole}" role or higher.`,
          details: []
        }
      });
      return;
    }

    next();
  };
