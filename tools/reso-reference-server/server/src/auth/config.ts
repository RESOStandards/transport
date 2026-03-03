/**
 * Role-based authentication configuration for the reference server.
 *
 * Supports three roles with hierarchical permissions:
 * - admin: full access including admin endpoints
 * - write: read + create/update/delete
 * - read: query and read only
 *
 * Tokens are configured via environment variables. Defaults are for
 * development only — do not use in production.
 */

/** Authentication roles in ascending order of privilege. */
export type AuthRole = 'read' | 'write' | 'admin';

/** Role hierarchy — higher index = more privilege. */
const ROLE_HIERARCHY: ReadonlyArray<AuthRole> = ['read', 'write', 'admin'];

/** Returns true if `role` has at least the privileges of `minimumRole`. */
export const hasRole = (role: AuthRole, minimumRole: AuthRole): boolean =>
  ROLE_HIERARCHY.indexOf(role) >= ROLE_HIERARCHY.indexOf(minimumRole);

/** Auth configuration derived from environment variables. */
export interface AuthTokenConfig {
  readonly adminToken: string;
  readonly writeToken: string;
  readonly readToken: string;
  readonly authRequired: boolean;
}

/** In-memory map of dynamic tokens (from mock OAuth) to roles. */
const dynamicTokens = new Map<string, AuthRole>();

/** Registers a dynamic token with a given role (used by mock OAuth). */
export const registerDynamicToken = (token: string, role: AuthRole): void => {
  dynamicTokens.set(token, role);
};

/** Loads auth configuration from environment variables. */
export const loadAuthConfig = (): AuthTokenConfig => ({
  adminToken: process.env.ADMIN_TOKEN ?? 'admin-token',
  writeToken: process.env.WRITE_TOKEN ?? 'write-token',
  readToken: process.env.READ_TOKEN ?? 'read-token',
  authRequired: process.env.AUTH_REQUIRED === 'true'
});

/**
 * Resolves a bearer token to an auth role.
 * Checks static tokens first, then dynamic tokens from mock OAuth.
 * Returns null if the token is not recognized.
 */
export const resolveRole = (token: string, config: AuthTokenConfig): AuthRole | null => {
  if (token === config.adminToken) return 'admin';
  if (token === config.writeToken) return 'write';
  if (token === config.readToken) return 'read';

  // Check dynamic tokens (from mock OAuth)
  const dynamicRole = dynamicTokens.get(token);
  if (dynamicRole) return dynamicRole;

  return null;
};
