/**
 * Authentication helpers — OAuth2 Client Credentials grant and bearer token
 * resolution. Adapted from web-api-add-edit-test/src/lib/auth.ts.
 */

import type { AuthConfig } from '../types.js';

/**
 * Resolve an AuthConfig to a bearer token string.
 * For "token" mode, returns the token directly.
 * For "client_credentials" mode, performs the OAuth2 token exchange.
 */
export const resolveToken = async (auth: AuthConfig): Promise<string> => {
  if (auth.mode === 'token') {
    return auth.authToken;
  }
  return fetchAccessToken(auth.clientId, auth.clientSecret, auth.tokenUrl);
};

/**
 * Perform an OAuth2 Client Credentials grant to obtain an access token.
 */
export const fetchAccessToken = async (clientId: string, clientSecret: string, tokenUrl: string): Promise<string> => {
  const body = new URLSearchParams({
    grant_type: 'client_credentials',
    client_id: clientId,
    client_secret: clientSecret
  });

  const response = await fetch(tokenUrl, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded',
      Accept: 'application/json'
    },
    body: body.toString()
  });

  if (!response.ok) {
    throw new Error(`OAuth2 token request failed: ${response.status} ${response.statusText}`);
  }

  const json = (await response.json()) as Record<string, unknown>;
  const accessToken = json.access_token;

  if (typeof accessToken !== 'string' || accessToken.length === 0) {
    throw new Error('OAuth2 token response missing or empty access_token field');
  }

  return accessToken;
};
