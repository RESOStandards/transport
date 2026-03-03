/**
 * Authentication helpers — delegates to @reso/odata-client.
 */

import {
  resolveToken,
  fetchAccessToken as clientFetchAccessToken,
} from "@reso/odata-client";
import type { AuthConfig } from "./types.js";

/**
 * Resolves an AuthConfig to a bearer token string.
 * For "token" mode, returns the token directly.
 * For "client_credentials" mode, performs the OAuth2 token exchange.
 */
export const resolveAuthToken = async (auth: AuthConfig): Promise<string> => {
  if (auth.mode === "token") {
    return resolveToken({ mode: "token", authToken: auth.authToken });
  }
  return resolveToken({
    mode: "client_credentials",
    clientId: auth.clientId,
    clientSecret: auth.clientSecret,
    tokenUrl: auth.tokenUrl,
  });
};

/**
 * Performs an OAuth2 Client Credentials grant to obtain an access token.
 */
export const fetchAccessToken = clientFetchAccessToken;
