/**
 * OData HTTP client — wraps native fetch with OData-standard headers and
 * normalized response handling. Inspired by Apache Olingo's ODataClient.
 */

import type { ClientConfig, ODataClient, ODataResponse } from '../types.js';
import { resolveToken } from './auth.js';

/**
 * Create an OData client for the given configuration.
 *
 * The client automatically sets OData-Version, Content-Type, Accept, and
 * Authorization headers on every request. Response headers are normalized
 * to lowercase keys.
 *
 * @example
 * ```ts
 * const client = await createClient({
 *   baseUrl: "http://localhost:8080",
 *   auth: { mode: "token", authToken: "test" },
 * });
 *
 * const response = await client.request("GET", "http://localhost:8080/Property('key')");
 * ```
 */
export const createClient = async (config: ClientConfig): Promise<ODataClient> => {
  const token = await resolveToken(config.auth);

  const request = async (
    method: 'GET' | 'POST' | 'PUT' | 'PATCH' | 'DELETE',
    url: string,
    options?: {
      readonly body?: unknown;
      readonly headers?: Readonly<Record<string, string>>;
    }
  ): Promise<ODataResponse> => {
    const headers: Record<string, string> = {
      'OData-Version': '4.01',
      'Content-Type': 'application/json',
      Accept: 'application/json',
      Authorization: `Bearer ${token}`,
      ...config.defaultHeaders,
      ...options?.headers
    };

    const response = await fetch(url, {
      method,
      headers,
      body: options?.body ? JSON.stringify(options.body) : undefined
    });

    const rawBody = await response.text();
    let body: unknown = null;
    try {
      body = JSON.parse(rawBody);
    } catch {
      // Empty body is expected for 204 No Content
    }

    const responseHeaders: Record<string, string> = {};
    response.headers.forEach((value, key) => {
      responseHeaders[key.toLowerCase()] = value;
    });

    return { status: response.status, headers: responseHeaders, body, rawBody };
  };

  return {
    baseUrl: config.baseUrl,
    request
  };
};
