/**
 * OData HTTP client — delegates to @reso/odata-client.
 *
 * Preserves the same exported interface (odataRequest, buildResourceUrl)
 * so that test-runner.ts and validators.ts require no changes.
 */

import { buildUri, createClient } from '@reso/odata-client';
import type { ODataResponse } from './types.js';

/** Options for making an OData HTTP request. */
export interface RequestOptions {
  readonly method: 'GET' | 'POST' | 'PATCH' | 'DELETE';
  readonly url: string;
  readonly headers?: Readonly<Record<string, string>>;
  readonly body?: unknown;
  readonly authToken: string;
}

/**
 * Sends an HTTP request with OData-standard headers and returns a normalized response.
 *
 * Uses @reso/odata-client's createClient under the hood. Creates a lightweight
 * client per call since the auth token may vary between requests.
 */
export const odataRequest = async (options: RequestOptions): Promise<ODataResponse> => {
  const client = await createClient({
    baseUrl: '',
    auth: { mode: 'token', authToken: options.authToken }
  });

  return client.request(options.method, options.url, {
    body: options.body,
    headers: options.headers
  });
};

/**
 * Builds an OData resource URL with optional key syntax.
 * The key value is URI-encoded to handle special characters safely.
 *
 * Without a key: `https://api.reso.org/Property`
 * With a key:    `https://api.reso.org/Property('12345')`
 */
export const buildResourceUrl = (serverUrl: string, resource: string, key?: string): string => {
  const builder = buildUri(serverUrl.replace(/\/$/, ''), resource);
  return key ? builder.key(key).build() : builder.build();
};
