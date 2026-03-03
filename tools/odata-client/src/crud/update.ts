/**
 * OData entity update (PATCH) helper.
 */

import type { ODataClient, ODataResponse, WriteOptions } from '../types.js';
import { buildUri } from '../uri/builder.js';

/**
 * Update an existing entity via OData PATCH (merge semantics).
 *
 * @param client - OData client instance
 * @param resource - Resource name (e.g. "Property")
 * @param key - Entity key value
 * @param body - Fields to update (merge patch)
 * @param options - Optional Prefer header setting
 * @returns OData response (200 with entity or 204 minimal)
 */
export const updateEntity = async (
  client: ODataClient,
  resource: string,
  key: string,
  body: Readonly<Record<string, unknown>>,
  options?: WriteOptions
): Promise<ODataResponse> => {
  const url = buildUri(client.baseUrl, resource).key(key).build();
  const headers: Record<string, string> = {};

  if (options?.prefer) {
    headers.Prefer = `return=${options.prefer}`;
  }
  if (options?.ifMatch) {
    headers['If-Match'] = options.ifMatch;
  }
  if (options?.ifNoneMatch) {
    headers['If-None-Match'] = options.ifNoneMatch;
  }

  return client.request('PATCH', url, { body, headers });
};
