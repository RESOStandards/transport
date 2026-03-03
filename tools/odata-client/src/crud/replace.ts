/**
 * OData entity full replace (PUT) helper.
 */

import type { ODataClient, ODataResponse, WriteOptions } from '../types.js';
import { buildUri } from '../uri/builder.js';

/**
 * Replace an existing entity via OData PUT (full replace semantics).
 *
 * All updatable properties MUST be provided; omitted properties are set
 * to their default values (OData 4.01 Protocol §11.4.4).
 *
 * @param client - OData client instance
 * @param resource - Resource name (e.g. "Property")
 * @param key - Entity key value
 * @param body - Complete entity representation
 * @param options - Optional Prefer header and ETag settings
 * @returns OData response (200 with entity or 204 minimal)
 */
export const replaceEntity = async (
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

  return client.request('PUT', url, { body, headers });
};
