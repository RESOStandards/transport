/**
 * OData entity delete (DELETE) helper.
 */

import { buildUri } from "../uri/builder.js";
import type { ODataClient, ODataResponse } from "../types.js";

/** Options for delete operations. */
export interface DeleteOptions {
  /** If-Match header for optimistic concurrency (ETag value). Use "*" to match any. */
  readonly ifMatch?: string;
}

/**
 * Delete an entity by its key value.
 *
 * @param client - OData client instance
 * @param resource - Resource name (e.g. "Property")
 * @param key - Entity key value
 * @param options - Optional ETag settings
 * @returns OData response (204 on success, 404 if not found)
 */
export const deleteEntity = async (
  client: ODataClient,
  resource: string,
  key: string,
  options?: DeleteOptions,
): Promise<ODataResponse> => {
  const url = buildUri(client.baseUrl, resource).key(key).build();
  const headers: Record<string, string> = {};

  if (options?.ifMatch) {
    headers["If-Match"] = options.ifMatch;
  }

  return client.request("DELETE", url, headers && Object.keys(headers).length > 0 ? { headers } : undefined);
};
