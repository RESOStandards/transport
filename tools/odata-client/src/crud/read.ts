/**
 * OData entity read (GET by key) helper.
 */

import { buildUri } from "../uri/builder.js";
import type { ODataClient, ODataResponse } from "../types.js";

/**
 * Read a single entity by its key value.
 *
 * @param client - OData client instance
 * @param resource - Resource name (e.g. "Property")
 * @param key - Entity key value
 * @returns OData response (200 with entity or 404)
 */
export const readEntity = async (
  client: ODataClient,
  resource: string,
  key: string,
): Promise<ODataResponse> => {
  const url = buildUri(client.baseUrl, resource).key(key).build();
  return client.request("GET", url);
};
