/**
 * OData entity creation (POST) helper.
 */

import { buildUri } from "../uri/builder.js";
import type { ODataClient, ODataResponse, WriteOptions } from "../types.js";

/**
 * Create a new entity via OData POST.
 *
 * @param client - OData client instance
 * @param resource - Resource name (e.g. "Property")
 * @param body - Entity data to create
 * @param options - Optional Prefer header setting
 * @returns OData response (201 with entity or 204 minimal)
 */
export const createEntity = async (
  client: ODataClient,
  resource: string,
  body: Readonly<Record<string, unknown>>,
  options?: WriteOptions,
): Promise<ODataResponse> => {
  const url = buildUri(client.baseUrl, resource).build();
  const headers: Record<string, string> = {};

  if (options?.prefer) {
    headers["Prefer"] = `return=${options.prefer}`;
  }

  return client.request("POST", url, { body, headers });
};
