/**
 * OData entity collection query (GET with query options) helper.
 */

import { buildUri } from "../uri/builder.js";
import type { ODataClient, ODataResponse, ODataQueryOptions } from "../types.js";

/**
 * Query an entity collection with OData system query options.
 *
 * @param client - OData client instance
 * @param resource - Resource name (e.g. "Property")
 * @param options - OData query options ($filter, $select, $orderby, $top, $skip, $count)
 * @returns OData response with `{ value: [...], "@odata.count"?: n }`
 */
export const queryEntities = async (
  client: ODataClient,
  resource: string,
  options?: ODataQueryOptions,
): Promise<ODataResponse> => {
  let builder = buildUri(client.baseUrl, resource);

  if (options?.$filter) {
    builder = builder.filter(options.$filter);
  }
  if (options?.$select) {
    builder = builder.select(...options.$select.split(",").map((s) => s.trim()));
  }
  if (options?.$orderby) {
    builder = builder.orderby(options.$orderby);
  }
  if (options?.$top !== undefined) {
    builder = builder.top(options.$top);
  }
  if (options?.$skip !== undefined) {
    builder = builder.skip(options.$skip);
  }
  if (options?.$count !== undefined) {
    builder = builder.count(options.$count);
  }
  if (options?.$expand) {
    builder = builder.expand(options.$expand);
  }
  if (options?.$search) {
    builder = builder.search(options.$search);
  }
  if (options?.$compute) {
    builder = builder.compute(options.$compute);
  }
  if (options?.$format) {
    builder = builder.format(options.$format);
  }

  const url = builder.build();
  return client.request("GET", url);
};
