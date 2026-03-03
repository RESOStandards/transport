/**
 * Parse an OData query string into structured query options.
 */

import type { ODataQueryOptions } from "../types.js";

/**
 * Parse OData system query options from a URL query string or
 * from an Express-style `req.query` object.
 */
export const parseQueryString = (
  input: string | Readonly<Record<string, string | string[] | undefined>>,
): ODataQueryOptions => {
  const params: Record<string, string> =
    typeof input === "string"
      ? Object.fromEntries(new URLSearchParams(input))
      : Object.fromEntries(
          Object.entries(input)
            .filter((entry): entry is [string, string] => typeof entry[1] === "string"),
        );

  const result: ODataQueryOptions = {
    ...(params["$filter"] && { $filter: params["$filter"] }),
    ...(params["$select"] && { $select: params["$select"] }),
    ...(params["$orderby"] && { $orderby: params["$orderby"] }),
    ...(params["$top"] !== undefined && { $top: parseInt(params["$top"], 10) }),
    ...(params["$skip"] !== undefined && { $skip: parseInt(params["$skip"], 10) }),
    ...(params["$count"] !== undefined && { $count: params["$count"] === "true" }),
    ...(params["$expand"] && { $expand: params["$expand"] }),
    ...(params["$search"] && { $search: params["$search"] }),
    ...(params["$compute"] && { $compute: params["$compute"] }),
    ...(params["$format"] && { $format: params["$format"] }),
  };

  return result;
};
