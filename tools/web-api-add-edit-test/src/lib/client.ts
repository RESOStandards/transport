import type { ODataResponse } from "./types.js";

/** Options for making an OData HTTP request. */
export interface RequestOptions {
  readonly method: "GET" | "POST" | "PATCH" | "DELETE";
  readonly url: string;
  readonly headers?: Readonly<Record<string, string>>;
  readonly body?: unknown;
  readonly authToken: string;
}

/**
 * Sends an HTTP request with OData-standard headers and returns a normalized response.
 *
 * Automatically sets OData-Version, Content-Type, Accept, and Authorization headers.
 * Custom headers passed in `options.headers` override the defaults.
 * Response headers are normalized to lowercase keys to avoid case-sensitivity issues
 * (HTTP headers are case-insensitive per RFC 7230).
 */
export const odataRequest = async (
  options: RequestOptions,
): Promise<ODataResponse> => {
  const headers: Record<string, string> = {
    "OData-Version": "4.01",
    "Content-Type": "application/json",
    Accept: "application/json",
    Authorization: `Bearer ${options.authToken}`,
    ...options.headers,
  };

  const response = await fetch(options.url, {
    method: options.method,
    headers,
    body: options.body ? JSON.stringify(options.body) : undefined,
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

/**
 * Builds an OData resource URL with optional key syntax.
 * The key value is URI-encoded to handle special characters safely.
 *
 * Without a key: `https://api.reso.org/Property`
 * With a key:    `https://api.reso.org/Property('12345')`
 */
export const buildResourceUrl = (
  serverUrl: string,
  resource: string,
  key?: string,
): string => {
  const base = `${serverUrl.replace(/\/$/, "")}/${encodeURIComponent(resource)}`;
  return key ? `${base}('${encodeURIComponent(key)}')` : base;
};
