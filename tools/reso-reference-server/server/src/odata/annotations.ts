/** Generates a weak ETag from the current timestamp, base64-encoded. */
export const generateEtag = (): string => `W/"${Buffer.from(new Date().toISOString()).toString('base64')}"`;

/** Builds OData annotation properties to spread into a JSON response body. */
export const buildAnnotations = (baseUrl: string, resource: string, key: string): Record<string, string> => ({
  '@odata.context': `${baseUrl}/$metadata#${resource}/$entity`,
  '@odata.id': `${baseUrl}/${resource}('${key}')`,
  '@odata.editLink': `${baseUrl}/${resource}('${key}')`,
  '@odata.etag': generateEtag()
});
