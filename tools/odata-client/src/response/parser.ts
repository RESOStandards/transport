/**
 * OData response parser — extract annotations and typed entities from
 * OData JSON response bodies.
 */

import type { ODataAnnotations, ODataClient, ODataCollection, ODataEntity } from '../types.js';

/**
 * Extract OData annotations from an entity response body.
 */
export const extractAnnotations = (entity: ODataEntity): ODataAnnotations => ({
  ...(typeof entity['@odata.context'] === 'string' && {
    context: entity['@odata.context']
  }),
  ...(typeof entity['@odata.id'] === 'string' && {
    id: entity['@odata.id']
  }),
  ...(typeof entity['@odata.editLink'] === 'string' && {
    editLink: entity['@odata.editLink']
  }),
  ...(typeof entity['@odata.etag'] === 'string' && {
    etag: entity['@odata.etag']
  })
});

/**
 * Type guard: check if a body is an OData collection response.
 */
export const isODataCollection = (body: unknown): body is ODataCollection => {
  if (typeof body !== 'object' || body === null) return false;
  return Array.isArray((body as Record<string, unknown>).value);
};

/**
 * Extract entity data from a response body (stripping OData annotations).
 */
export const extractEntityData = (entity: ODataEntity): Readonly<Record<string, unknown>> =>
  Object.fromEntries(Object.entries(entity).filter(([key]) => !key.startsWith('@')));

/**
 * Get the @odata.nextLink from a collection response body, if present.
 */
export const getNextLink = (body: unknown): string | undefined => {
  if (typeof body !== 'object' || body === null) return undefined;
  const link = (body as Record<string, unknown>)['@odata.nextLink'];
  return typeof link === 'string' ? link : undefined;
};

/**
 * Follow all @odata.nextLink pages and collect all entities from a
 * server-paged collection response.
 *
 * @param client - OData client to use for following nextLinks
 * @param firstResponse - The initial collection response
 * @returns All entities from all pages
 */
export const followAllPages = async (
  client: ODataClient,
  firstResponse: { readonly body: unknown }
): Promise<ReadonlyArray<ODataEntity>> => {
  const allEntities: ODataEntity[] = [];

  let body = firstResponse.body;
  while (body !== null && body !== undefined) {
    if (isODataCollection(body)) {
      allEntities.push(...body.value);
    }
    const nextLink = getNextLink(body);
    if (!nextLink) break;
    const response = await client.request('GET', nextLink);
    body = response.body;
  }

  return allEntities;
};
