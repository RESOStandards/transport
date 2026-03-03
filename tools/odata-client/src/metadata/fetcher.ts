/**
 * OData metadata fetcher — retrieves and parses CSDL/EDMX metadata
 * from an OData server's $metadata endpoint.
 */

import { parseCsdlXml } from '../csdl/parser.js';
import type { CsdlSchema } from '../csdl/types.js';

/**
 * Fetch raw EDMX XML metadata from an OData server.
 *
 * @param baseUrl - Server base URL (e.g. "http://localhost:8080")
 * @param token - Bearer authentication token
 * @returns Raw XML metadata string
 */
export const fetchRawMetadata = async (baseUrl: string, token: string): Promise<string> => {
  const metadataUrl = `${baseUrl.replace(/\/$/, '')}/$metadata`;
  const response = await fetch(metadataUrl, {
    headers: {
      Authorization: `Bearer ${token}`,
      Accept: 'application/xml'
    }
  });

  if (!response.ok) {
    throw new Error(`Failed to fetch metadata from ${metadataUrl}: ${response.status} ${response.statusText}`);
  }

  return response.text();
};

/**
 * Fetch and parse CSDL metadata from an OData server.
 *
 * @param baseUrl - Server base URL
 * @param token - Bearer authentication token
 * @returns Parsed CSDL schema
 */
export const fetchAndParseMetadata = async (baseUrl: string, token: string): Promise<CsdlSchema> => {
  const xml = await fetchRawMetadata(baseUrl, token);
  return parseCsdlXml(xml);
};
