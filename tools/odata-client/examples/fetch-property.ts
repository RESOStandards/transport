/**
 * Example: Fetch a Property record by key.
 *
 * Prerequisites: Start the reference server at http://localhost:8080
 * Run: npx tsx examples/fetch-property.ts
 */

import { createClient, extractAnnotations, extractEntityData, readEntity } from '../src/index.js';

const main = async (): Promise<void> => {
  const client = await createClient({
    baseUrl: 'http://localhost:8080',
    auth: { mode: 'token', authToken: 'test' }
  });

  // Read a property by key
  const response = await readEntity(client, 'Property', 'example-key-123');

  if (response.status === 200) {
    const annotations = extractAnnotations(response.body as Record<string, unknown>);
    const data = extractEntityData(response.body as Record<string, unknown>);
    console.log('Annotations:', annotations);
    console.log('Entity data:', data);
  } else if (response.status === 404) {
    console.log('Property not found');
  } else {
    console.error('Unexpected status:', response.status);
  }
};

main().catch(console.error);
