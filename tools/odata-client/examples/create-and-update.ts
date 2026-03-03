/**
 * Example: Create a new Property record, then update it.
 *
 * Prerequisites: Start the reference server at http://localhost:8080
 * Run: npx tsx examples/create-and-update.ts
 */

import { createClient, createEntity, extractAnnotations, extractEntityData, readEntity, updateEntity } from '../src/index.js';

const main = async (): Promise<void> => {
  const client = await createClient({
    baseUrl: 'http://localhost:8080',
    auth: { mode: 'token', authToken: 'test' }
  });

  // Create a new property
  console.log('Creating property...');
  const createResponse = await createEntity(
    client,
    'Property',
    {
      ListPrice: 250000,
      City: 'Austin',
      BedroomsTotal: 3
    },
    { prefer: 'representation' }
  );

  console.log('Create status:', createResponse.status);

  if (createResponse.status === 201 || createResponse.status === 204) {
    const entity = createResponse.body as Record<string, unknown>;
    const annotations = extractAnnotations(entity);
    console.log('Created entity annotations:', annotations);

    // Extract the key from the created entity
    const key = entity.ListingKey as string;
    if (key) {
      // Update the property
      console.log(`\nUpdating property ${key}...`);
      const updateResponse = await updateEntity(
        client,
        'Property',
        key,
        { ListPrice: 275000, City: 'Dallas' },
        { prefer: 'representation' }
      );

      console.log('Update status:', updateResponse.status);
      if (updateResponse.body) {
        const updatedData = extractEntityData(updateResponse.body as Record<string, unknown>);
        console.log('Updated data:', updatedData);
      }

      // Read it back to verify
      console.log(`\nReading property ${key}...`);
      const readResponse = await readEntity(client, 'Property', key);
      console.log('Read status:', readResponse.status);
      if (readResponse.body) {
        console.log('Final data:', extractEntityData(readResponse.body as Record<string, unknown>));
      }
    }
  }
};

main().catch(console.error);
