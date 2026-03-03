/**
 * Example: OAuth2 Client Credentials flow, then query.
 *
 * Prerequisites: Start the reference server at http://localhost:8080
 * Run: npx tsx examples/oauth-flow.ts
 */

import { createClient, isODataCollection, queryEntities } from '../src/index.js';

const main = async (): Promise<void> => {
  // Create client with OAuth2 Client Credentials
  const client = await createClient({
    baseUrl: 'http://localhost:8080',
    auth: {
      mode: 'client_credentials',
      clientId: 'my-client',
      clientSecret: 'my-secret',
      tokenUrl: 'http://localhost:8080/oauth/token'
    }
  });

  console.log('Authenticated via OAuth2 Client Credentials');
  console.log('Querying properties...');

  const response = await queryEntities(client, 'Property', { $top: 5 });

  if (response.status === 200 && isODataCollection(response.body)) {
    console.log(`Found ${response.body.value.length} properties`);
  } else {
    console.log('Response:', response.status, response.body);
  }
};

main().catch(console.error);
