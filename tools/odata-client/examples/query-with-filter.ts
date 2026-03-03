/**
 * Example: Query properties with $filter, $select, $orderby, and $top.
 *
 * Prerequisites: Start the reference server at http://localhost:8080
 * Run: npx tsx examples/query-with-filter.ts
 */

import { createClient, isODataCollection, parseFilter, queryEntities } from '../src/index.js';

const main = async (): Promise<void> => {
  const client = await createClient({
    baseUrl: 'http://localhost:8080',
    auth: { mode: 'token', authToken: 'test' }
  });

  // First, let's verify the filter parses correctly
  const filterExpr = "ListPrice gt 200000 and City eq 'Austin'";
  const ast = parseFilter(filterExpr);
  console.log('Parsed filter AST:', JSON.stringify(ast, null, 2));

  // Query with OData system query options
  const response = await queryEntities(client, 'Property', {
    $filter: filterExpr,
    $select: 'ListPrice,City,BedroomsTotal',
    $orderby: 'ListPrice desc',
    $top: 10
  });

  if (response.status === 200 && isODataCollection(response.body)) {
    console.log(`Found ${response.body.value.length} properties`);
    for (const entity of response.body.value) {
      console.log(`  ${entity.City} — $${entity.ListPrice} — ${entity.BedroomsTotal} BR`);
    }
  } else {
    console.log('Response status:', response.status);
    console.log('Body:', response.body);
  }
};

main().catch(console.error);
