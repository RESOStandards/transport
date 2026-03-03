/**
 * Example: Fetch and validate CSDL metadata from an OData server.
 *
 * Prerequisites: Start the reference server at http://localhost:8080
 * Run: npx tsx examples/validate-metadata.ts
 */

import { fetchAndParseMetadata, getEntityType, validateCsdl } from '../src/index.js';

const main = async (): Promise<void> => {
  const serverUrl = 'http://localhost:8080';
  const token = 'test';

  console.log(`Fetching metadata from ${serverUrl}/$metadata...`);
  const schema = await fetchAndParseMetadata(serverUrl, token);

  console.log(`\nNamespace: ${schema.namespace}`);
  console.log(`Entity types: ${schema.entityTypes.length}`);
  console.log(`Enum types: ${schema.enumTypes.length}`);

  if (schema.entityContainer) {
    console.log(`Entity sets: ${schema.entityContainer.entitySets.length}`);
  }

  // Validate the CSDL document
  console.log('\nValidating CSDL structure...');
  const validation = validateCsdl(schema);

  if (validation.valid) {
    console.log('CSDL is valid!');
  } else {
    console.log(`CSDL has ${validation.errors.length} error(s):`);
    for (const error of validation.errors) {
      console.log(`  [${error.path}] ${error.message}`);
    }
  }

  // Show entity type details
  console.log('\nEntity types:');
  for (const et of schema.entityTypes) {
    console.log(`  ${et.name} — ${et.properties.length} properties, key: [${et.key.join(', ')}]`);
  }

  // Detailed look at Property entity type
  const property = getEntityType(schema, 'Property');
  if (property) {
    console.log(`\nProperty entity type (${property.properties.length} fields):`);
    for (const prop of property.properties.slice(0, 10)) {
      console.log(`  ${prop.name}: ${prop.type}${prop.nullable === false ? ' (required)' : ''}`);
    }
    if (property.properties.length > 10) {
      console.log(`  ... and ${property.properties.length - 10} more`);
    }
  }
};

main().catch(console.error);
