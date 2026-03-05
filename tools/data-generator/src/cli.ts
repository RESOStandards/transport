#!/usr/bin/env node
import { parseArgs } from 'node:util';
import { checkbox, confirm, input, number, select } from '@inquirer/prompts';
import type { OutputOptions } from './client.js';
import { buildMultiResourcePlan } from './fk-resolver.js';
import type { ResoField, ResoLookup, SeedOptions } from './generators/types.js';
import { generateSeedData } from './index.js';
import { getDefaultRelatedCount, getRelatedResources } from './plan.js';

/** Metadata response from the reference server's JSON API. */
interface MetadataResponse {
  readonly fields: ReadonlyArray<ResoField>;
  readonly lookups: ReadonlyArray<ResoLookup>;
  readonly resources: ReadonlyArray<{ resourceName: string }>;
}

/** Fetches metadata from the server's JSON API endpoints. */
const fetchServerMetadata = async (serverUrl: string, authToken: string): Promise<MetadataResponse> => {
  const headers = {
    Accept: 'application/json',
    Authorization: `Bearer ${authToken}`
  };

  // Fetch available resources from the health endpoint
  const healthRes = await fetch(`${serverUrl}/health`, { headers });
  if (!healthRes.ok) throw new Error(`Server not reachable at ${serverUrl}`);

  // Fetch fields for all known resources
  const knownResources = [
    'Property',
    'Member',
    'Office',
    'Media',
    'OpenHouse',
    'Showing',
    'Teams',
    'TeamMembers',
    'OUID',
    'PropertyRooms',
    'PropertyGreenVerification',
    'PropertyPowerProduction',
    'PropertyUnitTypes'
  ];
  const allFields: ResoField[] = [];
  const resources: Array<{ resourceName: string }> = [];

  for (const resource of knownResources) {
    try {
      const res = await fetch(`${serverUrl}/api/metadata/fields?resource=${resource}`, { headers });
      if (res.ok) {
        const fields = (await res.json()) as ResoField[];
        if (fields.length > 0) {
          allFields.push(...fields);
          resources.push({ resourceName: resource });
        }
      }
    } catch {
      // Resource not available — skip
    }
  }

  // Fetch lookups for each resource
  const allLookups: ResoLookup[] = [];
  for (const resource of resources) {
    try {
      const res = await fetch(`${serverUrl}/api/metadata/lookups-for-resource?resource=${resource.resourceName}`, { headers });
      if (res.ok) {
        const lookupsByType = (await res.json()) as Record<string, ResoLookup[]>;
        for (const lookups of Object.values(lookupsByType)) {
          allLookups.push(...lookups);
        }
      }
    } catch {
      // Lookups not available — skip
    }
  }

  return { fields: allFields, lookups: allLookups, resources };
};

/** Groups fields by resource name. */
const groupFieldsByResource = (fields: ReadonlyArray<ResoField>): Record<string, ReadonlyArray<ResoField>> => {
  const grouped: Record<string, ResoField[]> = {};
  for (const field of fields) {
    if (!grouped[field.resourceName]) grouped[field.resourceName] = [];
    grouped[field.resourceName].push(field);
  }
  return grouped;
};

/** Groups lookups by lookup name. */
const groupLookupsByType = (lookups: ReadonlyArray<ResoLookup>): Record<string, ReadonlyArray<ResoLookup>> => {
  const grouped: Record<string, ResoLookup[]> = {};
  for (const lookup of lookups) {
    if (!grouped[lookup.lookupName]) grouped[lookup.lookupName] = [];
    grouped[lookup.lookupName].push(lookup);
  }
  return grouped;
};

/** Parses command-line arguments for non-interactive mode. */
const parseCliArgs = () => {
  const { values } = parseArgs({
    options: {
      url: { type: 'string', short: 'u' },
      resource: { type: 'string', short: 'r' },
      count: { type: 'string', short: 'n' },
      related: { type: 'string' },
      'auth-token': { type: 'string', short: 't' },
      format: { type: 'string', short: 'f' },
      output: { type: 'string', short: 'o' },
      deps: { type: 'boolean', short: 'd' },
      'no-deps': { type: 'boolean' },
      help: { type: 'boolean', short: 'h' }
    },
    strict: false
  });
  return values;
};

const printHelp = () => {
  console.log(`
RESO Data Generator — Generate realistic test data for RESO OData servers

Usage:
  reso-data-generator                    # Interactive mode
  reso-data-generator [options]          # Non-interactive mode

Options:
  -u, --url <url>           Server URL (default: http://localhost:8080)
  -r, --resource <name>     Resource to generate (e.g., Property)
  -n, --count <number>      Number of records to generate (default: 10)
  --related <spec>          Related records (e.g., Media:5,OpenHouse:2,Showing:2)
  -d, --deps                Auto-generate dependency resources with valid FKs (default: true)
  --no-deps                 Disable dependency resolution
  -t, --auth-token <token>  Bearer token for authentication
  -f, --format <format>     Output format: http, json, or curl (default: http)
  -o, --output <path>       Output path (directory for json, file for curl)
  -h, --help                Show this help message

Examples:
  # Interactive mode
  reso-data-generator

  # POST 50 Properties with dependency resources and related records
  reso-data-generator -u http://localhost:8080 -r Property -n 50 \\
    --related Media:5,OpenHouse:2,Showing:2 -t admin-token

  # Generate JSON files (dependencies auto-resolved)
  reso-data-generator -r Property -n 10 -f json -o ./seed-data \\
    --related Media:5

  # Generate without dependency resolution
  reso-data-generator -r Property -n 10 -f json -o ./seed-data --no-deps

  # Generate a seed.sh curl script
  reso-data-generator -r Property -n 10 -f curl -o ./seed.sh \\
    --related Media:5,OpenHouse:2 -t admin-token
`);
};

/** Parses a related records spec like "Media:5,OpenHouse:2" into a record. */
const parseRelatedSpec = (spec: string): Record<string, number> => {
  const result: Record<string, number> = {};
  for (const pair of spec.split(',')) {
    const [resource, countStr] = pair.trim().split(':');
    if (resource && countStr) {
      result[resource] = Number(countStr);
    }
  }
  return result;
};

/** Runs the interactive CLI flow. */
const runInteractive = async () => {
  console.log('\n  RESO Data Generator\n');

  // Output format
  const format = await select({
    message: 'Output format:',
    choices: [
      { value: 'http' as const, name: 'HTTP — POST records to an OData server' },
      { value: 'json' as const, name: 'JSON — Write records as JSON files to a directory' },
      { value: 'curl' as const, name: 'Curl — Generate a seed.sh script with curl commands' }
    ]
  });

  // Server URL (needed for http and curl, and for metadata in all cases)
  const serverUrl = await input({
    message: 'Server URL:',
    default: 'http://localhost:8080'
  });

  // Auth token
  const authToken = await input({
    message: 'Auth token:',
    default: 'admin-token'
  });

  // Output path (for json and curl)
  let outputPath = '';
  if (format === 'json') {
    outputPath = await input({
      message: 'Output directory:',
      default: './seed-data'
    });
  } else if (format === 'curl') {
    outputPath = await input({
      message: 'Output script file:',
      default: './seed.sh'
    });
  }

  // Fetch metadata
  console.log('\nFetching metadata from server...');
  let metadata: MetadataResponse;
  try {
    metadata = await fetchServerMetadata(serverUrl, authToken);
  } catch (err) {
    console.error(`Failed to fetch metadata: ${err instanceof Error ? err.message : 'Unknown error'}`);
    console.error('Make sure the server is running and accessible.');
    process.exit(1);
  }

  const fieldsByResource = groupFieldsByResource(metadata.fields);
  const availableResources = metadata.resources.map(r => r.resourceName);
  console.log(`Found ${availableResources.length} resources: ${availableResources.join(', ')}\n`);

  // Select resource
  const resource = await select({
    message: 'Resource to generate:',
    choices: availableResources.map(r => ({
      value: r,
      name: `${r} (${fieldsByResource[r]?.length ?? 0} fields)`
    }))
  });

  // Record count
  const count =
    (await number({
      message: `Number of ${resource} records:`,
      default: 10,
      min: 1,
      max: 10000
    })) ?? 10;

  // Related records
  const relatedResources = getRelatedResources(resource, fieldsByResource);
  const relatedRecords: Record<string, number> = {};

  if (relatedResources.length > 0) {
    const selectedRelated = await checkbox({
      message: 'Include related records:',
      choices: relatedResources.map(r => ({
        value: r,
        name: `${r} (default: ${getDefaultRelatedCount(r)} per ${resource})`,
        checked: true
      }))
    });

    for (const related of selectedRelated) {
      const relatedCount =
        (await number({
          message: `Number of ${related} per ${resource}:`,
          default: getDefaultRelatedCount(related),
          min: 1,
          max: 100
        })) ?? getDefaultRelatedCount(related);
      relatedRecords[related] = relatedCount;
    }
  }

  // Dependency resolution
  const resolveDependencies = await confirm({
    message: 'Auto-generate dependency resources (Member, Office, etc.) with valid FK linkages?',
    default: true
  });

  // Summary
  console.log('\n--- Generation Plan ---');
  console.log(`  Resource: ${resource}`);
  console.log(`  Count: ${count}`);
  if (resolveDependencies) {
    const plan = buildMultiResourcePlan(
      resource,
      count,
      Object.keys(relatedRecords).length > 0 ? relatedRecords : undefined,
      fieldsByResource
    );
    const depPhases = plan.phases.filter(p => p.resource !== resource);
    if (depPhases.length > 0) {
      console.log('  Dependencies (auto-generated):');
      for (const phase of depPhases) {
        console.log(`    ${phase.resource}: ${phase.count} records`);
      }
    }
    if (plan.backFillPhases.length > 0) {
      console.log('  Back-fill phases:');
      for (const bf of plan.backFillPhases) {
        const fks = bf.fkBindings.map(b => `${b.fkColumn} -> ${b.targetResource}`).join(', ');
        console.log(`    ${bf.resource}: ${fks}`);
      }
    }
  }
  if (Object.keys(relatedRecords).length > 0) {
    console.log('  Related records:');
    for (const [r, c] of Object.entries(relatedRecords)) {
      console.log(`    ${r}: ${c} per ${resource} (${c * count} total)`);
    }
  }
  console.log(`  Format: ${format}`);
  if (format === 'http') console.log(`  Server: ${serverUrl}`);
  if (format === 'json') console.log(`  Output: ${outputPath}`);
  if (format === 'curl') console.log(`  Script: ${outputPath}`);
  console.log('');

  const proceed = await confirm({ message: 'Proceed?', default: true });
  if (!proceed) {
    console.log('Cancelled.');
    return;
  }

  // Build output options
  let output: OutputOptions;
  if (format === 'http') {
    output = { format: 'http', serverUrl, auth: { mode: 'token', authToken } };
  } else if (format === 'json') {
    output = { format: 'json', outputDir: outputPath };
  } else {
    output = { format: 'curl', serverUrl, auth: { mode: 'token', authToken }, outputFile: outputPath };
  }

  // Run generation
  console.log('');
  const lookupsByType = groupLookupsByType(metadata.lookups);
  const result = await generateSeedData(
    {
      serverUrl,
      resource,
      count,
      relatedRecords: Object.keys(relatedRecords).length > 0 ? relatedRecords : undefined,
      auth: { mode: 'token', authToken },
      fieldsByResource,
      lookupsByType,
      resolveDependencies
    },
    output,
    (message, completed, total) => {
      process.stdout.write(`\r  ${message}: ${completed}/${total}`);
    }
  );

  // Report
  console.log('\n\n--- Results ---');
  console.log(`  Created: ${result.created}`);
  console.log(`  Failed: ${result.failed}`);
  console.log(`  Duration: ${(result.durationMs / 1000).toFixed(1)}s`);
  if (result.errors.length > 0) {
    console.log('  Errors (first 5):');
    for (const err of result.errors.slice(0, 5)) {
      console.log(`    - ${err}`);
    }
  }
  console.log('');
};

/** Runs the non-interactive CLI with parsed arguments. */
const runNonInteractive = async (args: ReturnType<typeof parseCliArgs>) => {
  const serverUrl = (args.url as string) ?? 'http://localhost:8080';
  const resource = args.resource as string;
  const count = Number(args.count ?? '10');
  const authToken = (args['auth-token'] as string) ?? 'admin-token';
  const format = (args.format as string) ?? 'http';
  const outputPath = (args.output as string) ?? (format === 'json' ? './seed-data' : './seed.sh');
  // --deps is default true; --no-deps disables it
  const resolveDependencies = !args['no-deps'];

  if (!resource) {
    console.error('Error: --resource is required in non-interactive mode');
    process.exit(1);
  }

  // Parse related records
  const relatedRecords = args.related ? parseRelatedSpec(args.related as string) : undefined;

  // Fetch metadata
  console.log(`Fetching metadata from ${serverUrl}...`);
  const metadata = await fetchServerMetadata(serverUrl, authToken);
  const fieldsByResource = groupFieldsByResource(metadata.fields);
  const lookupsByType = groupLookupsByType(metadata.lookups);

  // Build output options
  let output: OutputOptions;
  if (format === 'http') {
    output = { format: 'http', serverUrl, auth: { mode: 'token', authToken } };
  } else if (format === 'json') {
    output = { format: 'json', outputDir: outputPath };
  } else if (format === 'curl') {
    output = { format: 'curl', serverUrl, auth: { mode: 'token', authToken }, outputFile: outputPath };
  } else {
    console.error(`Unknown format: ${format}. Use http, json, or curl.`);
    process.exit(1);
  }

  const result = await generateSeedData(
    {
      serverUrl,
      resource,
      count,
      relatedRecords,
      auth: { mode: 'token', authToken },
      fieldsByResource,
      lookupsByType,
      resolveDependencies
    },
    output,
    (message, completed, total) => {
      process.stdout.write(`\r  ${message}: ${completed}/${total}`);
    }
  );

  console.log(`\n\nCreated: ${result.created}, Failed: ${result.failed}, Duration: ${(result.durationMs / 1000).toFixed(1)}s`);
  if (result.errors.length > 0) {
    for (const err of result.errors.slice(0, 5)) {
      console.error(`  Error: ${err}`);
    }
  }

  process.exit(result.failed > 0 ? 1 : 0);
};

/** Main entry point. */
const main = async () => {
  const args = parseCliArgs();

  if (args.help) {
    printHelp();
    return;
  }

  // If resource is provided, run non-interactive; otherwise interactive
  if (args.resource) {
    await runNonInteractive(args);
  } else {
    await runInteractive();
  }
};

main().catch(err => {
  console.error('Fatal error:', err instanceof Error ? err.message : err);
  process.exit(1);
});
