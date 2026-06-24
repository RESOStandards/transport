#!/usr/bin/env node
/**
 * Generate reference OData EDMX artifacts from the DD reference JSON, in both representations.
 *
 * Reads references/dd/json/dd-{ver}.json, renders via reso-common's generateEdmx, and writes
 * references/dd/edmx/dd-{ver}-{rep}.xml (rep ∈ {lookup-resource, enum}). Run by the
 * generate-dd-json workflow's Node step after the JSON is (re)generated — transport owns the
 * reference artifacts and publishes them so downstream consumes rather than regenerates (#219).
 */
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs';
import { resolve, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { generateEdmx } from '@reso-standards/reso-common';

const REPO_ROOT = resolve(dirname(fileURLToPath(import.meta.url)), '../../..');
const JSON_DIR = resolve(REPO_ROOT, 'references/dd/json');
const EDMX_DIR = resolve(REPO_ROOT, 'references/dd/edmx');

// enumMode → published rep slug
const REPS = [
  { enumMode: 'string', slug: 'lookup-resource' },
  { enumMode: 'enum-type', slug: 'enum' },
];

// dd-{ver}.json carries `resources` as an array of STRINGS; reso-common's ResoMetadata wants
// resource OBJECTS. Fields map straight across; lookups need only the fields generateEdmx reads.
const toResoMetadata = (dd) => ({
  description: dd.description,
  version: dd.version,
  generatedOn: dd.generatedOn,
  resources: dd.resources.map((name) => ({ resourceName: name, wikiPageURL: '', payloads: [] })),
  fields: dd.fields,
  lookups: dd.lookups.map((l) => ({
    lookupName: l.lookupName,
    lookupValue: l.lookupValue,
    type: l.type,
    annotations: l.annotations ?? [],
  })),
});

const versions = process.argv.slice(2);
if (versions.length === 0) {
  console.error('Usage: generate-reference-edmx.mjs <version>...  (e.g. 1.7 2.0 2.1)');
  process.exit(1);
}

mkdirSync(EDMX_DIR, { recursive: true });

for (const ver of versions) {
  const dd = JSON.parse(readFileSync(resolve(JSON_DIR, `dd-${ver}.json`), 'utf8'));
  const metadata = toResoMetadata(dd);
  const targetResources = dd.resources;
  for (const { enumMode, slug } of REPS) {
    const edmx = generateEdmx(metadata, targetResources, enumMode);
    const out = resolve(EDMX_DIR, `dd-${ver}-${slug}.xml`);
    writeFileSync(out, `${edmx}\n`, 'utf8');
    console.log(`  wrote references/dd/edmx/dd-${ver}-${slug}.xml (${edmx.length} bytes)`);
  }
}
