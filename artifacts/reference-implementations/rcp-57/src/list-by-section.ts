import { readFile } from 'node:fs/promises';
import { extractChecks } from './checks.js';
import { specPath } from './paths.js';
const want = process.argv[2] ?? 'Section 2.9';
const checks = extractChecks(await readFile(specPath, 'utf-8'));
for (const c of checks.filter((c) => c.cites.includes(want))) console.log(`  ${c.id}  ${c.text}\n`);
