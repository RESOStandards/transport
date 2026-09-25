import { readFile } from 'node:fs/promises';
import { extractChecks } from './checks.js';
import { specPath } from './paths.js';
const checks = extractChecks(await readFile(specPath, 'utf-8'));
for (const c of checks.filter((c) => c.cites.includes('Section 2.4'))) {
  console.log(`  ${c.id}  ${c.text}`);
  console.log('');
}
