import { readFile } from 'node:fs/promises';
import { coverage, extractChecks } from './checks.js';
import { scenarios } from './scenarios.js';
import { specPath } from './paths.js';


const checks = extractChecks(await readFile(specPath, 'utf-8'));
const report = coverage(checks, scenarios);

console.log(`  Section 3 states ${report.checksInSpec} checks`);
console.log(`  covered by scenarios: ${report.covered.length}`);
console.log(`  not yet verified:     ${report.uncovered.length}`);
if (report.drifted.length > 0) {
  console.log(`  DRIFTED — the check changed since the scenario was written:`);
  for (const d of report.drifted) console.log(`    ${d.scenario} covers ${d.check}`);
} else {
  console.log('  drifted: none');
}

const byCite = new Map<string, number>();
for (const c of checks) for (const s of c.cites) byCite.set(s, (byCite.get(s) ?? 0) + 1);
console.log('\n  checks per section cited:');
for (const [s, n] of [...byCite].sort((a, b) => b[1] - a[1]).slice(0, 8)) {
  console.log(`    ${s.padEnd(34)} ${n}`);
}
