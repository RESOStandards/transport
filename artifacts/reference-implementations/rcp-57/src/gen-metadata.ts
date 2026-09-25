import { readFile, writeFile } from 'node:fs/promises';
import { metadataFromSpec } from './metadata.js';
import { metadataReportPath, specPath } from './paths.js';


const report = metadataFromSpec(await readFile(specPath, 'utf-8'), '2026-09-24');
await writeFile(metadataReportPath, `${JSON.stringify(report, null, 2)}\n`, 'utf-8');

const byRes = new Map<string, number>();
for (const f of report.fields) byRes.set(f.resourceName, (byRes.get(f.resourceName) ?? 0) + 1);
for (const [r, n] of byRes) console.log(`  ${r.padEnd(20)} ${n} fields`);
console.log(`  lookups              ${report.lookups.length} values`);
const coll = report.fields.filter((f) => f.isCollection === true).map((f) => f.fieldName);
const enums = report.fields.filter((f) => f.isEnumeration).map((f) => f.fieldName);
console.log(`  enumerations         ${enums.join(', ')}`);
console.log(`  collections          ${coll.join(', ') || '(none)'}`);
