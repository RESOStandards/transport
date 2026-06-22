#!/usr/bin/env node
/**
 * DD reference metadata fitness checks.
 *
 * Runs the principles from reso-certification/docs/dd-reference-fitness-principles.md
 * against one or more dd-{ver}.json payloads. Exits non-zero on any failure.
 *
 * Usage:
 *   node check-dd-reference-fitness.js <dd-json-path> [<dd-json-path> ...]
 *
 * Defaults to scanning both reference-metadata/ and src/etl/reference-metadata/
 * if no paths are supplied.
 */

import { readFileSync, existsSync } from 'node:fs';
import { resolve, basename } from 'node:path';

const isEdm = (t) => typeof t === 'string' && t.startsWith('Edm.');
const nonEmptyString = (v) => typeof v === 'string' && v.length > 0;

const versionFromFilename = (path) => {
  const m = basename(path).match(/^dd-(.+)\.json$/);
  return m ? m[1] : null;
};

const checks = [
  // 2. Resource list completeness vs fields
  {
    name: 'resource-completeness',
    description: 'every field.resourceName appears in resources[]',
    run: (d) => {
      const resources = new Set(d.resources ?? []);
      const missing = [...new Set(d.fields.map((f) => f.resourceName))]
        .filter((r) => !resources.has(r));
      return missing.length === 0
        ? { ok: true }
        : { ok: false, detail: `missing from resources[]: ${missing.join(', ')}` };
    },
  },
  // 3. Expansion shape
  {
    name: 'expansion-shape',
    description: 'every expansion has type, typeName, sourceResource',
    run: (d) => {
      const exp = d.fields.filter((f) => f.isExpansion === true);
      const bad = exp.filter(
        (f) =>
          !nonEmptyString(f.type) ||
          !nonEmptyString(f.typeName) ||
          !nonEmptyString(f.sourceResource),
      );
      return bad.length === 0
        ? { ok: true, info: `${exp.length} expansions checked` }
        : {
            ok: false,
            detail:
              `${bad.length} malformed:\n  ` +
              bad
                .slice(0, 10)
                .map(
                  (f) =>
                    `${f.resourceName}.${f.fieldName} (type=${f.type}, typeName=${f.typeName}, sourceResource=${f.sourceResource})`,
                )
                .join('\n  '),
          };
    },
  },
  // 4. sourceResource resolves to known resource
  {
    name: 'sourceresource-resolves',
    description: 'every expansion sourceResource resolves to a resource',
    run: (d) => {
      const resources = new Set(d.resources ?? []);
      const exp = d.fields.filter((f) => f.isExpansion === true);
      const unresolved = exp
        .filter(
          (f) => nonEmptyString(f.sourceResource) && !resources.has(f.sourceResource),
        )
        .map((f) => `${f.resourceName}.${f.fieldName} → ${f.sourceResource}`);
      return unresolved.length === 0
        ? { ok: true }
        : { ok: false, detail: `${unresolved.length} unresolved:\n  ` + unresolved.slice(0, 10).join('\n  ') };
    },
  },
  // 5. Non-Edm types resolve to lookups (excluding expansions and lookupStatus=Open)
  {
    name: 'non-edm-resolves',
    description: 'every non-Edm field type (other than lookupStatus="Open") resolves to a lookup',
    run: (d) => {
      const lookupNames = new Set((d.lookups ?? []).map((l) => l.lookupName));
      const candidates = d.fields.filter(
        (f) =>
          !isEdm(f.type) && !f.isExpansion && f.lookupStatus !== 'Open',
      );
      const unresolved = candidates
        .filter((f) => !lookupNames.has(f.type))
        .map((f) => `${f.resourceName}.${f.fieldName} → ${f.type} (lookupStatus=${f.lookupStatus ?? 'absent'})`);
      return unresolved.length === 0
        ? { ok: true, info: `${candidates.length} non-Edm fields checked` }
        : { ok: false, detail: `${unresolved.length} unresolved:\n  ` + unresolved.slice(0, 10).join('\n  ') };
    },
  },
  // 6. Lookup completeness
  {
    name: 'lookup-completeness',
    description: 'every lookup has non-empty lookupName and lookupValue',
    run: (d) => {
      const bad = (d.lookups ?? []).filter(
        (l) => !nonEmptyString(l.lookupName) || !nonEmptyString(l.lookupValue),
      );
      return bad.length === 0
        ? { ok: true, info: `${d.lookups?.length ?? 0} lookups checked` }
        : { ok: false, detail: `${bad.length} malformed (first: ${JSON.stringify(bad[0])})` };
    },
  },
  // 10. Version header matches filename
  {
    name: 'version-header-match',
    description: 'top-level version matches filename',
    run: (d, { filename }) => {
      const expected = versionFromFilename(filename);
      if (!expected) return { ok: true, info: 'filename has no version segment; skipping' };
      return d.version === expected
        ? { ok: true }
        : { ok: false, detail: `filename says ${expected}, top-level version is ${d.version}` };
    },
  },
];

const DEFAULT_PATHS = [
  'reso-certification/reference-metadata/dd-1.7.json',
  'reso-certification/reference-metadata/dd-2.0.json',
  'reso-certification/reference-metadata/dd-2.1.json',
  'reso-certification/src/etl/reference-metadata/dd-1.7.json',
  'reso-certification/src/etl/reference-metadata/dd-2.0.json',
  'reso-certification/src/etl/reference-metadata/dd-2.1.json',
];

const argPaths = process.argv.slice(2);
const paths = (argPaths.length > 0 ? argPaths : DEFAULT_PATHS)
  .map((p) => resolve(p))
  .filter((p) => existsSync(p));

if (paths.length === 0) {
  console.error('No JSON files found.');
  process.exit(2);
}

let totalFail = 0;
const filesByVersion = new Map();

for (const path of paths) {
  const data = JSON.parse(readFileSync(path, 'utf-8'));
  const ver = versionFromFilename(path);
  if (!filesByVersion.has(ver)) filesByVersion.set(ver, []);
  filesByVersion.get(ver).push({ path, data });

  console.log(`\n── ${path} ──`);
  for (const check of checks) {
    const result = check.run(data, { filename: path });
    if (result.ok) {
      console.log(`  ✓ ${check.name}: pass${result.info ? ` (${result.info})` : ''}`);
    } else {
      console.log(`  ✗ ${check.name}: ${check.description}`);
      console.log(`    ${result.detail}`);
      totalFail++;
    }
  }
}

// 7. Cross-version FK consistency — runs across versions when ≥2 are present
console.log('\n── cross-version checks ──');
const allVersions = [...filesByVersion.entries()]
  .filter(([v]) => v)
  .map(([v, files]) => ({ version: v, data: files[0].data }));

if (allVersions.length >= 2) {
  const fkByKey = new Map();
  for (const { version, data } of allVersions) {
    for (const f of data.fields.filter((f) => f.isExpansion && nonEmptyString(f.sourceResource))) {
      const key = `${f.resourceName}.${f.fieldName}`;
      if (!fkByKey.has(key)) fkByKey.set(key, []);
      fkByKey.get(key).push({ version, sourceResource: f.sourceResource });
    }
  }
  const drift = [];
  for (const [key, entries] of fkByKey) {
    const targets = new Set(entries.map((e) => e.sourceResource));
    if (targets.size > 1) {
      drift.push(
        `${key}: ${entries.map((e) => `${e.version}→${e.sourceResource}`).join(', ')}`,
      );
    }
  }
  if (drift.length === 0) {
    console.log(`  ✓ cross-version-fk-consistency: pass (${fkByKey.size} FK fields shared across versions)`);
  } else {
    console.log(`  ⚠ cross-version-fk-consistency: ${drift.length} fields drift across versions`);
    drift.slice(0, 10).forEach((d) => console.log(`    ${d}`));
    console.log('    (note: cross-version drift is an audit-flag, not a fail — verify against version rename docs)');
  }
} else {
  console.log('  (skipped — need ≥2 versions for cross-version check)');
}

console.log();
if (totalFail === 0) {
  console.log(`✓ All checks passed across ${paths.length} file(s).`);
  process.exit(0);
} else {
  console.log(`✗ ${totalFail} check failure(s) across ${paths.length} file(s).`);
  process.exit(1);
}
