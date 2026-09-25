import { readdir, readFile } from 'node:fs/promises';
import { describe, expect, it } from 'vitest';
import { coverage, extractChecks, type Scenario } from '../src/checks.js';
import { scenarios } from '../src/scenarios.js';
import { specPath } from '../src/paths.js';

const spec = await readFile(specPath, 'utf-8');
const checks = extractChecks(spec);

describe('checks are read from the specification', () => {
  it('finds them, and would fail loudly if the section changed shape', () => {
    expect(checks.length).toBeGreaterThan(20);
    expect(checks[0].id).toBe('S3-01');
  });

  it('flattens links so a check reads as prose', () => {
    expect(checks.some((c) => c.text.includes(']('))).toBe(false);
  });

  it('records the sections each check cites', () => {
    expect(checks.some((c) => c.cites.includes('Section 2.4'))).toBe(true);
  });

  it('throws rather than returning nothing when Section 3 is missing', () => {
    expect(() => extractChecks('# Nothing here')).toThrow(/Section 3 not found/);
  });
});

describe('coverage is honest', () => {
  it('reports what is covered and what is not', () => {
    const r = coverage(checks, scenarios);
    expect(r.checksInSpec).toBe(checks.length);
    expect(r.covered.length + r.uncovered.length).toBe(checks.length);
    // The property that matters: uncovered is reported, not omitted.
    expect(r.uncovered.length).toBeGreaterThan(0);
  });

  it('every scenario binds to a check that exists', () => {
    const ids = new Set(checks.map((c) => c.id));
    for (const s of scenarios) expect(ids.has(s.covers)).toBe(true);
  });

  it('the scenarios written so far have not drifted', () => {
    expect(coverage(checks, scenarios).drifted).toEqual([]);
  });

  it('DETECTS drift when a check is reworded', () => {
    const stale: Scenario = {
      id: 'S3-09-stale',
      covers: 'S3-09',
      kind: 'present',
      coveredTextWas: 'something the specification no longer says',
    };
    const r = coverage(checks, [stale]);
    expect(r.drifted).toEqual([{ scenario: 'S3-09-stale', check: 'S3-09' }]);
    // Drift does not remove coverage — it flags it, so the gap is visible
    // rather than the check silently reverting to unverified.
    expect(r.covered).toContain('S3-09');
  });
});

describe('a declared scenario cannot stand without a test', () => {
  // The registry is a list of claims. This is the claim-check: every entry must
  // name a test that exists. Without it, adding an entry would raise the
  // coverage number on its own — the exact flattery the log exists to prevent.
  const testSource = (async () => {
    const dir = new URL('.', import.meta.url);
    const files = await readdir(dir);
    const bodies = await Promise.all(
      files.filter((f) => f.endsWith('.test.ts')).map((f) => readFile(new URL(f, dir), 'utf-8'))
    );
    return bodies.join('\n');
  })();

  it('every scenario id appears in a test name', async () => {
    const source = await testSource;
    const orphans = scenarios.filter((s) => !source.includes(s.id));
    expect(orphans.map((s) => s.id)).toEqual([]);
  });

  it('every scenario id in a test is declared in the registry', async () => {
    const source = await testSource;
    const declared = new Set(scenarios.map((s) => s.id));
    // A scenario describe is `<slug> — <prose>`. Ordinary describes have no such
    // head, so they are not mistaken for scenario claims.
    const used = new Set(
      [...source.matchAll(/describe\('([a-z0-9][a-z0-9-]{3,}) \u2014 /g)].map((m) => m[1])
    );
    expect([...used].filter((id) => !declared.has(id))).toEqual([]);
  });
});
