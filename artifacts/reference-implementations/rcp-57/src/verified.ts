/**
 * Which scenarios actually ran, and passed.
 *
 * Before this, the certification log took the scenario registry at its word: a
 * declared entry counted as coverage whether or not a test stood behind it.
 * Eight new entries could have moved the log from "6 of 36 verified" to "14 of
 * 36" without a single assertion — the same flattery the log exists to prevent,
 * one level up.
 *
 * So coverage is read from the suite's own results. A scenario is *executed* when
 * a test bearing its id ran, and *verified* only when every such test passed. A
 * registry entry with no test is reported as declared-but-unexecuted, which is
 * louder than being quietly counted.
 *
 * The binding is the scenario id at the head of a `describe` title, as
 * `expansion-is-read-only — ...`. Ids are matched exactly against the registry
 * rather than by pattern, so a renamed scenario reports as unexecuted instead of
 * being matched by something that merely looks like an id.
 */

import { readFile, stat } from 'node:fs/promises';

/** What the suite observed about one scenario. */
export interface Executed {
  readonly scenario: string;
  readonly tests: number;
  readonly passed: number;
  readonly failed: number;
}

export interface SuiteResults {
  /** By scenario id. Absent means no test named it. */
  readonly executed: ReadonlyMap<string, Executed>;
  readonly totalTests: number;
  readonly allPassed: boolean;
  /** When the results were produced, so a stale file can be reported as stale. */
  readonly ranAt: Date;
}

interface Assertion {
  readonly fullName: string;
  readonly status: string;
}

/**
 * Read a vitest JSON report.
 *
 * Throws when the file is missing rather than reporting zero coverage: a missing
 * results file means the suite was not run, and "not run" must never be
 * presentable as "nothing verified, carry on".
 */
export const readSuiteResults = async (
  path: string,
  scenarioIds: readonly string[]
): Promise<SuiteResults> => {
  const raw = await readFile(path, 'utf-8').catch(() => {
    throw new Error(
      `no test results at ${path}. Run \`npm run test:report\` first — the log reports what the suite observed, so it cannot be written without it.`
    );
  });
  const parsed = JSON.parse(raw) as {
    readonly testResults?: ReadonlyArray<{ readonly assertionResults?: readonly Assertion[] }>;
    readonly numTotalTests?: number;
  };
  const assertions = (parsed.testResults ?? []).flatMap((f) => f.assertionResults ?? []);
  if (assertions.length === 0) throw new Error(`no test results found in ${path}`);

  const tally = new Map<string, { tests: number; passed: number; failed: number }>();
  for (const a of assertions) {
    // The slug at the head of the describe title, compared exactly. Substring
    // matching would let one scenario id that happens to contain another count
    // for both, which is the guarantee the module comment above promises.
    const head = /^([a-z0-9][a-z0-9-]{3,}) \u2014 /.exec(a.fullName)?.[1];
    for (const id of scenarioIds.filter((i) => i === head)) {
      const e = tally.get(id) ?? { tests: 0, passed: 0, failed: 0 };
      tally.set(id, {
        tests: e.tests + 1,
        passed: e.passed + (a.status === 'passed' ? 1 : 0),
        failed: e.failed + (a.status === 'failed' ? 1 : 0),
      });
    }
  }

  return {
    executed: new Map([...tally].map(([scenario, e]) => [scenario, { scenario, ...e }])),
    totalTests: parsed.numTotalTests ?? assertions.length,
    allPassed: assertions.every((a) => a.status === 'passed'),
    ranAt: (await stat(path)).mtime,
  };
};

/**
 * Whether the results predate the code they claim to describe.
 *
 * A log regenerated without rerunning the suite would report yesterday's
 * coverage as today's. Cheap to check, and the failure it prevents is silent.
 */
export const stalenessOf = async (
  resultsPath: string,
  sources: readonly string[]
): Promise<{ readonly stale: boolean; readonly newer: readonly string[] }> => {
  const ranAt = (await stat(resultsPath)).mtimeMs;
  const newer: string[] = [];
  for (const s of sources) {
    const m = await stat(s).catch(() => undefined);
    if (m !== undefined && m.mtimeMs > ranAt) newer.push(s);
  }
  return { stale: newer.length > 0, newer };
};

/** Verified = executed, with nothing failing. A failure is not coverage. */
export const isVerified = (results: SuiteResults, scenario: string): boolean => {
  const e = results.executed.get(scenario);
  return e !== undefined && e.tests > 0 && e.failed === 0;
};
