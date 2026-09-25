/**
 * Where this package reads the specification and writes what it generates.
 *
 * Resolved from this module's own location rather than from the working
 * directory, so every script and test agrees on the same files however it was
 * invoked. The package previously hard-coded one contributor's home directory in
 * ten places across eight files, which worked on exactly one machine.
 *
 * The layout this assumes, inside the transport repository:
 *
 *     proposals/offer-management.md                     <- the specification
 *     artifacts/reference-implementations/rcp-57/       <- this package
 *
 * `import.meta.url` is a file in `src/` when vitest runs the TypeScript directly
 * and a file in `dist/` when a built script runs, and both sit one level under
 * the package root, so one relative path serves both.
 */

import { fileURLToPath } from 'node:url';

/** The package root: `artifacts/reference-implementations/rcp-57/`. */
export const packageRoot = new URL('../', import.meta.url);

/** The repository root, four levels up from `src/` or `dist/`. */
const repoRoot = new URL('../../../../', import.meta.url);

/**
 * The specification this package reads.
 *
 * `RCP57_SPEC` overrides it, so the package can be pointed at a draft elsewhere
 * without editing anything. A consumer outside this repository needs that.
 */
export const specPath: string =
  process.env.RCP57_SPEC ?? fileURLToPath(new URL('proposals/offer-management.md', repoRoot));

/** The generated metadata report, written beside this package. */
export const metadataReportPath: string = fileURLToPath(
  new URL('metadata-report.json', packageRoot)
);

/** The generated certification log, written beside this package. */
export const certificationLogPath: string = fileURLToPath(
  new URL('CERTIFICATION-LOG.md', packageRoot)
);

/** The suite's JSON report, which the certification log reads for coverage. */
export const testResultsPath: string = fileURLToPath(
  new URL('test-results.json', packageRoot)
);
