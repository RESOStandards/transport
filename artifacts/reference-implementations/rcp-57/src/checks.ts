/**
 * The certification checks, read from the specification rather than copied.
 *
 * Section 3 is the list of what a candidate must do. Hand-copying it into code
 * guarantees drift: the specification moved eleven times today alone, and a
 * transcribed list would already be stale. So the list is extracted, and the
 * implementation declares which extracted check each scenario covers.
 *
 * That gives the property worth having — **coverage is reportable**. The runner
 * can say which checks it verifies and, more usefully, which it does not. A
 * certification tool that silently tests a subset reads as a passing grade.
 *
 * Follows the Web API Core pattern in `reso-certification`: a small set of
 * observation primitives covering many scenarios, declared as data, with an
 * indeterminate result for what could not be evaluated rather than a false pass.
 */

/** How a check is observed. Kinds, not one function per rule. */
export type ObservationKind =
  /** A served schema matches declared resources, fields, types, nullability. */
  | 'serves'
  /** A valid input is accepted. */
  | 'accepts'
  /** An invalid input is refused. */
  | 'rejects'
  /** A required member, or one of a permitted set, is present. */
  | 'present'
  /** A reference resolves to something the candidate holds. */
  | 'resolves'
  /** Prior state is unchanged across an operation. */
  | 'immutable'
  /** Values are monotonic, or ordered by a stated rule. */
  | 'sequence'
  /** Content is absent from where it must not appear. */
  | 'absent'
  /** A request is refused without credentials. */
  | 'refuses'
  /** The same input yields the same output. */
  | 'reproducible'
  /** A value is taken from the right source and not another. */
  | 'derives';

/** One check as Section 3 states it. */
export interface SpecCheck {
  /** Stable id: `S3-01`. Positional, so it moves when the specification does —
   *  which is why a scenario also carries the check's text for a drift guard. */
  readonly id: string;
  /** The check, verbatim, with markdown links flattened. */
  readonly text: string;
  /** Sections it cites. */
  readonly cites: readonly string[];
}

/** A scenario the runner can execute, bound to the check it covers. */
export interface Scenario {
  readonly id: string;
  readonly covers: string;
  readonly kind: ObservationKind;
  /** The check's text when the scenario was written. A mismatch against the
   *  current specification means the check changed and the scenario may no
   *  longer test what it claims — reported, never silently accepted. */
  readonly coveredTextWas: string;
}

/** What a scenario produced. `indeterminate` is not a pass. */
export interface ObservationResult {
  readonly scenario: string;
  readonly passed: boolean;
  readonly message: string;
  readonly indeterminate?: boolean;
}

/** Coverage of the specification by the scenarios that exist. */
export interface CoverageReport {
  readonly checksInSpec: number;
  readonly covered: readonly string[];
  readonly uncovered: readonly SpecCheck[];
  /** Scenarios whose check text has changed since they were written. */
  readonly drifted: readonly { readonly scenario: string; readonly check: string }[];
}

const LINK = /\[([^\]]+)\]\([^)]*\)/g;
const SECTION = /\[(Section [\d.]+|[A-Z][^\]]*)\]\(#([^)]+)\)/g;

/**
 * Read Section 3 out of the specification.
 *
 * Deliberately tolerant of formatting and strict about structure: it finds the
 * section by heading, then takes bullet items. A specification that stops using
 * bullets for checks should fail loudly here rather than quietly return fewer.
 */
export const extractChecks = (specMarkdown: string): readonly SpecCheck[] => {
  const marker = '# Section 3: Certification';
  const at = specMarkdown.indexOf(marker);
  if (at < 0) throw new Error('Section 3 not found: the specification changed shape.');
  const body = specMarkdown.slice(at + marker.length).split('\n# Section 4')[0];

  const items = [...body.matchAll(/^\* (.+?)(?=\n\* |\n\n|\n#|$)/gms)].map((m) => m[1]);
  if (items.length === 0) throw new Error('Section 3 has no bullet items: extraction is broken.');

  return items.map((raw, i) => {
    const cites = [...raw.matchAll(SECTION)].map((m) => m[1]);
    return {
      id: `S3-${String(i + 1).padStart(2, '0')}`,
      text: raw.replace(LINK, '$1').replace(/\s+/g, ' ').trim(),
      cites: [...new Set(cites)],
    };
  });
};

/** Which checks the scenarios cover, which they do not, and which have drifted. */
export const coverage = (
  checks: readonly SpecCheck[],
  scenarios: readonly Scenario[]
): CoverageReport => {
  const byId = new Map(checks.map((c) => [c.id, c]));
  const covered = [...new Set(scenarios.map((s) => s.covers))].filter((id) => byId.has(id));
  const drifted = scenarios
    .filter((s) => {
      const c = byId.get(s.covers);
      return c !== undefined && c.text !== s.coveredTextWas;
    })
    .map((s) => ({ scenario: s.id, check: s.covers }));
  return {
    checksInSpec: checks.length,
    covered,
    uncovered: checks.filter((c) => !covered.includes(c.id)),
    drifted,
  };
};
