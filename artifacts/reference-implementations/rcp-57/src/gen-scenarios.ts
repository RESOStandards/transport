/**
 * Regenerate the scenario registry from the specification.
 *
 * Scenarios bind to checks by **a distinctive phrase, not by check id**, and they
 * are named for what they observe rather than for the check's number. Check ids
 * are positional: `S3-04` is simply the fourth bullet, so inserting a check into
 * Section 3 renumbers every check after it.
 *
 * Both halves of that matter. Keyed by id, regeneration would rebind each scenario
 * to whatever check had moved into its old slot and report no drift, because the
 * text it compares against would have been rewritten in the same pass. Silent, and
 * in the direction that flatters. Named for the id, every test title would also go
 * stale on the same insertion, which is how a suite ends up labeled with numbers
 * that no longer mean anything. Adding two checks in one pass renumbered
 * twenty-four of them, which is what prompted this.
 *
 * An anchor has to match exactly one check or this fails. Matching none means the
 * specification dropped or reworded the rule; matching several means the anchor
 * is not distinctive enough to bind anything.
 */
import { readFile, writeFile } from 'node:fs/promises';
import { extractChecks } from './checks.js';
import { specPath } from './paths.js';


interface Binding {
  /** A stable slug naming what is observed. Survives renumbering. */
  readonly id: string;
  readonly kind: string;
  /** A phrase unique to the check this scenario observes. */
  readonly anchor: string;
}

const bindings: readonly Binding[] = [
  { id: 'offer-needs-published-listing', kind: 'rejects', anchor: 'references no published listing activity' },
  { id: 'submission-correlates-to-offer', kind: 'resolves', anchor: 'correlate to an `Offer` it holds' },
  { id: 'property-group-resolves', kind: 'resolves', anchor: 'that resolves, whether the group travels inline' },
  { id: 'property-group-inline-or-by-key', kind: 'accepts', anchor: 'that resolves, whether the group travels inline' },
  { id: 'either-payload-form', kind: 'accepts', anchor: 'on its own and expanded into its `Offer`' },
  { id: 'second-offer-both-live', kind: 'accepts', anchor: 'second `Offer` from the same buyer' },
  { id: 'second-offer-after-ended', kind: 'accepts', anchor: 'second `Offer` from the same buyer' },
  { id: 'ended-offer-takes-no-turns', kind: 'rejects', anchor: 'withdrawn, rejected or has expired' },
  { id: 'coordinate-needs-system-member', kind: 'present', anchor: 'carrying `OfferUoi` and neither of that pair fails' },
  { id: 'coordinate-refused-before-store', kind: 'rejects', anchor: 'carrying `OfferUoi` and neither of that pair fails' },
  { id: 'superseded-submission-frozen', kind: 'immutable', anchor: 'byte-identical' },
  { id: 'counter-leaves-prior-intact', kind: 'immutable', anchor: 'byte-identical' },
  { id: 'expansion-is-read-only', kind: 'refuses', anchor: 'through the `Submissions` expansion' },
  { id: 'no-terms-act-adds-no-submission', kind: 'absent', anchor: 'MUST NOT create a submission for it' },
  { id: 'current-state-is-highest-sequence', kind: 'derives', anchor: 'pair of statuses on its highest-sequence submission' },
  { id: 'only-own-side-status', kind: 'immutable', anchor: "MUST NOT set the counterparty's status field" },
  { id: 'disagreeing-statuses-allowed', kind: 'accepts', anchor: "MUST NOT set the counterparty's status field" },
  { id: 'sequence-is-max-plus-one', kind: 'sequence', anchor: 'one greater than the highest it has seen' },
  { id: 'sequence-is-not-a-count', kind: 'sequence', anchor: 'one greater than the highest it has seen' },
  { id: 'tie-breaks-on-organization', kind: 'sequence', anchor: 'Unique Organization Identifier ascending' },
  { id: 'prior-turns-retrievable', kind: 'sequence', anchor: 'remain retrievable under its `OfferId`' },
  { id: 'turns-in-sequence-order', kind: 'sequence', anchor: 'remain retrievable under its `OfferId`' },
  { id: 'name-only-coordinate-accepted', kind: 'accepts', anchor: 'no organization identifier was supplied for the listing' },
  { id: 'payload-refuses-unauthenticated', kind: 'refuses', anchor: 'MUST refuse an unauthenticated dereference' },
  { id: 'identity-from-token-only', kind: 'derives', anchor: "requester's identifier from the presented token" },
  { id: 'no-delegated-entitlement', kind: 'refuses', anchor: "another participant's assertion" },
  { id: 'non-party-refused', kind: 'refuses', anchor: 'requester outside the parties' },
  { id: 'withheld-buyer-fields-not-an-error', kind: 'present', anchor: 'absence of buyer or co-buyer fields' },
  { id: 'refusal-does-not-disclose-existence', kind: 'refuses', anchor: 'the same status and the same body whether or not that offer exists' },
];

const checks = extractChecks(await readFile(specPath, 'utf-8'));

const out = bindings.map((b) => {
  const hits = checks.filter((c) => c.text.includes(b.anchor));
  if (hits.length === 0) {
    throw new Error(
      `${b.id}: no check contains "${b.anchor}". The rule was reworded or removed — read the current Section 3 and update the anchor deliberately.`
    );
  }
  if (hits.length > 1) {
    throw new Error(
      `${b.id}: "${b.anchor}" matches ${hits.length} checks (${hits.map((h) => h.id).join(', ')}). An anchor must identify one.`
    );
  }
  return { id: b.id, kind: b.kind, covers: hits[0].id, coveredTextWas: hits[0].text };
});

/** Wrap at a readable width, as concatenated literals that rejoin exactly. */
const asLiteral = (text: string, indent: string): string => {
  const words = text.split(' ');
  const lines: string[] = [];
  let line = '';
  for (const w of words) {
    if (line !== '' && (line + ' ' + w).length > 86) {
      lines.push(line);
      line = w;
    } else {
      line = line === '' ? w : line + ' ' + w;
    }
  }
  if (line !== '') lines.push(line);
  return lines
    .map((l, i) => indent + JSON.stringify(l + (i < lines.length - 1 ? ' ' : '')))
    .join(' +\n');
};

const header = `/**
 * The scenarios that exist, bound to the checks they cover.
 *
 * GENERATED by \`gen-scenarios.ts\`. Do not edit — edit the anchor table there.
 *
 * \`coveredTextWas\` comes from the specification rather than being typed, so a
 * scenario cannot be born already drifted. Regenerate after deliberately
 * re-reading a changed check, never to silence a drift report.
 *
 * A scenario here is a *claim* that something is observed, and the claim is
 * checked twice: \`write-log.ts\` counts it only when a test bearing its id passed,
 * and \`coverage.test.ts\` fails when an entry has no test behind it. Declaring an
 * entry is not a way to raise the coverage number.
 */
import type { Scenario } from './checks.js';

export const scenarios: readonly Scenario[] = [
`;

const body = out
  .map(
    (s) =>
      `  {\n    id: ${JSON.stringify(s.id)},\n    covers: ${JSON.stringify(s.covers)},\n` +
      `    kind: ${JSON.stringify(s.kind)},\n    coveredTextWas:\n${asLiteral(s.coveredTextWas, '      ')},\n  },`
  )
  .join('\n');

const target = new URL('../src/scenarios.ts', import.meta.url);
await writeFile(target, `${header}${body}\n];\n`, 'utf-8');
console.log(`  wrote src/scenarios.ts — ${out.length} scenarios over ${new Set(out.map((s) => s.covers)).size} checks`);
