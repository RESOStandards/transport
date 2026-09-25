/**
 * Submission ordering.
 *
 * ActivityPub does not guarantee delivery order, so arrival order is not
 * evidence of sequence. Each submission carries `OfferSubmissionSequence`, a
 * logical clock of the same shape as `EntityEventSequence`.
 *
 * The part that is easy to implement wrongly is the tie. Two submissions of one
 * offer MAY share a sequence, and that is not a defect to repair: it means both
 * parties acted without having seen the other, which is a real event and the
 * reason the number is worth carrying. The tie is broken on the submitting
 * party's organization identifier, ascending, because every party must reach the
 * same ordering from the same facts — and arrival time differs per recipient, so
 * a tie broken on arrival produces a different history for each party.
 *
 * That last point is why this module exists separately from a sort call. The
 * obvious implementation is `sort by sequence, then by timestamp`, and it is
 * wrong in exactly the case the sequence number was introduced to handle.
 */

import type { ObservationResult } from './checks.js';

/** The minimum a record needs to be ordered. The store's rows satisfy it too. */
export interface Ordered {
  readonly OfferSubmissionSequence: number;
  /** The submitting party. Breaks a tie, ascending. */
  readonly SubmittingUoi: string;
}

export interface Submission extends Ordered {
  readonly OfferSubmissionKey: string;
  readonly OfferId: string;
  /** Present, and deliberately NOT used for ordering. */
  readonly OfferSubmissionTimestamp?: string;
}

/** The next sequence for an offer: one more than the highest seen, or 1. */
export const nextSequence = (existing: readonly Submission[]): number =>
  existing.length === 0
    ? 1
    : Math.max(...existing.map((s) => s.OfferSubmissionSequence)) + 1;

/**
 * The order every party must agree on.
 *
 * Sequence ascending, ties broken on the submitting organization identifier
 * ascending. Timestamps are never consulted: clocks differ, and a counter may
 * legitimately carry an earlier one than the submission it answers.
 */
export const orderSubmissions = <T extends Ordered>(subs: readonly T[]): readonly T[] =>
  [...subs].sort((a, b) =>
    a.OfferSubmissionSequence !== b.OfferSubmissionSequence
      ? a.OfferSubmissionSequence - b.OfferSubmissionSequence
      : a.SubmittingUoi.localeCompare(b.SubmittingUoi)
  );

/** S3-16: a created submission is numbered max-seen plus one, and never renumbered. */
export const observeAssignsSequence = (
  scenario: string,
  before: readonly Submission[],
  created: Submission
): ObservationResult => {
  const expected = nextSequence(before);
  if (created.OfferSubmissionSequence === expected) {
    return {
      scenario,
      passed: true,
      message: `numbered ${expected}, one greater than the highest of ${before.length} prior`,
    };
  }
  return {
    scenario,
    passed: false,
    message:
      `numbered ${created.OfferSubmissionSequence}, expected ${expected}. ` +
      (created.OfferSubmissionSequence < expected
        ? 'A number at or below one already used collides with a real submission.'
        : 'Skipping numbers loses the guarantee that a gap means a missing submission.'),
  };
};

/**
 * S3-17: a tie is broken on the organization identifier, never on arrival.
 *
 * Observed by ordering the same pair twice under opposite arrival orders. An
 * implementation that consults arrival time produces two different answers, and
 * that difference is the whole finding — a suite that checked one arrival order
 * would pass a broken implementation half the time.
 */
export const observeTieBreak = (
  scenario: string,
  tied: readonly Submission[],
  orderUnderTest: (subs: readonly Submission[]) => readonly Submission[]
): ObservationResult => {
  if (new Set(tied.map((s) => s.OfferSubmissionSequence)).size !== 1) {
    return {
      scenario,
      passed: false,
      indeterminate: true,
      message: 'the fixture is not a tie, so no tie-break is observable. Fix the fixture.',
    };
  }
  const forward = orderUnderTest(tied).map((s) => s.OfferSubmissionKey);
  const reversed = orderUnderTest([...tied].reverse()).map((s) => s.OfferSubmissionKey);
  const expected = orderSubmissions(tied).map((s) => s.OfferSubmissionKey);

  if (forward.join() !== reversed.join()) {
    return {
      scenario,
      passed: false,
      message:
        'ordering changed when arrival order changed, so the tie is broken on ' +
        'arrival. Every recipient would then hold a different history.',
    };
  }
  return forward.join() === expected.join()
    ? { scenario, passed: true, message: 'tie broken on organization identifier, ascending' }
    : {
        scenario,
        passed: false,
        message: `stable but wrong: got ${forward.join(', ')}, expected ${expected.join(', ')}`,
      };
};

/** S3-18: prior submissions stay retrievable, and come back in sequence order. */
export const observeRetrievableInOrder = (
  scenario: string,
  created: readonly Submission[],
  returned: readonly Submission[]
): ObservationResult => {
  const missing = created
    .filter((c) => !returned.some((r) => r.OfferSubmissionKey === c.OfferSubmissionKey))
    .map((c) => c.OfferSubmissionKey);
  if (missing.length > 0) {
    return {
      scenario,
      passed: false,
      message: `created but not returned: ${missing.join(', ')}. A submission is evidence, not a draft.`,
    };
  }
  const expected = orderSubmissions(created).map((s) => s.OfferSubmissionKey);
  const actual = returned.map((s) => s.OfferSubmissionKey);
  return actual.join() === expected.join()
    ? { scenario, passed: true, message: `all ${created.length} returned in sequence order` }
    : {
        scenario,
        passed: false,
        message: `returned ${actual.join(', ')}, expected ${expected.join(', ')}`,
      };
};

/** S3-12: superseded submissions are byte-identical to what they held. */
export const observeSupersededUnchanged = (
  scenario: string,
  beforeCounter: readonly Submission[],
  afterCounter: readonly Submission[]
): ObservationResult => {
  const after = new Map(afterCounter.map((s) => [s.OfferSubmissionKey, s]));
  const changed = beforeCounter
    .filter((b) => {
      const a = after.get(b.OfferSubmissionKey);
      return a === undefined || JSON.stringify(a) !== JSON.stringify(b);
    })
    .map((b) => b.OfferSubmissionKey);
  return changed.length === 0
    ? { scenario, passed: true, message: `${beforeCounter.length} superseded submissions unchanged` }
    : {
        scenario,
        passed: false,
        message:
          `modified or dropped after a counter: ${changed.join(', ')}. ` +
          'Destroying a prior turn destroys the record of what was agreed and when.',
      };
};
