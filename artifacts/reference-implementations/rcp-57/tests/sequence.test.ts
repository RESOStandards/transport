import { describe, expect, it } from 'vitest';
import {
  type Submission,
  nextSequence,
  observeAssignsSequence,
  observeRetrievableInOrder,
  observeSupersededUnchanged,
  observeTieBreak,
  orderSubmissions,
} from '../src/sequence.js';

const sub = (over: Partial<Submission> = {}): Submission => ({
  OfferSubmissionKey: 'k1',
  OfferId: 'OFFER-1',
  OfferSubmissionSequence: 1,
  SubmittingUoi: 'M00000136',
  ...over,
});

describe('nextSequence', () => {
  it('starts at 1 for the first submission of an offer', () => {
    expect(nextSequence([])).toBe(1);
  });

  it('is one greater than the highest seen, not the count', () => {
    // The distinction matters: a tie means two submissions share a number, so
    // counting would reissue a number already in use.
    const tied = [
      sub({ OfferSubmissionKey: 'a', OfferSubmissionSequence: 2, SubmittingUoi: 'A' }),
      sub({ OfferSubmissionKey: 'b', OfferSubmissionSequence: 2, SubmittingUoi: 'B' }),
    ];
    expect(tied.length).toBe(2);
    expect(nextSequence(tied)).toBe(3);
  });
});

describe('orderSubmissions', () => {
  it('orders by sequence ascending', () => {
    const out = orderSubmissions([
      sub({ OfferSubmissionKey: 'c', OfferSubmissionSequence: 3 }),
      sub({ OfferSubmissionKey: 'a', OfferSubmissionSequence: 1 }),
      sub({ OfferSubmissionKey: 'b', OfferSubmissionSequence: 2 }),
    ]);
    expect(out.map((s) => s.OfferSubmissionKey)).toEqual(['a', 'b', 'c']);
  });

  it('breaks a tie on the organization identifier, ascending', () => {
    const out = orderSubmissions([
      sub({ OfferSubmissionKey: 'z', OfferSubmissionSequence: 2, SubmittingUoi: 'M9' }),
      sub({ OfferSubmissionKey: 'a', OfferSubmissionSequence: 2, SubmittingUoi: 'M1' }),
    ]);
    expect(out.map((s) => s.OfferSubmissionKey)).toEqual(['a', 'z']);
  });

  it('ignores timestamps entirely, including one that contradicts the sequence', () => {
    // A counter may legitimately carry an earlier timestamp than the submission
    // it answers, because the clocks belong to different systems.
    const out = orderSubmissions([
      sub({
        OfferSubmissionKey: 'later-seq-earlier-clock',
        OfferSubmissionSequence: 2,
        OfferSubmissionTimestamp: '2026-01-01T00:00:00Z',
      }),
      sub({
        OfferSubmissionKey: 'earlier-seq-later-clock',
        OfferSubmissionSequence: 1,
        OfferSubmissionTimestamp: '2026-12-31T00:00:00Z',
      }),
    ]);
    expect(out.map((s) => s.OfferSubmissionKey)).toEqual([
      'earlier-seq-later-clock',
      'later-seq-earlier-clock',
    ]);
  });

  it('does not mutate its input', () => {
    const given = [
      sub({ OfferSubmissionKey: 'b', OfferSubmissionSequence: 2 }),
      sub({ OfferSubmissionKey: 'a', OfferSubmissionSequence: 1 }),
    ];
    orderSubmissions(given);
    expect(given.map((s) => s.OfferSubmissionKey)).toEqual(['b', 'a']);
  });
});

describe('sequence-is-not-a-count — sequence assignment', () => {
  it('passes on max-seen plus one', () => {
    const before = [sub({ OfferSubmissionSequence: 4 })];
    const r = observeAssignsSequence('S3-16-a', before, sub({ OfferSubmissionSequence: 5 }));
    expect(r.passed).toBe(true);
  });

  it('fails a reused number, and says why it is dangerous', () => {
    const before = [sub({ OfferSubmissionSequence: 4 })];
    const r = observeAssignsSequence('S3-16-a', before, sub({ OfferSubmissionSequence: 4 }));
    expect(r.passed).toBe(false);
    expect(r.message).toMatch(/collides with a real submission/);
  });

  it('fails a skipped number, and says what the gap would have meant', () => {
    const before = [sub({ OfferSubmissionSequence: 4 })];
    const r = observeAssignsSequence('S3-16-a', before, sub({ OfferSubmissionSequence: 9 }));
    expect(r.passed).toBe(false);
    expect(r.message).toMatch(/gap means a missing submission/);
  });
});

describe('tie-breaks-on-organization — the tie-break', () => {
  const tied = [
    sub({ OfferSubmissionKey: 'buyer', OfferSubmissionSequence: 2, SubmittingUoi: 'M1' }),
    sub({ OfferSubmissionKey: 'lister', OfferSubmissionSequence: 2, SubmittingUoi: 'M9' }),
  ];

  it('passes a correct implementation', () => {
    expect(observeTieBreak('S3-17-a', tied, orderSubmissions).passed).toBe(true);
  });

  it('CATCHES an implementation that breaks the tie on arrival order', () => {
    // The obvious wrong implementation: stable sort on sequence alone, so ties
    // keep whatever order they arrived in.
    const byArrival = (subs: readonly Submission[]): readonly Submission[] =>
      [...subs].sort((a, b) => a.OfferSubmissionSequence - b.OfferSubmissionSequence);
    const r = observeTieBreak('S3-17-a', tied, byArrival);
    expect(r.passed).toBe(false);
    expect(r.message).toMatch(/broken on arrival/);
    expect(r.message).toMatch(/different history/);
  });

  it('catches a stable but wrongly-directed tie-break', () => {
    const descending = (subs: readonly Submission[]): readonly Submission[] =>
      [...subs].sort((a, b) =>
        a.OfferSubmissionSequence !== b.OfferSubmissionSequence
          ? a.OfferSubmissionSequence - b.OfferSubmissionSequence
          : b.SubmittingUoi.localeCompare(a.SubmittingUoi)
      );
    const r = observeTieBreak('S3-17-a', tied, descending);
    expect(r.passed).toBe(false);
    expect(r.message).toMatch(/stable but wrong/);
  });

  it('is indeterminate when the fixture contains no tie', () => {
    const notTied = [sub({ OfferSubmissionSequence: 1 }), sub({ OfferSubmissionSequence: 2 })];
    const r = observeTieBreak('S3-17-a', notTied, orderSubmissions);
    expect(r.indeterminate).toBe(true);
    expect(r.passed).toBe(false);
  });
});

describe('prior-turns-retrievable — prior submissions stay retrievable, in order', () => {
  const created = [
    sub({ OfferSubmissionKey: 'a', OfferSubmissionSequence: 1 }),
    sub({ OfferSubmissionKey: 'b', OfferSubmissionSequence: 2 }),
    sub({ OfferSubmissionKey: 'c', OfferSubmissionSequence: 3 }),
  ];

  it('passes when all are returned in sequence order', () => {
    expect(observeRetrievableInOrder('S3-18-a', created, created).passed).toBe(true);
  });

  it('fails when a prior submission has vanished', () => {
    const r = observeRetrievableInOrder('S3-18-a', created, created.slice(1));
    expect(r.passed).toBe(false);
    expect(r.message).toMatch(/evidence, not a draft/);
  });

  it('fails when they come back out of order', () => {
    const r = observeRetrievableInOrder('S3-18-a', created, [...created].reverse());
    expect(r.passed).toBe(false);
    expect(r.message).toMatch(/expected a, b, c/);
  });
});

describe('superseded-submission-frozen — superseded submissions are unchanged', () => {
  const before = [sub({ OfferSubmissionKey: 'a', OfferSubmissionSequence: 1 })];

  it('passes when the counter left them alone', () => {
    expect(observeSupersededUnchanged('S3-12-a', before, [...before]).passed).toBe(true);
  });

  it('fails when a superseded submission was edited', () => {
    const edited = [sub({ OfferSubmissionKey: 'a', OfferSubmissionSequence: 1, SubmittingUoi: 'CHANGED' })];
    const r = observeSupersededUnchanged('S3-12-a', before, edited);
    expect(r.passed).toBe(false);
    expect(r.message).toMatch(/destroys the record/);
  });

  it('fails when a superseded submission was dropped', () => {
    const r = observeSupersededUnchanged('S3-12-a', before, []);
    expect(r.passed).toBe(false);
  });
});
