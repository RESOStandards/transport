import { beforeEach, describe, expect, it } from 'vitest';
import { createHub, type Hub, type Terms } from '../src/hub.js';
import { sqliteStore } from '../src/store-sqlite.js';
import type { OfferStore } from '../src/store.js';

/**
 * The operations, against the real SQLite store rather than a fake.
 *
 * A fake store would pass every one of these while proving nothing about the
 * schema, and two of the rules under test are enforced by the schema. So the
 * store is real and in memory: no fixture files, no cleanup, and no drift
 * between what the tests exercise and what a server runs.
 *
 * Each `describe` is named for the scenario it executes. That is not decoration:
 * `write-log.ts` reads the suite's results and marks a scenario verified only
 * when a test bearing its id passed, so a scenario declared in the registry with
 * no test behind it is reported as unverified rather than counted.
 */

const LISTING = { ListingId: '11284417', OfferOriginatingSystemName: 'MyMls' };
const LISTING_SIDE = 'M00000001';
const LISTING_BROKERAGE = 'M00000002';
const PG = { OfferPropertyGroupKey: 'PG-1', fields: { City: 'Chicago', StateOrProvince: 'IL' } };
const terms = (price: number): Terms => ({ PurchasePrice: price, EarnestMoney: 15000 });

let store: OfferStore;
let hub: Hub;

beforeEach(() => {
  store = sqliteStore();
  hub = createHub(store);
  hub.publishListing('11284417', [LISTING_SIDE, LISTING_BROKERAGE]);
});

const submit = (over: Partial<Parameters<Hub['submitOffer']>[0]> = {}) =>
  hub.submitOffer({
    coordinate: LISTING,
    buyerRef: 'dana',
    submittingUoi: 'M00000136',
    propertyGroup: PG,
    terms: terms(530000),
    ...over,
  });

/** An offer, or a thrown setup failure. Never a silent skip. */
const anOffer = (over: Partial<Parameters<Hub['submitOffer']>[0]> = {}): string => {
  const r = submit(over);
  if (!r.ok) throw new Error(`setup failed: ${r.reason}`);
  return r.value.offerId;
};

describe('offer-needs-published-listing — an offer must reference a published listing', () => {
  it('refuses an offer on a listing never published for offers', () => {
    const r = hub.submitOffer({
      coordinate: { ListingId: '999', OfferOriginatingSystemName: 'MyMls' },
      buyerRef: 'dana',
      submittingUoi: 'M1',
      propertyGroup: PG,
      terms: terms(1),
    });
    expect(r.ok).toBe(false);
    if (!r.ok) {
      expect(r.code).toBe('listing-not-published');
      // Publishing opens a listing for offers; its marketing status does not.
      expect(r.reason).toMatch(/not its marketing status/);
    }
  });

  it('accepts one once the listing is published', () => {
    expect(submit().ok).toBe(true);
  });
});

describe('submission-correlates-to-offer — a submission must correlate to an offer the candidate holds', () => {
  it('refuses a submission against an OfferId that does not exist', () => {
    expect(() =>
      store.appendSubmission({
        OfferSubmissionKey: 'X', OfferId: 'NO-SUCH-OFFER',
        OfferPropertyGroupKey: 'PG-1', OfferSubmissionSequence: 1,
        SubmittingUoi: 'M1', terms: {},
      })
    ).toThrow(/FOREIGN KEY/i);
  });
});

describe('property-group-resolves — a submission must carry a property group key that resolves', () => {
  it('refuses a submission whose property group is not held', () => {
    const id = anOffer();
    expect(() =>
      store.appendSubmission({
        OfferSubmissionKey: 'X', OfferId: id,
        OfferPropertyGroupKey: 'NOT-THERE', OfferSubmissionSequence: 9,
        SubmittingUoi: 'M1', terms: {},
      })
    ).toThrow(/FOREIGN KEY/i);
  });

  it('accepts one whose group travelled inline with the offer', () => {
    const id = anOffer();
    expect(store.getPropertyGroup(id, 'PG-1')).toBeDefined();
    expect(hub.submissions(id)[0].OfferPropertyGroupKey).toBe('PG-1');
  });
});

describe('second-offer-both-live — a buyer may hold two live offers on one listing', () => {
  it('accepts a second offer while the first is still live', () => {
    expect(submit().ok).toBe(true);
    expect(submit({ terms: terms(560000) }).ok).toBe(true);
    expect(store.offersForBuyer('11284417', 'dana')).toHaveLength(2);
  });

  it('groups them on one listing however each names it', () => {
    // The listing was published as '11284417'. One offer names it by that alone,
    // the other by both members. Both are on the same listing, so both must group
    // under it — deriving the grouping key from the coordinate put them in two
    // different buckets depending on which member each carried.
    anOffer();
    anOffer({
      coordinate: {
        ListingId: '11284417',
        ListingKey: 'MRED-L-11284417',
        OfferOriginatingSystemName: 'MyMls',
      },
      terms: terms(560000),
    });
    expect(store.offersForBuyer('11284417', 'dana')).toHaveLength(2);
  });

  it('gives each its own OfferId rather than merging them', () => {
    const a = anOffer();
    const b = anOffer({ terms: terms(560000) });
    expect(a).not.toBe(b);
    // Keying on listing plus buyer would have collapsed these into one.
    expect(hub.submissions(a)).toHaveLength(1);
    expect(hub.submissions(b)).toHaveLength(1);
  });
});

describe('second-offer-after-ended — a buyer may open a new offer after an earlier one ended', () => {
  it('accepts a new offer once the first was rejected', () => {
    const first = anOffer();
    hub.conclude(first, 'receiving', 'Rejected');
    expect(submit({ terms: terms(555000) }).ok).toBe(true);
    expect(store.offersForBuyer('11284417', 'dana')).toHaveLength(2);
  });
});

describe('ended-offer-takes-no-turns — an ended negotiation takes no more turns', () => {
  const ended = (as: 'Accepted' | 'Rejected' | 'Withdrawn'): string => {
    const id = anOffer();
    hub.conclude(id, 'receiving', as);
    return id;
  };

  it.each(['Accepted', 'Rejected', 'Withdrawn'] as const)('refuses a counter after %s', (as) => {
    const r = hub.counter(ended(as), { submittingUoi: 'M1', terms: terms(1) });
    expect(r.ok).toBe(false);
    if (!r.ok) {
      expect(r.code).toBe('offer-ended');
      expect(r.reason).toMatch(/does not reopen/);
    }
  });

  it('refuses a status act after it ended, too', () => {
    expect(hub.recordAct(ended('Rejected'), 'receiving', 'Acknowledged').ok).toBe(false);
  });
});

describe('coordinate-refused-before-store — an incomplete coordinate is refused before anything is stored', () => {
  it('refuses an organization identifier standing alone', () => {
    const r = submit({ coordinate: { ListingId: '11284417', OfferUoi: 'M00000136' } });
    expect(r.ok).toBe(false);
    if (!r.ok) expect(r.code).toBe('invalid-coordinate');
  });

  it('stores nothing when the coordinate is refused', () => {
    submit({ coordinate: { ListingId: '11284417', OfferUoi: 'M1' } });
    // A refusal that had already written the property group would leave a
    // fragment of an offer that does not exist. Nothing at all is stored, so no
    // offer exists to look under.
    expect(store.offersForBuyer('11284417', 'dana')).toHaveLength(0);
  });
});

describe('counter-leaves-prior-intact — a counter never disturbs what came before', () => {
  it('leaves the prior submission byte-identical', () => {
    const id = anOffer();
    const before = JSON.stringify(store.submissionsFor(id)[0]);
    hub.counter(id, { submittingUoi: 'M9', terms: terms(545000) });
    expect(JSON.stringify(store.submissionsFor(id)[0])).toBe(before);
  });
});

describe('no-terms-act-adds-no-submission — an act that changes no terms creates no submission', () => {
  it('records a status without appending a submission', () => {
    const id = anOffer();
    const before = hub.submissions(id).length;
    expect(hub.recordAct(id, 'receiving', 'Acknowledged').ok).toBe(true);
    expect(hub.submissions(id)).toHaveLength(before);
  });

  it('creates no submission for an acceptance either', () => {
    const id = anOffer();
    const before = hub.submissions(id).length;
    hub.conclude(id, 'receiving', 'Accepted');
    expect(hub.submissions(id)).toHaveLength(before);
  });
});

describe('current-state-is-highest-sequence — current state is the pair on the highest-sequence submission', () => {
  it('reports the pair from the current submission, not an earlier one', () => {
    const id = anOffer();
    hub.counter(id, { submittingUoi: 'M9', terms: terms(545000) });
    hub.recordAct(id, 'receiving', 'Acknowledged');
    const subs = hub.submissions(id);
    expect(store.currentState(id)).toMatchObject({ sequence: 2, receiving: 'Acknowledged' });
    expect(subs[1].OfferReceivedStatus).toBe('Acknowledged');
    // Superseded turns are frozen: the acknowledgement did not reach back.
    expect(subs[0].OfferReceivedStatus).toBeUndefined();
  });

  it('holds at every point in the exchange, not only at the end', () => {
    const id = anOffer();
    expect(store.currentState(id)).toMatchObject({ sequence: 1, submitting: 'Submitted' });
    hub.recordAct(id, 'receiving', 'Acknowledged');
    expect(store.currentState(id)).toMatchObject({ sequence: 1, receiving: 'Acknowledged' });
    hub.counter(id, { submittingUoi: 'M9', terms: terms(545000) });
    // A new turn carries its own pair; it does not inherit the previous one.
    expect(store.currentState(id)).toMatchObject({ sequence: 2, submitting: 'Countered' });
    expect(store.currentState(id)?.receiving).toBeUndefined();
  });
});

describe('only-own-side-status — a party never writes the counterparty status', () => {
  it('writes only the acting side', () => {
    const id = anOffer();
    hub.recordAct(id, 'receiving', 'Acknowledged');
    const state = store.currentState(id);
    expect(state?.receiving).toBe('Acknowledged');
    // The submitting side said "Submitted" and nobody else may change it.
    expect(state?.submitting).toBe('Submitted');
  });
});

describe('disagreeing-statuses-allowed — statuses that disagree are not grounds for refusal', () => {
  it('keeps the offer live and still serving when the two sides differ', () => {
    const id = anOffer();
    hub.recordAct(id, 'submitting', 'Countered');
    hub.recordAct(id, 'receiving', 'Acknowledged');
    const state = store.currentState(id);
    expect(state?.submitting).not.toBe(state?.receiving);
    // Neither the read nor the next turn may fail on the disagreement.
    expect(store.getOffer(id)?.EndedAs).toBeUndefined();
    expect(hub.submissions(id)).toHaveLength(1);
    expect(hub.counter(id, { submittingUoi: 'M9', terms: terms(545000) }).ok).toBe(true);
  });
});

describe('sequence-is-max-plus-one — sequence is one greater than the highest seen', () => {
  it('numbers the first submission 1, then 2, then 3', () => {
    const id = anOffer();
    expect(hub.submissions(id)[0].OfferSubmissionSequence).toBe(1);
    expect(hub.counter(id, { submittingUoi: 'M9', terms: terms(545000) }))
      .toMatchObject({ ok: true, value: { sequence: 2 } });
    expect(hub.counter(id, { submittingUoi: 'M1', terms: terms(540000) }))
      .toMatchObject({ ok: true, value: { sequence: 3 } });
  });

  it('uses the highest seen, not the count, when a sequence is shared', () => {
    const id = anOffer();
    // Two parties acted without seeing each other: both are sequence 2.
    for (const [key, uoi] of [['TIE-A', 'M1'], ['TIE-B', 'M9']] as const) {
      store.appendSubmission({
        OfferSubmissionKey: key, OfferId: id, OfferPropertyGroupKey: 'PG-1',
        OfferSubmissionSequence: 2, SubmittingUoi: uoi, terms: {},
      });
    }
    expect(store.submissionsFor(id)).toHaveLength(3);
    // Counting would reissue 3 twice over. Max-seen-plus-one gives 3 once.
    expect(hub.counter(id, { submittingUoi: 'M1', terms: terms(1) }))
      .toMatchObject({ ok: true, value: { sequence: 3 } });
  });
});

describe('turns-in-sequence-order — every turn stays retrievable, in sequence order', () => {
  it('keeps both turns, ordered by sequence', () => {
    const id = anOffer();
    hub.counter(id, { submittingUoi: 'M9', terms: terms(545000) });
    const subs = hub.submissions(id);
    expect(subs.map((s) => s.OfferSubmissionSequence)).toEqual([1, 2]);
    expect(subs[0].terms.PurchasePrice).toBe(530000);
    expect(subs[1].terms.PurchasePrice).toBe(545000);
  });
});
