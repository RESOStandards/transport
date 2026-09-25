import { beforeEach, describe, expect, it } from 'vitest';
import { createHub, type Hub } from '../src/hub.js';
import { readOfferDocument } from '../src/read-payload.js';
import { createOfferServer, listen } from '../src/server.js';
import { sqliteStore } from '../src/store-sqlite.js';
import type { OfferStore } from '../src/store.js';
import { tokenResolver } from '../src/auth.js';

/**
 * Section 2.5's two forms, and the rule that the expansion is read-only.
 *
 * The standalone documents here are the ones Section 2.12 shows, so what is
 * tested is the specification's own examples rather than a shape convenient to
 * the implementation.
 */

const STANDALONE = {
  '@reso.context': 'urn:reso:metadata:2.1:resource:offersubmission',
  OfferSubmissionKey: '9f2c7a10',
  OfferId: 'MRED-2026-0004412',
  OfferSubmissionSequence: 1,
  OfferPropertyGroupKey: 'PG-1',
  PurchasePrice: 530000,
  PropertyGroup: { StreetNumber: '1803', City: 'Chicago' },
};

const EXPANDED = {
  '@reso.context': 'urn:reso:metadata:2.1:resource:offer',
  OfferKey: '3d51a08c',
  OfferId: 'MRED-2026-0004412',
  ListingId: '11284417',
  Submissions: [
    { OfferSubmissionSequence: 1, OfferSubmissionStatus: 'Submitted', PurchasePrice: 530000 },
    { OfferSubmissionSequence: 2, OfferSubmissionStatus: 'Countered', PurchasePrice: 545000 },
  ],
};

describe('either-payload-form — either form is accepted, and neither is required', () => {
  it('reads a submission that travelled on its own', () => {
    const r = readOfferDocument(STANDALONE);
    expect(r.ok).toBe(true);
    if (r.ok) {
      expect(r.value.form).toBe('standalone');
      expect(r.value.OfferId).toBe('MRED-2026-0004412');
      expect(r.value.submissions).toHaveLength(1);
      expect(r.value.submissions[0].fields.PurchasePrice).toBe(530000);
    }
  });

  it('reads submissions expanded into their offer', () => {
    const r = readOfferDocument(EXPANDED);
    expect(r.ok).toBe(true);
    if (r.ok) {
      expect(r.value.form).toBe('expanded');
      expect(r.value.submissions).toHaveLength(2);
      expect(r.value.submissions.map((s) => s.OfferSubmissionSequence)).toEqual([1, 2]);
    }
  });

  it('gives both forms the same OfferId, so neither is privileged', () => {
    const a = readOfferDocument(STANDALONE);
    const b = readOfferDocument(EXPANDED);
    expect(a.ok && b.ok && a.value.OfferId === b.value.OfferId).toBe(true);
  });

  it('carries OfferId down to a submission that omitted it', () => {
    const r = readOfferDocument(EXPANDED);
    // Expanded, the offer already states it, so an element need not repeat it.
    expect(r.ok && r.value.submissions.every((s) => s.OfferId === 'MRED-2026-0004412')).toBe(true);
  });

  it('decides the form from the context, not from which members are present', () => {
    // An offer with no submissions yet is an offer, not a standalone submission.
    const r = readOfferDocument({
      '@reso.context': 'urn:reso:metadata:2.1:resource:offer',
      OfferId: 'X',
    });
    expect(r.ok).toBe(true);
    if (r.ok) {
      expect(r.value.form).toBe('expanded');
      expect(r.value.submissions).toEqual([]);
    }
  });

  it.each([
    ['a document that is not an object', 42, /not a JSON object/],
    ['no context', { OfferId: 'X' }, /carries no @reso.context/],
    ['an unknown resource', { '@reso.context': 'urn:reso:x:resource:listing' }, /unrecognized/],
    [
      'Submissions that is not an array',
      { '@reso.context': 'urn:reso:metadata:2.1:resource:offer', OfferId: 'X', Submissions: {} },
      /not an array/,
    ],
    [
      'a submission correlating to no offer',
      { '@reso.context': 'urn:reso:metadata:2.1:resource:offersubmission' },
      /correlates to no offer/,
    ],
  ])('refuses %s, and says why', (_label, doc, reason) => {
    const r = readOfferDocument(doc);
    expect(r.ok).toBe(false);
    if (!r.ok) expect(r.reason).toMatch(reason);
  });
});

describe('property-group-inline-or-by-key — the property group travels inline or by key, and inline is not required', () => {
  it('reads it inline when present', () => {
    const r = readOfferDocument(STANDALONE);
    expect(r.ok && r.value.submissions[0].PropertyGroup?.City).toBe('Chicago');
  });

  it('accepts a submission carrying only the key', () => {
    const { PropertyGroup: _omitted, ...byKey } = STANDALONE;
    const r = readOfferDocument(byKey);
    expect(r.ok).toBe(true);
    if (r.ok) {
      expect(r.value.submissions[0].OfferPropertyGroupKey).toBe('PG-1');
      expect(r.value.submissions[0].PropertyGroup).toBeUndefined();
    }
  });

  it('keeps the group out of the terms, so it is not mistaken for one', () => {
    const r = readOfferDocument(STANDALONE);
    expect(r.ok && 'PropertyGroup' in r.value.submissions[0].fields).toBe(false);
    expect(r.ok && '@reso.context' in r.value.submissions[0].fields).toBe(false);
  });
});

describe('expansion-is-read-only — no submission is created, altered or removed through the expansion', () => {
  let store: OfferStore;
  let hub: Hub;
  let base: string;
  let stop: () => Promise<void>;
  let offerId: string;

  beforeEach(async () => {
    store = sqliteStore();
    hub = createHub(store);
    hub.publishListing('11284417', ['M00000001']);
    const r = hub.submitOffer({
      coordinate: { ListingId: '11284417', OfferOriginatingSystemName: 'MyMls' },
      buyerRef: 'dana',
      submittingUoi: 'M00000136',
      propertyGroup: { OfferPropertyGroupKey: 'PG-1', fields: { City: 'Chicago' } },
      terms: { PurchasePrice: 530000 },
    });
    if (!r.ok) throw new Error('setup failed');
    offerId = r.value.offerId;
    const started = await listen(
      createOfferServer({ store, hub, resolve: tokenResolver(new Map([['t', 'M00000136']])) })
    );
    base = started.url;
    stop = started.close;
  });

  const attempt = (method: string) =>
    fetch(`${base}/offers/${encodeURIComponent(offerId)}/payload`, {
      method,
      headers: { authorization: 'Bearer t', 'content-type': 'application/json' },
      ...(method === 'GET' || method === 'HEAD'
        ? {}
        : { body: JSON.stringify({ Submissions: [{ PurchasePrice: 1 }] }) }),
    });

  it.each(['POST', 'PUT', 'PATCH', 'DELETE'])('refuses %s against the expansion', async (m) => {
    const r = await attempt(m);
    expect(r.status).toBe(405);
    expect(r.headers.get('allow')).toBe('GET');
    await stop();
    store.close();
  });

  it('leaves the submissions untouched after every attempt', async () => {
    const before = JSON.stringify(store.submissionsFor(offerId));
    for (const m of ['POST', 'PUT', 'PATCH', 'DELETE']) await attempt(m);
    // A write that had reached the store would have added a submission at 545000
    // or replaced the one at 530000. Neither happened.
    expect(JSON.stringify(store.submissionsFor(offerId))).toBe(before);
    expect(store.submissionsFor(offerId)).toHaveLength(1);
    await stop();
    store.close();
  });
});
