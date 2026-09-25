import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import { tokenResolver } from '../src/auth.js';
import { createHub, type Hub } from '../src/hub.js';
import { createOfferServer, listen } from '../src/server.js';
import { sqliteStore } from '../src/store-sqlite.js';
import type { OfferStore } from '../src/store.js';

/**
 * Regressions for four confirmed defects in the payload server.
 *
 * Every one of them survived 122 passing tests, because those tests exercised
 * the operations a well-behaved party performs. These exercise what a party that
 * is not well-behaved can reach. The suite had no such tests before, which is
 * the reason all four got as far as a pre-publish audit.
 */

const LISTING_ORG = 'M00000001';
const SUBMITTER = 'M00000136';
const OTHER_ORG = 'M00000500';

const TOKENS = new Map([
  ['tok-list', LISTING_ORG],
  ['tok-sub', SUBMITTER],
  ['tok-other', OTHER_ORG],
]);

let store: OfferStore;
let hub: Hub;
let base: string;
let stop: () => Promise<void>;

const start = async (over: Record<string, unknown> = {}) => {
  const started = await listen(
    createOfferServer({ store, hub, resolve: tokenResolver(TOKENS), ...over })
  );
  base = started.url;
  stop = started.close;
};

const get = (path: string, token?: string) =>
  fetch(`${base}${path}`, {
    headers: token === undefined ? {} : { authorization: `Bearer ${token}` },
  });

beforeEach(() => {
  store = sqliteStore();
  hub = createHub(store);
  hub.publishListing('L1', [LISTING_ORG]);
  hub.publishListing('L2', [OTHER_ORG]);
});

afterEach(async () => {
  if (stop !== undefined) await stop();
  store.close();
});

const anOffer = (terms: Record<string, unknown>, group: Record<string, unknown> = {}) => {
  const r = hub.submitOffer({
    coordinate: { ListingId: 'L1', OfferOriginatingSystemName: 'MyMls' },
    buyerRef: 'dana',
    submittingUoi: SUBMITTER,
    propertyGroup: { OfferPropertyGroupKey: 'PG-1', fields: { City: 'Chicago', ...group } },
    terms: { PurchasePrice: 500000, ...terms },
  });
  if (!r.ok) throw new Error(`setup failed: ${r.reason}`);
  return r.value.offerId;
};

describe('a malformed identifier cannot take the server down', () => {
  it('answers 400 rather than throwing, with no credentials presented', async () => {
    await start();
    // decodeURIComponent throws on a malformed escape, synchronously inside the
    // request handler. Unguarded it reached the process as an uncaught exception,
    // so an unauthenticated stranger could stop the server with one request.
    const r = await get('/offers/%zz/payload');
    expect(r.status).toBe(400);
  });

  it('is still serving afterwards', async () => {
    await start();
    const id = anOffer({});
    await get('/offers/%zz/payload');
    await get('/offers/%e0%a4%a/payload');
    const r = await get(`/offers/${encodeURIComponent(id)}/payload`, 'tok-sub');
    expect(r.status).toBe(200);
  });

  it('refuses before authenticating, so the guard cannot be reached around', async () => {
    await start();
    expect((await get('/offers/%zz/payload', 'tok-other')).status).toBe(400);
  });
});

describe('a submitting party cannot state what the server states', () => {
  it('does not let terms forge the status, the id or the sequence', async () => {
    await start();
    const id = anOffer({
      OfferSubmissionStatus: 'Accepted',
      OfferReceivedStatus: 'Accepted',
      OfferId: 'OFFER-FORGED',
      OfferSubmissionSequence: 999,
      OfferSubmissionKey: 'FORGED-KEY',
    });
    const body = (await (await get(`/offers/${id}/payload`, 'tok-list')).json()) as {
      Submissions: Record<string, unknown>[];
    };
    const sub = body.Submissions[0];
    // The server's word, not the submitter's.
    expect(sub.OfferSubmissionStatus).toBe('Submitted');
    expect(sub.OfferReceivedStatus).toBeUndefined();
    expect(sub.OfferId).toBe(id);
    expect(sub.OfferSubmissionSequence).toBe(1);
    expect(sub.OfferSubmissionKey).not.toBe('FORGED-KEY');
  });

  it('does not let terms inject a nested expansion', async () => {
    await start();
    const id = anOffer({ Submissions: [{ PurchasePrice: 1 }], PropertyGroup: { City: 'Nowhere' } });
    const body = (await (await get(`/offers/${id}/payload`, 'tok-list')).json()) as {
      Submissions: Record<string, unknown>[];
    };
    expect(body.Submissions).toHaveLength(1);
    expect((body.Submissions[0].PropertyGroup as Record<string, unknown>).City).toBe('Chicago');
  });

  it('keeps ordinary terms, so the filter is not simply dropping everything', async () => {
    await start();
    const id = anOffer({ EarnestMoney: 15000, Contingency: 'Inspection' });
    const body = (await (await get(`/offers/${id}/payload`, 'tok-list')).json()) as {
      Submissions: Record<string, unknown>[];
    };
    expect(body.Submissions[0].PurchasePrice).toBe(500000);
    expect(body.Submissions[0].EarnestMoney).toBe(15000);
    expect(body.Submissions[0].Contingency).toBe('Inspection');
  });

  it('drops keys that would reach Object.prototype', async () => {
    await start();
    const id = anOffer(JSON.parse('{"__proto__":{"polluted":"yes"},"constructor":{"x":1}}'));
    const raw = await (await get(`/offers/${id}/payload`, 'tok-list')).text();
    expect(raw).not.toContain('polluted');
    expect(raw).not.toContain('__proto__');
  });
});

describe('one organization cannot overwrite another organization', () => {
  it('keeps each offer property group to its own offer', async () => {
    await start();
    const victim = anOffer({}, { StreetName: 'Victim Lane' });

    // A different org, party to nothing of the first, submits its own offer on
    // its own listing choosing the same property group key.
    const attacker = hub.submitOffer({
      coordinate: { ListingId: 'L2', OfferOriginatingSystemName: 'MyMls' },
      buyerRef: 'mallory',
      submittingUoi: OTHER_ORG,
      propertyGroup: { OfferPropertyGroupKey: 'PG-1', fields: { StreetName: 'Attacker Way' } },
      terms: { PurchasePrice: 1 },
    });
    expect(attacker.ok).toBe(true);

    const body = (await (await get(`/offers/${victim}/payload`, 'tok-list')).json()) as {
      Submissions: Record<string, unknown>[];
    };
    const group = body.Submissions[0].PropertyGroup as Record<string, unknown>;
    expect(group.StreetName).toBe('Victim Lane');
  });

  it('resolves a property group only within its own offer', () => {
    const victim = anOffer({}, { StreetName: 'Victim Lane' });
    expect(store.getPropertyGroup(victim, 'PG-1')?.fields.StreetName).toBe('Victim Lane');
    expect(store.getPropertyGroup('OFFER-DOES-NOT-EXIST', 'PG-1')).toBeUndefined();
  });
});

describe('withholding covers every place the personal data appears', () => {
  const BUYER = { OfferBuyerLegalName: 'Dana R. Whitfield', OfferCoBuyerPhone: '+1-312-555-0147' };

  it('withholds buyer fields carried inside the property group', async () => {
    await start({ buyerFieldsFor: [SUBMITTER] });
    // The same personal data, one level down. Withholding that covered only
    // terms served it here in full to a party meant not to see it.
    const id = anOffer(BUYER, BUYER);
    const raw = await (await get(`/offers/${id}/payload`, 'tok-list')).text();
    expect(raw).not.toContain('Whitfield');
    expect(raw).not.toContain('555-0147');
  });

  it('serves them to the party that is entitled, in both places', async () => {
    await start({ buyerFieldsFor: [SUBMITTER] });
    const id = anOffer(BUYER, BUYER);
    const raw = await (await get(`/offers/${id}/payload`, 'tok-sub')).text();
    expect(raw).toContain('Whitfield');
  });

  it('withholds from everyone when no allowlist is configured', async () => {
    await start();
    // The most sensitive elements the specification defines default to withheld,
    // so an operator who never configured this cannot disclose them by omission.
    const id = anOffer(BUYER);
    const raw = await (await get(`/offers/${id}/payload`, 'tok-sub')).text();
    expect(raw).not.toContain('Whitfield');
  });
});

describe('a private payload is not cacheable', () => {
  it('forbids storing it anywhere', async () => {
    await start();
    const id = anOffer({});
    const r = await get(`/offers/${id}/payload`, 'tok-sub');
    expect(r.headers.get('cache-control')).toBe('no-store');
    expect(r.headers.get('x-content-type-options')).toBe('nosniff');
  });
});
