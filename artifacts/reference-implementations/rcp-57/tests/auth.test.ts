import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import { createHub, type Hub } from '../src/hub.js';
import { createOfferServer, listen } from '../src/server.js';
import { sqliteStore } from '../src/store-sqlite.js';
import type { OfferStore } from '../src/store.js';
import { tokenResolver } from '../src/auth.js';

/**
 * Section 2.11, against a real HTTP server.
 *
 * These five checks could not be observed before there was a server: every one
 * of them is about a status code, a header or a body that only exists over the
 * wire. Asserting them against a function call would have tested a different
 * thing and reported it as coverage.
 *
 * The cast: `LISTING_*` hold the listing, `BUYER_*` submit an offer, `STRANGER`
 * is on the network and party to nothing, and `OTHER_PARTY` is party to a
 * *different* offer — the case Section 2.11 calls out, where involvement in one
 * offer must not become involvement in another.
 */

const LISTING_AGENT = 'M00000001';
const LISTING_BROKERAGE = 'M00000002';
const BUYER_AGENT = 'M00000136';
const BUYER_BROKERAGE = 'M00000137';
const STRANGER = 'M00000999';
const OTHER_PARTY = 'M00000500';

const TOKENS = new Map([
  ['tok-listing', LISTING_AGENT],
  ['tok-listing-brokerage', LISTING_BROKERAGE],
  ['tok-buyer', BUYER_AGENT],
  ['tok-buyer-brokerage', BUYER_BROKERAGE],
  ['tok-stranger', STRANGER],
  ['tok-other', OTHER_PARTY],
]);

const PG = { OfferPropertyGroupKey: 'PG-1', fields: { City: 'Chicago' } };

let store: OfferStore;
let hub: Hub;
let base: string;
let stop: () => Promise<void>;
let offerId: string;
/** A second offer, to which OTHER_PARTY is a party and our buyer is not. */
let otherOfferId: string;

const start = async (over: Partial<Parameters<typeof createOfferServer>[0]> = {}) => {
  const server = createOfferServer({ store, hub, resolve: tokenResolver(TOKENS), ...over });
  const started = await listen(server);
  base = started.url;
  stop = started.close;
};

beforeEach(async () => {
  store = sqliteStore();
  hub = createHub(store);
  hub.publishListing('11284417', [LISTING_AGENT, LISTING_BROKERAGE]);
  hub.publishListing('77777777', [OTHER_PARTY]);

  const a = hub.submitOffer({
    coordinate: { ListingId: '11284417', OfferOriginatingSystemName: 'MyMls' },
    buyerRef: 'dana',
    submittingUoi: BUYER_AGENT,
    alsoParties: [BUYER_BROKERAGE],
    propertyGroup: PG,
    terms: {
      PurchasePrice: 530000,
      OfferBuyerLegalName: 'Dana R. Whitfield',
      OfferCoBuyerLegalName: 'Sam T. Whitfield',
    },
  });
  const b = hub.submitOffer({
    coordinate: { ListingId: '77777777', OfferOriginatingSystemName: 'MyMls' },
    buyerRef: 'else',
    submittingUoi: OTHER_PARTY,
    propertyGroup: { OfferPropertyGroupKey: 'PG-2', fields: { City: 'Evanston' } },
    terms: { PurchasePrice: 1 },
  });
  if (!a.ok || !b.ok) throw new Error('setup failed');
  offerId = a.value.offerId;
  otherOfferId = b.value.offerId;
  await start();
});

afterEach(async () => {
  await stop();
  store.close();
});

const get = (path: string, token?: string, extra: Record<string, string> = {}) =>
  fetch(`${base}${path}`, {
    headers: { ...(token === undefined ? {} : { authorization: `Bearer ${token}` }), ...extra },
  });

const payloadPath = () => `/offers/${encodeURIComponent(offerId)}/payload`;

describe('payload-refuses-unauthenticated — a payload link refuses an unauthenticated dereference', () => {
  it('answers 401 with no Authorization header', async () => {
    const r = await get(payloadPath());
    expect(r.status).toBe(401);
  });

  it('challenges, so a client knows what to present', async () => {
    const r = await get(payloadPath());
    expect(r.headers.get('www-authenticate')).toMatch(/Bearer/);
  });

  it.each([
    ['an unknown token', 'tok-nonsense'],
    ['an empty token', ''],
    ['whitespace', '   '],
  ])('answers 401 for %s', async (_label, token) => {
    expect((await get(payloadPath(), token)).status).toBe(401);
  });

  it('serves no offer content in the refusal', async () => {
    const body = await (await get(payloadPath())).text();
    expect(body).not.toContain('530000');
    expect(body).not.toContain('Whitfield');
  });

  it('serves the payload to a party, so 401 is the guard and not the whole story', async () => {
    const r = await get(payloadPath(), 'tok-buyer');
    expect(r.status).toBe(200);
    const body = (await r.json()) as { '@reso.context': string; Submissions: unknown[] };
    // One offer-shaped document, with its turns expanded under `Submissions`.
    expect(body['@reso.context']).toBe('urn:reso:metadata:2.1:resource:offer');
    expect(body.Submissions).toHaveLength(1);
  });
});

describe('identity-from-token-only — the requester is determined from the token, never from the request', () => {
  it.each([
    ['a query parameter', `?uoi=${BUYER_AGENT}`, {}],
    ['a header naming a party', '', { 'x-uoi': BUYER_AGENT }],
    ['a forwarded-identity header', '', { 'x-forwarded-user': BUYER_AGENT }],
  ])('ignores %s when no token is presented', async (_label, query, headers) => {
    const r = await get(`${payloadPath()}${query}`, undefined, headers);
    expect(r.status).toBe(401);
  });

  it('ignores a request-carried identifier that contradicts the token', async () => {
    // The token is the stranger's; the request claims to be the buyer. The
    // stranger's answer must be what comes back.
    const r = await get(`${payloadPath()}?uoi=${BUYER_AGENT}`, 'tok-stranger', {
      'x-uoi': BUYER_AGENT,
    });
    expect(r.status).not.toBe(200);
  });

  it('resolves each token to its own identifier, so identity is not shared', async () => {
    expect((await get(payloadPath(), 'tok-buyer')).status).toBe(200);
    expect((await get(payloadPath(), 'tok-listing')).status).toBe(200);
    expect((await get(payloadPath(), 'tok-stranger')).status).not.toBe(200);
  });
});

describe('no-delegated-entitlement — another participant may not assert entitlement', () => {
  it.each([
    ['an entitlement header', { 'x-entitled': 'true' }],
    ['an issuer vouching for the requester', { 'x-issuer-asserts-party': BUYER_AGENT }],
    ['a scope claiming access to all offers', { 'x-scope': 'offers:read:all' }],
  ])('refuses a non-party presenting %s', async (_label, headers) => {
    const r = await get(payloadPath(), 'tok-stranger', headers);
    expect(r.status).not.toBe(200);
  });

  it('refuses a party to a different offer, however it is vouched for', async () => {
    // Authentication federates; authorization does not. OTHER_PARTY holds a
    // valid token and is genuinely a party — to another offer.
    const r = await get(payloadPath(), 'tok-other', { 'x-issuer-asserts-party': OTHER_PARTY });
    expect(r.status).not.toBe(200);
  });

  it('still serves that requester its own offer, so the refusal is scoped', async () => {
    const r = await get(`/offers/${encodeURIComponent(otherOfferId)}/payload`, 'tok-other');
    expect(r.status).toBe(200);
  });
});

describe('non-party-refused — offer content is refused to a requester outside the parties', () => {
  it('refuses a stranger', async () => {
    const r = await get(payloadPath(), 'tok-stranger');
    expect(r.status).not.toBe(200);
    expect(await r.text()).not.toContain('530000');
  });

  it('serves every party on both sides', async () => {
    for (const token of ['tok-buyer', 'tok-buyer-brokerage', 'tok-listing', 'tok-listing-brokerage']) {
      expect((await get(payloadPath(), token)).status).toBe(200);
    }
  });

  it('does not treat involvement in one offer as involvement in another', async () => {
    expect((await get(payloadPath(), 'tok-other')).status).not.toBe(200);
    expect((await get(`/offers/${encodeURIComponent(otherOfferId)}/payload`, 'tok-buyer')).status)
      .not.toBe(200);
  });

  it('answers a non-party the same way whether or not the offer exists', async () => {
    // Section 2.11's last paragraph: a refusal must not confirm existence to a
    // requester with no entitlement to know. Same status, same body, either way.
    const real = await get(payloadPath(), 'tok-stranger');
    const fake = await get('/offers/OFFER-DOES-NOT-EXIST/payload', 'tok-stranger');
    expect(real.status).toBe(fake.status);
    expect(await real.text()).toBe(await fake.text());
  });

  it('stays indistinguishable under a 403 configuration too', async () => {
    // The 404 default masks an ordering bug: deciding existence before
    // entitlement gives a non-party 404 for a missing offer and 403 for a real
    // one, which discloses exactly what the section forbids. Under the 404
    // default both answers are 404 and the bug is invisible, so the ordering has
    // to be observed here, where the two answers would differ.
    await stop();
    await start({ nonPartyAnswer: 403 });
    const real = await get(payloadPath(), 'tok-stranger');
    const fake = await get('/offers/OFFER-DOES-NOT-EXIST/payload', 'tok-stranger');
    expect(real.status).toBe(403);
    expect(fake.status).toBe(403);
    expect(await real.text()).toBe(await fake.text());
  });
});

describe('withheld-buyer-fields-not-an-error — absent buyer fields are not an error', () => {
  it('serves 200 with the buyer fields withheld from a party that does not need them', async () => {
    await stop();
    await start({ buyerFieldsFor: [BUYER_AGENT, BUYER_BROKERAGE] });
    const r = await get(payloadPath(), 'tok-listing');
    // Omission is not refusal: the requester is a party and the response succeeds.
    expect(r.status).toBe(200);
    const body = (await r.json()) as { Submissions: Record<string, unknown>[] };
    expect(body.Submissions[0].OfferBuyerLegalName).toBeUndefined();
    expect(body.Submissions[0].OfferCoBuyerLegalName).toBeUndefined();
    expect(body.Submissions[0].PurchasePrice).toBe(530000);
  });

  it('serves them to a party that does need them', async () => {
    await stop();
    await start({ buyerFieldsFor: [BUYER_AGENT, BUYER_BROKERAGE] });
    const r = await get(payloadPath(), 'tok-buyer');
    const body = (await r.json()) as { Submissions: Record<string, unknown>[] };
    expect(body.Submissions[0].OfferBuyerLegalName).toBe('Dana R. Whitfield');
  });
});

describe('refusal-does-not-disclose-existence — the refusal table of Section 2.11', () => {
  // Section 2.11 states these three answers, and no Section 3 check covers them.
  // Tested here anyway, because a stated rule nobody verifies is how a
  // specification acquires an unimplementable requirement. The gap is reported
  // in the certification log rather than quietly closed by this test.
  it('answers 401 for no token, and never 403 or 404', async () => {
    expect((await get(payloadPath())).status).toBe(401);
  });

  it('answers a non-party 403 or 404, never 200, and 404 by default', async () => {
    // Section 2.11 permits either, and requires the same one in both cases.
    expect((await get(payloadPath(), 'tok-stranger')).status).toBe(404);
    await stop();
    await start({ nonPartyAnswer: 403 });
    expect((await get(payloadPath(), 'tok-stranger')).status).toBe(403);
  });

  it('answers 404 for an offer that does not exist, by the not-a-party path', async () => {
    // Nobody is a party to an offer that does not exist, so this is refused as a
    // non-party rather than as a missing record. That is deliberate: the two
    // answering identically is what stops the refusal disclosing existence.
    const r = await get('/offers/NO-SUCH-OFFER/payload', 'tok-buyer');
    expect(r.status).toBe(404);
  });

  it('does not serve content on any refusal path', async () => {
    for (const token of [undefined, 'tok-nonsense', 'tok-stranger']) {
      const body = await (await get(payloadPath(), token)).text();
      expect(body).not.toContain('530000');
      expect(body).not.toContain('Whitfield');
    }
  });
});
