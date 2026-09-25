import { Temporal } from '@js-temporal/polyfill';
import { describe, expect, it } from 'vitest';
import { buildAccept, buildOffer, buildReject, buildStateNote } from '../src/activities.js';

/**
 * Section 2.2: an implementation MUST NOT extend ActivityPub's JSON-LD with
 * custom terms.
 *
 * The test has to read the **expanded** form. Fedify declares four contexts on
 * every activity, one of them a vendor namespace, so a conformance test that
 * reads the literal `@context` rejects an implementation whose data is pure
 * Activity Streams. Expanded, every property resolves or it does not, and that
 * is the question the rule actually asks.
 *
 * This began as a scratch script whose output nobody checked. It is a test now,
 * because the finding it carries is the one a certification runner most needs.
 */

const AS = 'https://www.w3.org/ns/activitystreams#';
const at = Temporal.Instant.from('2026-09-15T14:02:00Z');
const u = (s: string) => new URL(s);

const ACTOR = u('https://agent.example/amy');
const COUNTERPARTY = u('https://agent.example/bob');
const OFFER = u('https://offers.example/1');

const activities = {
  Offer: buildOffer({
    id: OFFER,
    actor: ACTOR,
    inReplyTo: u('https://listings.example/133'),
    to: [COUNTERPARTY],
    attributedTo: [u('https://agent.example/app')],
    published: at,
    payload: { href: u('https://offers.example/p/1'), mediaType: 'application/json' },
  }),
  Accept: buildAccept({
    id: u('https://offers.example/2'), actor: COUNTERPARTY,
    offerId: OFFER, to: [ACTOR], published: at,
  }),
  Reject: buildReject({
    id: u('https://offers.example/3'), actor: COUNTERPARTY,
    offerId: OFFER, to: [ACTOR], published: at,
  }),
  Note: buildStateNote({
    id: u('https://offers.example/4'), actor: COUNTERPARTY,
    inReplyTo: OFFER, to: [ACTOR], published: at, content: 'Acknowledged',
  }),
};

const expandedKeys = async (activity: { toJsonLd: (o: object) => Promise<unknown> }) => {
  const expanded = (await activity.toJsonLd({ format: 'expand' })) as Record<string, unknown>[];
  return Object.keys(expanded[0] ?? {});
};

describe('every activity uses only Activity Streams vocabulary', () => {
  it.each(Object.keys(activities))('%s carries no foreign property', async (name) => {
    const keys = await expandedKeys(activities[name as keyof typeof activities]);
    expect(keys.length).toBeGreaterThan(0);
    const foreign = keys.filter((k) => !k.startsWith('@') && !k.startsWith(AS));
    expect(foreign).toEqual([]);
  });

  it('would reject a foreign term, so the check is not vacuous', async () => {
    const keys = await expandedKeys(activities.Offer);
    const withForeign = [...keys, 'https://gotosocial.org/ns#sensitive'];
    expect(withForeign.filter((k) => !k.startsWith('@') && !k.startsWith(AS))).toHaveLength(1);
  });

  it('declares more contexts than it uses, which is why the literal test is wrong', async () => {
    const compact = (await activities.Offer.toJsonLd({})) as { '@context': unknown };
    const declared = Array.isArray(compact['@context'])
      ? compact['@context']
      : [compact['@context']];
    // The vendor namespace is present in the declared context and unused in the
    // data. A certification check reading this value would fail a conformant
    // implementation built on the most widely used library.
    expect(declared.length).toBeGreaterThan(1);
  });
});
