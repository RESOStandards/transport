/**
 * The payload server.
 *
 * Section 2.2 keeps offer content out of activities: an activity carries
 * identity and transitions, and references the payload by a protected link. This
 * serves those links, and it is the only place offer content leaves the store.
 *
 * Built on `node:http` rather than a framework. The whole surface is three
 * routes, the certification checks are about status codes and headers, and a
 * framework would add a dependency whose own behavior would then need
 * distinguishing from the specification's.
 *
 * What it refuses is as much the point as what it serves. Every refusal path in
 * Section 2.11 is reachable here, which is what makes those five checks
 * observable at all — until there was a server, none of them could be tested.
 */

import { createServer, type IncomingMessage, type Server, type ServerResponse } from 'node:http';
import {
  bearerOf,
  decide,
  type NonPartyAnswer,
  type ResolveRequester,
  statusFor,
} from './auth.js';
import { OFFER_CONTEXT } from './constants.js';
import type { Hub } from './hub.js';
import type { OfferStore } from './store.js';

export interface ServerConfig {
  readonly store: OfferStore;
  readonly hub: Hub;
  readonly resolve: ResolveRequester;
  /** `404` by default: the answer that cannot disclose an offer's existence. */
  readonly nonPartyAnswer?: NonPartyAnswer;
  /**
   * Identifiers permitted to see buyer and co-buyer fields. Section 2.11 allows
   * withholding them from a party that does not require them, and requires a
   * consumer not to treat their absence as an error.
   *
   * **Omitting this withholds from everyone.** These are the most sensitive
   * elements the specification defines, so the default is the one that cannot
   * disclose by omission. Defaulting the other way meant an operator who never
   * configured it served buyer legal name, address and telephone to every party,
   * which is the opposite of how the status default is argued in `auth.ts`.
   */
  readonly buyerFieldsFor?: readonly string[];
}

/**
 * The buyer and co-buyer fields of Section 2.5, which Section 2.11 names as the
 * most sensitive elements the specification defines.
 *
 * Spelled out rather than pattern-matched, and checked against the generated
 * model in `model.test.ts`. The first version of this listed `BuyerName` and
 * `CoBuyerName`, which the specification does not define, so the withholding
 * path was hiding fields that never existed and the tests agreed with it because
 * they used the same invented names.
 */
const BUYER_FIELDS = [
  'OfferBuyerLegalName',
  'OfferBuyerLegalAddress',
  'OfferBuyerPhone',
  'OfferCoBuyerLegalName',
  'OfferCoBuyerLegalAddress',
  'OfferCoBuyerPhone',
] as const;

export const buyerFieldNames: readonly string[] = BUYER_FIELDS;

/**
 * Members the server states about a submission, which a submitting party MUST
 * NOT be able to state about itself.
 *
 * Terms are free-form by design: the Data Dictionary owns what may appear there,
 * and this package does not police it. That freedom is exactly why these keys
 * have to be removed. A party that can put `OfferSubmissionStatus: "Accepted"`
 * in its own terms could otherwise tell the counterparty its offer was accepted.
 */
const SERVER_STATED = new Set([
  'OfferSubmissionKey',
  'OfferId',
  'OfferPropertyGroupKey',
  'OfferSubmissionSequence',
  'OfferSubmissionStatus',
  'OfferReceivedStatus',
  'PropertyGroup',
  'Submissions',
  '@reso.context',
]);

/** Keys that reach `Object.prototype` if an object is built from input. */
const UNSAFE_KEY = new Set(['__proto__', 'constructor', 'prototype']);

/**
 * Terms as they may be served: buyer fields withheld where required, members the
 * server states removed, and prototype-reaching keys dropped.
 *
 * Filtering on the way out rather than on the way in, because the store holds
 * what a party actually sent and that is the evidence. What is served is a
 * projection of it.
 */
const serveableTerms = (
  terms: Readonly<Record<string, unknown>>,
  includeBuyerFields: boolean
): Record<string, unknown> =>
  Object.fromEntries(
    Object.entries(terms).filter(
      ([k]) =>
        !SERVER_STATED.has(k) &&
        !UNSAFE_KEY.has(k) &&
        (includeBuyerFields || !BUYER_FIELDS.some((f) => f === k))
    )
  );

const send = (res: ServerResponse, status: number, body?: unknown): void => {
  if (body === undefined) {
    res.writeHead(status);
    res.end();
    return;
  }
  const json = JSON.stringify(body);
  res.writeHead(status, {
    'content-type': 'application/json; charset=utf-8',
    'content-length': String(Buffer.byteLength(json)),
    // Offer payloads are private and the link is long-lived, so nothing may
    // store them: not a shared proxy applying heuristic freshness, not a browser
    // disk cache if the URL is ever opened in one.
    'cache-control': 'no-store',
    'x-content-type-options': 'nosniff',
    'referrer-policy': 'no-referrer',
  });
  res.end(json);
};

/**
 * The payload for one offer, as RESO Common Format.
 *
 * `@reso.context` names the resource so a consumer validates it against the Data
 * Dictionary rather than against a shape invented here.
 */
const payloadOf = (
  store: OfferStore,
  offerId: string,
  includeBuyerFields: boolean
): Record<string, unknown> => {
  const offer = store.getOffer(offerId);
  return {
    '@reso.context': OFFER_CONTEXT,
    OfferKey: offer?.OfferKey,
    OfferId: offerId,
    ...(offer?.ListingId === undefined ? {} : { ListingId: offer.ListingId }),
    ...(offer?.ListingKey === undefined ? {} : { ListingKey: offer.ListingKey }),
    ...(offer?.OfferUoi === undefined ? {} : { OfferUoi: offer.OfferUoi }),
    ...(offer?.OfferOriginatingSystemName === undefined
      ? {}
      : { OfferOriginatingSystemName: offer.OfferOriginatingSystemName }),
    // `Submissions`, not `value`: this is the expansion of the Offer to
    // OfferSubmission relationship, named by dropping the prefix the two resource
    // names share, as `Property` carries `Rooms` for `PropertyRooms`. A bare
    // `value` array would have made the document an OData collection response
    // rather than an offer with its turns expanded into it.
    Submissions: store.submissionsFor(offerId).map((s) => {
      const group = store.getPropertyGroup(offerId, s.OfferPropertyGroupKey);
      // Terms first, then the members the server states, so a party's own terms
      // can never overwrite them. Spreading terms last was the defect: a
      // submitter could set OfferSubmissionStatus to "Accepted" in its terms and
      // have the counterparty read it as the server's word.
      return {
        ...serveableTerms(s.terms, includeBuyerFields),
        OfferSubmissionKey: s.OfferSubmissionKey,
        OfferId: s.OfferId,
        OfferPropertyGroupKey: s.OfferPropertyGroupKey,
        OfferSubmissionSequence: s.OfferSubmissionSequence,
        ...(s.OfferSubmissionStatus === undefined
          ? {}
          : { OfferSubmissionStatus: s.OfferSubmissionStatus }),
        ...(s.OfferReceivedStatus === undefined
          ? {}
          : { OfferReceivedStatus: s.OfferReceivedStatus }),
        // Inline under `PropertyGroup`, by the same naming rule. Carried inline
        // because the receiving system may hold no listing record for the
        // property, so the address has to travel with the offer. Filtered by the
        // same rule as terms: buyer fields hide here too, and withholding that
        // covered only terms left the same personal data reachable one level down.
        ...(group === undefined
          ? {}
          : { PropertyGroup: serveableTerms(group.fields, includeBuyerFields) }),
      };
    }),
  };
};

/** Decode one path segment, or report that it cannot be decoded. */
const decodeOfferId = (raw: string): string | undefined => {
  try {
    return decodeURIComponent(raw);
  } catch {
    return undefined;
  }
};

/** `/offers/{offerId}/payload` — the only route that serves offer content. */
const OFFER_PAYLOAD = /^\/offers\/([^/]+)\/payload\/?$/;

export const createOfferServer = (config: ServerConfig): Server => {
  const { store, resolve } = config;
  const nonParty = config.nonPartyAnswer ?? 404;

  return createServer((req: IncomingMessage, res: ServerResponse) => {
    const url = new URL(req.url ?? '/', 'http://localhost');
    const match = OFFER_PAYLOAD.exec(url.pathname);

    if (match === null) return send(res, 404, { error: 'not found' });
    // Section 2.5: the expansion is a projection for reading. No method here
    // creates, alters or removes a submission, and the refusal is explicit rather
    // than incidental so a certification run can observe it.
    if (req.method !== 'GET') {
      res.setHeader('allow', 'GET');
      return send(res, 405, { error: 'method not allowed' });
    }

    // A malformed percent-escape makes decodeURIComponent throw, and the throw is
    // synchronous inside the request handler, so it reached the process as an
    // uncaught exception. `GET /offers/%zz/payload` with no credentials at all
    // took the server down before any authentication ran.
    const offerId = decodeOfferId(match[1]);
    if (offerId === undefined) return send(res, 400, { error: 'malformed offer identifier' });

    // The token is the only input to identity. Note what is NOT read here: no
    // query parameter, no body field, no `X-`header naming a party. Section 2.11
    // forbids inferring the requester from anything the request carries, and the
    // way to honour that is to never look.
    const decision = decide(store, resolve, offerId, bearerOf(req.headers.authorization));

    if (decision.kind === 'unauthenticated') {
      // A `WWW-Authenticate` challenge is what makes 401 actionable rather than
      // merely a refusal, and it is what a client-credentials flow looks for.
      res.setHeader('www-authenticate', 'Bearer realm="offers"');
      return send(res, 401, { error: 'unauthenticated' });
    }
    if (decision.kind !== 'serve') {
      return send(res, statusFor(decision, nonParty), { error: 'not found' });
    }

    const includeBuyerFields =
      config.buyerFieldsFor?.includes(decision.requester.identifier) ?? false;
    return send(res, 200, payloadOf(store, offerId, includeBuyerFields));
  });
};

/** Start on an ephemeral port and report the base URL. Used by the suite. */
export const listen = (server: Server): Promise<{ url: string; close: () => Promise<void> }> =>
  new Promise((resolve) => {
    server.listen(0, '127.0.0.1', () => {
      const addr = server.address();
      const port = typeof addr === 'object' && addr !== null ? addr.port : 0;
      resolve({
        url: `http://127.0.0.1:${port}`,
        close: () => new Promise<void>((done) => server.close(() => done())),
      });
    });
  });
