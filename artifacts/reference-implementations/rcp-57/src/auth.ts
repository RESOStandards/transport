/**
 * Authentication and authorization, as two separate decisions.
 *
 * Section 2.11 is emphatic that these are independent, and that the second one
 * never federates: who a requester *is* may be established by another
 * participant, but whether they may read *this* offer is held only by the
 * implementation holding it.
 *
 * **The signatures are the enforcement.** Two rules in Section 2.11 are the kind
 * that a correct implementation obeys and a careless one breaks without noticing,
 * so neither function is given the means to break them:
 *
 * - `resolveRequester` takes a token and nothing else. There is no parameter for
 *   a value carried in the request, so an identifier cannot be inferred from one.
 * - `isPartyTo` takes the resolved identifier and the store. There is no
 *   parameter for an asserted entitlement, so another participant's claim cannot
 *   be accepted in place of the implementation's own determination.
 *
 * A reviewer can see both rules hold by reading the two type signatures. That is
 * a stronger guarantee than a comment saying not to do it.
 */

import type { OfferStore } from './store.js';

/** A resolved requester: an organization or a system identifier, never a name. */
export interface Requester {
  /** A Unique Organization Identifier or Unique System Identifier. */
  readonly identifier: string;
}

/**
 * Resolves a bearer token to a requester.
 *
 * Deliberately narrow: a token, and nothing else. An implementation backed by
 * OpenID Connect substitutes its own resolver here without widening this.
 */
export type ResolveRequester = (token: string | undefined) => Requester | undefined;

/** A token registry, for tests and the playground. Production swaps in OIDC. */
export const tokenResolver = (tokens: ReadonlyMap<string, string>): ResolveRequester => {
  return (token) => {
    if (token === undefined || token.trim() === '') return undefined;
    const identifier = tokens.get(token);
    return identifier === undefined ? undefined : { identifier };
  };
};

/** Extract a bearer token from an `Authorization` header. */
export const bearerOf = (header: string | undefined): string | undefined => {
  if (header === undefined) return undefined;
  const m = /^Bearer\s+(.+)$/i.exec(header.trim());
  return m === null ? undefined : m[1].trim();
};

/**
 * Whether a requester is a party to this offer, on the implementation's own
 * record. Takes no assertion of entitlement, because none may be accepted.
 */
export const isPartyTo = (store: OfferStore, offerId: string, requester: Requester): boolean =>
  store.partiesOf(offerId).some((p) => p.identifier === requester.identifier);

/** The outcome of deciding one payload request. */
export type Decision =
  | { readonly kind: 'serve'; readonly requester: Requester }
  /** No token, or a token that does not authenticate. */
  | { readonly kind: 'unauthenticated' }
  /** Authenticated, but not a party to this offer. */
  | { readonly kind: 'not-a-party'; readonly requester: Requester };

/**
 * Decide one payload request.
 *
 * Order matters and is the order Section 2.11 states: authenticate, then decide
 * entitlement, then existence. Checking existence before entitlement would leak
 * whether an offer exists to a requester with no standing to know — the leak the
 * section's last paragraph is about.
 */
export const decide = (
  store: OfferStore,
  resolve: ResolveRequester,
  offerId: string,
  token: string | undefined
): Decision => {
  const requester = resolve(token);
  if (requester === undefined) return { kind: 'unauthenticated' };
  // Entitlement before existence. Section 2.11's third row, "authenticated and a
  // party, but no such offer", has no reachable case here: parties are recorded
  // per offer, so an offer that does not exist has no parties and nobody can be
  // a party to it. Both conditions therefore answer the same way, which is also
  // what stops the answer disclosing whether the offer exists.
  if (!isPartyTo(store, offerId, requester)) return { kind: 'not-a-party', requester };
  return { kind: 'serve', requester };
};

/**
 * How a refusal is answered.
 *
 * Section 2.11 states `403` for a non-party, then permits `404` where `403`
 * would itself disclose that the offer exists. Both are conformant, so this is a
 * setting rather than a constant, and the checks accept either.
 *
 * The default is `404`. A non-party learning that an offer exists is a
 * disclosure, and the specification names `404` as the safer answer; taking the
 * safer branch by default and requiring a deliberate opt-in to the other is the
 * way round that cannot leak by omission.
 */
export type NonPartyAnswer = 403 | 404;

export const statusFor = (d: Decision, nonParty: NonPartyAnswer = 404): number => {
  switch (d.kind) {
    case 'serve':
      return 200;
    case 'unauthenticated':
      return 401;
    case 'not-a-party':
      return nonParty;
  }
};
