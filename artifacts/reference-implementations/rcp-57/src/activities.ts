/**
 * The RCP-57 activities, built with the standard Activity Streams vocabulary.
 *
 * The specification's hardest rule is negative: an activity carries identity and
 * transitions and **no offer content**, and nothing may extend ActivityPub's
 * JSON-LD with custom terms. So the test of this module is not that it works —
 * it is that the JSON it emits is indistinguishable from an ordinary
 * ActivityPub message, and that every element of the offer is somewhere else.
 *
 * Everything here builds vocabulary objects and leaves delivery alone. Who signs
 * what, and which inbox it reaches, is the federation layer's problem and is
 * deliberately not entangled with the shape of the message.
 */

import type { Temporal } from '@js-temporal/polyfill';
// The vocabulary lives on its own subpath; the package root exports the
// federation machinery instead.
import { Accept, Link, Note, Offer, Reject } from '@fedify/fedify/vocab';

/** Where an offer's actual terms live. Never in the activity. */
export interface PayloadRef {
  /** Absolute URL. MUST refuse an unauthenticated dereference. */
  readonly href: URL;
  /** Always RESO Common Format here. */
  readonly mediaType: string;
}

export interface ThreadRoot {
  /** The activity the listing's point of entry published. Offers reply to it. */
  readonly id: URL;
  /** The listing side, which offers are addressed to. */
  readonly listingActor: URL;
}

export interface OfferInput {
  /** Assigned by the server, never by the client. Opaque is permitted. */
  readonly id: URL;
  /** Who is making the offer. */
  readonly actor: URL;
  /** The activity this answers: the listing root, or a prior submission. */
  readonly inReplyTo: URL;
  /** Addressed to named parties, or to the public collection. */
  readonly to: readonly URL[];
  /** Systems acting on a party's behalf. */
  readonly attributedTo?: readonly URL[];
  readonly published: Temporal.Instant;
  readonly payload: PayloadRef;
}

/**
 * An offer, or a counter, which is the same activity replying to a different
 * parent. The specification is explicit that a counter is not a new kind of
 * record and not an edit.
 */
export const buildOffer = (input: OfferInput): Offer =>
  new Offer({
    id: input.id,
    actors: [input.actor],
    // Fedify names Activity Streams `inReplyTo` as `replyTarget`. The wire form
    // is the standard term; only the accessor differs.
    replyTarget: input.inReplyTo,
    tos: [...input.to],
    attributions: [...(input.attributedTo ?? [])],
    published: input.published,
    urls: [new Link({ href: input.payload.href, mediaType: input.payload.mediaType })],
  });

/**
 * A state with no Activity Streams verb of its own travels as a Note, which is
 * what the specification says to do rather than inventing a term for it.
 */
export const buildStateNote = (input: {
  readonly id: URL;
  readonly actor: URL;
  readonly inReplyTo: URL;
  readonly to: readonly URL[];
  readonly published: Temporal.Instant;
  readonly content: string;
}): Note =>
  new Note({
    id: input.id,
    attributions: [input.actor],
    replyTarget: input.inReplyTo,
    tos: [...input.to],
    published: input.published,
    content: input.content,
  });

/** Acceptance names the submission it accepts, which in a countered negotiation
 *  is the most recent counter rather than the original offer. */
export const buildAccept = (input: {
  readonly id: URL;
  readonly actor: URL;
  readonly offerId: URL;
  readonly to: readonly URL[];
  readonly published: Temporal.Instant;
}): Accept =>
  new Accept({
    id: input.id,
    actors: [input.actor],
    object: input.offerId,
    tos: [...input.to],
    published: input.published,
  });

export const buildReject = (input: {
  readonly id: URL;
  readonly actor: URL;
  readonly offerId: URL;
  readonly to: readonly URL[];
  readonly published: Temporal.Instant;
}): Reject =>
  new Reject({
    id: input.id,
    actors: [input.actor],
    object: input.offerId,
    tos: [...input.to],
    published: input.published,
  });
