/**
 * The offer hub: the operations a server performs, with the specification's
 * rules enforced in one place.
 *
 * Every operation returns a result rather than throwing, because a refusal is a
 * normal outcome here and its reason is the useful part. A certification runner
 * reads the reason; a UI shows it; neither wants a stack trace.
 *
 * The rules that live here rather than in the store are the ones about
 * *sequence of events*: what may follow what. The store enforces shape — a
 * submission cannot be edited, a property group must resolve — and this enforces
 * order: an ended negotiation takes no more turns, an act with no terms writes a
 * status rather than appending, and each side writes only its own field.
 */

import {
  type Coordinate,
  validateCoordinate,
} from './coordinate.js';
import {
  type OfferRecord,
  type OfferStore,
  type PropertyGroupRecord,
  type Side,
  type SubmissionRecord,
} from './store.js';

export type Outcome<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly reason: string; readonly code: Refusal };

/** Why an operation was refused. Distinct codes, because callers branch on them. */
export type Refusal =
  | 'listing-not-published'
  | 'invalid-coordinate'
  | 'offer-not-found'
  | 'offer-ended'
  | 'property-group-unresolved'
  | 'not-a-party';

const no = <T>(code: Refusal, reason: string): Outcome<T> => ({ ok: false, code, reason });
const yes = <T>(value: T): Outcome<T> => ({ ok: true, value });

export interface Terms {
  readonly PurchasePrice: number;
  readonly [field: string]: unknown;
}

export interface SubmitInput {
  readonly coordinate: Coordinate;
  readonly buyerRef: string;
  readonly submittingUoi: string;
  readonly propertyGroup: PropertyGroupRecord;
  readonly terms: Terms;
  /** The submitting agent's brokerage, and any system acting for them. The
   *  submitting organization is a party whether or not this is given. */
  readonly alsoParties?: readonly string[];
}

export interface Hub {
  /**
   * Opening a listing for offers. Until this, no offer can reference it.
   *
   * `by` is the listing side: the listing agent, their brokerage and any system
   * acting for them. They become parties to every offer on the listing, which is
   * how the receiving side is entitled to read payloads without any participant
   * asserting that entitlement.
   */
  publishListing: (listingRef: string, by: readonly string[]) => void;
  isPublished: (listingRef: string) => boolean;
  /** The parties to one offer, as this hub recorded them. */
  partiesOf: (offerId: string) => readonly string[];

  submitOffer: (input: SubmitInput) => Outcome<{ offerId: string; sequence: number }>;
  counter: (
    offerId: string,
    by: { submittingUoi: string; terms: Terms }
  ) => Outcome<{ sequence: number }>;
  /** An act that changes no terms: writes one side's status, appends nothing. */
  recordAct: (
    offerId: string,
    side: Side,
    status: string
  ) => Outcome<{ sequence: number }>;
  /** Acceptance, rejection and withdrawal end the negotiation. */
  conclude: (
    offerId: string,
    side: Side,
    as: NonNullable<OfferRecord['EndedAs']>
  ) => Outcome<{ endedAs: string }>;

  submissions: (offerId: string) => readonly SubmissionRecord[];
}

export const createHub = (store: OfferStore): Hub => {
  // Scoped to this hub rather than to the module. As a module global the count
  // was shared by every hub in the process, so ids in one test depended on how
  // many hubs an earlier test had built.
  let issued = 0;
  const nextId = (prefix: string): string => {
    issued += 1;
    return `${prefix}-${String(issued).padStart(6, '0')}`;
  };

  /** Listing reference to its listing-side parties. */
  const published = new Map<string, readonly string[]>();

  /** An offer that has ended takes no further turns, of any kind. */
  const live = (offerId: string): Outcome<OfferRecord> => {
    const offer = store.getOffer(offerId);
    if (offer === undefined) return no('offer-not-found', `no offer ${offerId}`);
    if (offer.EndedAs !== undefined) {
      return no(
        'offer-ended',
        `offer ${offerId} was ${offer.EndedAs.toLowerCase()}. An ended negotiation does not reopen; a later offer from the same buyer is a new Offer.`
      );
    }
    return yes(offer);
  };

  return {
    publishListing: (listingRef, by) => {
      published.set(listingRef, by);
    },
    isPublished: (listingRef) => published.has(listingRef),
    partiesOf: (offerId) => store.partiesOf(offerId).map((p) => p.identifier),

    submitOffer: (input) => {
      // Either identifier may match. A hub publishes a listing knowing both, and
      // an offer need carry only one, so insisting on a single preferred member
      // refuses offers that name the listing perfectly well by the other. That
      // was a live false refusal: matching on ListingKey first rejected an offer
      // carrying both when the listing had been published by ListingId.
      const candidates = [input.coordinate.ListingKey, input.coordinate.ListingId].filter(
        (r): r is string => r !== undefined && r.trim() !== ''
      );
      const listingRef = candidates.find((r) => published.has(r));
      if (listingRef === undefined) {
        return no(
          'listing-not-published',
          `listing ${candidates.join(' / ') || '(unnamed)'} has not been published for offers. Publishing is what opens a listing, not its marketing status.`
        );
      }
      const coord = validateCoordinate(input.coordinate);
      if (!coord.ok) return no('invalid-coordinate', coord.reason);

      // The offer is created first, because a property group belongs to one and
      // cannot be stored without it. Storing groups in a space shared across
      // offers let one organization overwrite another's address by choosing the
      // same key.
      const offerId = nextId('OFFER');
      store.putOffer({
        OfferKey: nextId('OK'),
        OfferId: offerId,
        ...input.coordinate,
        BuyerRef: input.buyerRef,
        ListingRef: listingRef,
      });
      store.putPropertyGroup(offerId, input.propertyGroup);
      store.appendSubmission({
        OfferSubmissionKey: nextId('SUB'),
        OfferId: offerId,
        OfferPropertyGroupKey: input.propertyGroup.OfferPropertyGroupKey,
        OfferSubmissionSequence: 1,
        SubmittingUoi: input.submittingUoi,
        OfferSubmissionStatus: 'Submitted',
        terms: input.terms,
      });

      // Parties are recorded once, per offer, at creation: the submitting side
      // from the offer, the receiving side from whoever published the listing.
      // Recorded here rather than derived at read time so a later change to a
      // listing's representatives cannot retroactively grant access to an offer
      // that was negotiated before them.
      for (const identifier of [input.submittingUoi, ...(input.alsoParties ?? [])]) {
        store.addParty({ OfferId: offerId, identifier, role: 'submitting' });
      }
      for (const identifier of published.get(listingRef) ?? []) {
        store.addParty({ OfferId: offerId, identifier, role: 'receiving' });
      }
      return yes({ offerId, sequence: 1 });
    },

    counter: (offerId, by) => {
      const l = live(offerId);
      if (!l.ok) return l;
      const prior = store.submissionsFor(offerId);
      if (prior.length === 0) {
        return no('offer-not-found', `offer ${offerId} has no submission to counter`);
      }
      // One greater than the highest SEEN, never the count: two submissions may
      // share a sequence when both parties acted unaware of the other, and
      // counting would reissue a number already in use.
      const sequence = store.highestSequence(offerId) + 1;
      store.appendSubmission({
        OfferSubmissionKey: nextId('SUB'),
        OfferId: offerId,
        OfferPropertyGroupKey: prior[0].OfferPropertyGroupKey,
        OfferSubmissionSequence: sequence,
        SubmittingUoi: by.submittingUoi,
        OfferSubmissionStatus: 'Countered',
        terms: by.terms,
      });
      return yes({ sequence });
    },

    recordAct: (offerId, side, status) => {
      const l = live(offerId);
      if (!l.ok) return l;
      const current = store.currentState(offerId);
      if (current === undefined) {
        return no('offer-not-found', `offer ${offerId} has no submission to record against`);
      }
      // No submission is appended. An act carrying no terms is not a turn, and
      // appending one would put a submission in the record that nobody made.
      store.setStatusOnCurrent(offerId, side, status);
      return yes({ sequence: current.sequence });
    },

    conclude: (offerId, side, as) => {
      const l = live(offerId);
      if (!l.ok) return l;
      const current = store.currentState(offerId);
      if (current === undefined) {
        return no('offer-not-found', `offer ${offerId} has no submission to conclude`);
      }
      store.setStatusOnCurrent(offerId, side, as);
      store.endOffer(offerId, as);
      return yes({ endedAs: as });
    },

    submissions: (offerId) => store.submissionsFor(offerId),
  };
};
