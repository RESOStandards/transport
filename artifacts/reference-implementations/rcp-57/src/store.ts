/**
 * The store interface.
 *
 * One interface, two implementations: an in-memory store for the public
 * playground, which keeps nothing, and SQLite for the open-source server. The
 * playground cannot drift from the product because both drive the same
 * operations through this.
 *
 * **The shape enforces the rules the specification cares most about**, rather
 * than leaving them to be remembered:
 *
 * - There is no `updateSubmission`. A submission is a turn in a negotiation and
 *   a negotiation is evidence, so a counter appends and never edits. A method
 *   that could edit one would make the append-only rule a convention.
 * - Status is set through `setStatusOnCurrent`, which takes no submission key.
 *   It can only ever reach the highest-sequence submission, because a superseded
 *   one is frozen. The signature is the constraint.
 * - `setStatusOnCurrent` takes a `side`, and writes only that side's field. No
 *   caller can set the counterparty's status, because no parameter lets it.
 *
 * Reads that the specification treats as load-bearing get their own methods
 * rather than being assembled by callers: the current state of an offer is the
 * pair on its highest-sequence submission, and if every caller derived that
 * itself, one of them would eventually derive it differently.
 */

import { orderSubmissions } from './sequence.js';

/** Which party is writing. Each writes only its own status field. */
export type Side = 'submitting' | 'receiving';

/**
 * A party to one offer: the submitting agent and their brokerage, the listing
 * agent and their brokerage, and the systems acting for any of them
 * (Section 2.11).
 *
 * Parties are recorded per offer, never per participant, because the
 * specification is explicit that involvement in one offer is not involvement in
 * another. A global "is on the network" set would be exactly the conflation
 * Section 2.11 forbids.
 */
export interface PartyRecord {
  readonly OfferId: string;
  /** A Unique Organization Identifier or Unique System Identifier. */
  readonly identifier: string;
  readonly role: Side;
}

/** An offer: one negotiation between two parties on one listing. */
export interface OfferRecord {
  readonly OfferKey: string;
  readonly OfferId: string;
  readonly ListingId?: string;
  readonly ListingKey?: string;
  readonly OfferOriginatingSystemName?: string;
  readonly OfferOriginatingSystemId?: string;
  readonly OfferUoi?: string;
  readonly OfferUsi?: string;
  /** The buyer this negotiation belongs to. A buyer may hold more than one. */
  readonly BuyerRef: string;
  /**
   * The listing this offer is on, as the reference the listing was published
   * under. Carried rather than derived from the coordinate: an offer may name the
   * listing by either identifier, and deriving it would group two offers on one
   * listing under different references depending on which member each carried.
   */
  readonly ListingRef: string;
  /** Set when the negotiation ends. An ended offer accepts no more turns. */
  readonly EndedAs?: 'Accepted' | 'Rejected' | 'Withdrawn' | 'Expired';
}

/** One turn. Immutable once superseded. */
export interface SubmissionRecord {
  readonly OfferSubmissionKey: string;
  readonly OfferId: string;
  readonly OfferPropertyGroupKey: string;
  readonly OfferSubmissionSequence: number;
  readonly SubmittingUoi: string;
  readonly OfferSubmissionStatus?: string;
  readonly OfferReceivedStatus?: string;
  /** The terms, as RESO Common Format. Validated before it reaches the store. */
  readonly terms: Readonly<Record<string, unknown>>;
}

/**
 * The subject property of a submission.
 *
 * Scoped to one offer. The key is chosen by the submitting party, so two
 * unrelated organizations can and do pick the same one, and a store keyed on it
 * alone let either overwrite the other's address. Keying on the offer as well
 * makes that collision impossible rather than merely unlikely.
 */
export interface PropertyGroupRecord {
  readonly OfferPropertyGroupKey: string;
  readonly fields: Readonly<Record<string, unknown>>;
}

/** The pair of statuses on the highest-sequence submission. */
export interface CurrentState {
  readonly sequence: number;
  readonly submitting?: string;
  readonly receiving?: string;
}

export interface OfferStore {
  /** Store a property group against the offer it belongs to. */
  putPropertyGroup: (offerId: string, group: PropertyGroupRecord) => void;
  /** Resolve a property group within one offer. Never across offers. */
  getPropertyGroup: (offerId: string, key: string) => PropertyGroupRecord | undefined;

  putOffer: (offer: OfferRecord) => void;
  getOffer: (offerId: string) => OfferRecord | undefined;
  /** Every offer a buyer holds on one listing. More than one is permitted. */
  offersForBuyer: (listingRef: string, buyerRef: string) => readonly OfferRecord[];
  endOffer: (offerId: string, endedAs: NonNullable<OfferRecord['EndedAs']>) => void;

  /** Append a turn. There is deliberately no way to change one. */
  appendSubmission: (submission: SubmissionRecord) => void;
  /** In sequence order, ties broken on the submitting organization ascending. */
  submissionsFor: (offerId: string) => readonly SubmissionRecord[];
  /** The highest sequence seen for this offer, or 0. */
  highestSequence: (offerId: string) => number;
  /** The pair on the highest-sequence submission. Derived in one place. */
  currentState: (offerId: string) => CurrentState | undefined;
  /** Writes one side's field on the current submission only. */
  setStatusOnCurrent: (offerId: string, side: Side, status: string) => void;

  /** Record a party to one offer. Scoped to the offer, never to the network. */
  addParty: (party: PartyRecord) => void;
  /** The parties to one offer. The implementation's own record, and the only
   *  basis on which a payload request may be authorized. */
  partiesOf: (offerId: string) => readonly PartyRecord[];

  close: () => void;
}

/**
 * Sequence ascending, ties on the submitting organization. Never on arrival.
 *
 * Re-exported rather than reimplemented. There were two identical sorts, and the
 * certification scenario for the tie-break exercised the one in `sequence.ts`
 * while the server served rows ordered by the one here, so the check observed a
 * copy of the rule rather than the rule the product used.
 */
export const inOrder = orderSubmissions;

