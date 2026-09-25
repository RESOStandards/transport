/**
 * Protocol identifiers the specification fixes.
 *
 * Named rather than repeated, because they appeared as literals in the server,
 * the reader and seven tests, so a Data Dictionary version bump meant finding
 * nine sites and the consumer silently disagreeing with the producer if one was
 * missed.
 */

/** The Data Dictionary version these payloads declare. */
export const DD_VERSION = '2.1';

const context = (resource: string): string => `urn:reso:metadata:${DD_VERSION}:resource:${resource}`;

/** An `Offer` payload, with its submissions expanded under `Submissions`. */
export const OFFER_CONTEXT: string = context('offer');

/** A standalone `OfferSubmission` payload. */
export const SUBMISSION_CONTEXT: string = context('offersubmission');
