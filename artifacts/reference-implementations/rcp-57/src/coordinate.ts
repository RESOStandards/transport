/**
 * The listing coordinate.
 *
 * An `Offer` names a listing, and a listing identifier alone does not identify
 * one: two organizations can issue the same `ListingId` for different
 * properties. So the coordinate is a listing identifier plus the organization or
 * system that issued it.
 *
 * The rule is asymmetric in a way that is easy to get backwards, and the
 * specification had it backwards until 2026-09-24. The **originating system
 * pair is the required side** and the organization identifier is optional and
 * additional. An implementation that publishes `OfferUoi` alone is not
 * forward-looking, it is unconformant today — RESO versioning requires a
 * provider implementing a new element to also support the current standard until
 * the next major version.
 *
 * Both directions are checked here because both are certification checks, and
 * they fail in opposite directions: one rejects an offer that dropped the
 * current standard, the other rejects an implementation too strict to accept a
 * coordinate qualified only by a name.
 */

import type { ObservationResult } from './checks.js';

/** The coordinate members an `Offer` may carry. */
export interface Coordinate {
  readonly ListingId?: string;
  readonly ListingKey?: string;
  /** Current standard. One of this pair is required. */
  readonly OfferOriginatingSystemName?: string;
  readonly OfferOriginatingSystemId?: string;
  /** Optional, additional, and never a substitute for the pair above. */
  readonly OfferUoi?: string;
  /** Narrows to the system rather than the organization. */
  readonly OfferUsi?: string;
}

const present = (v: string | undefined): boolean => v !== undefined && v.trim().length > 0;

/**
 * Is this a coordinate an implementation must accept?
 *
 * Returns the reason on failure rather than a boolean, because "which half is
 * missing" is the whole diagnostic value — a missing listing identifier and a
 * missing organization member are different mistakes with different fixes.
 */
export const validateCoordinate = (
  c: Coordinate
): { readonly ok: true } | { readonly ok: false; readonly reason: string } => {
  const hasListing = present(c.ListingId) || present(c.ListingKey);
  const hasOriginating =
    present(c.OfferOriginatingSystemName) || present(c.OfferOriginatingSystemId);

  if (!hasListing) {
    return { ok: false, reason: 'carries neither ListingId nor ListingKey' };
  }
  if (!hasOriginating) {
    return present(c.OfferUoi)
      ? {
          ok: false,
          // Named separately because it is the mistake a well-intentioned
          // implementer makes: adopting the future element and dropping the
          // current one, which fails certification rather than anticipating it.
          reason:
            'carries OfferUoi but neither OfferOriginatingSystemName nor ' +
            'OfferOriginatingSystemId. The organization identifier is additional, ' +
            'never a substitute, until Data Dictionary 3.0',
        }
      : {
          ok: false,
          reason:
            'carries a listing identifier with no organization or system member: ' +
            'another organization may issue the same listing identifier',
        };
  }
  return { ok: true };
};

/** S3-09: an offer whose coordinate is incomplete must be refused. */
export const observeRejectsIncomplete = (
  scenario: string,
  offered: Coordinate,
  accepted: boolean
): ObservationResult => {
  const verdict = validateCoordinate(offered);
  if (verdict.ok) {
    return {
      scenario,
      passed: false,
      indeterminate: true,
      message:
        'the coordinate under test is valid, so this scenario cannot observe a ' +
        'rejection. Fix the fixture rather than the candidate.',
    };
  }
  return accepted
    ? { scenario, passed: false, message: `accepted an offer that ${verdict.reason}` }
    : { scenario, passed: true, message: `refused an offer that ${verdict.reason}` };
};

/** S3-28: a coordinate qualified only by a name must be accepted. */
export const observeAcceptsLegacy = (
  scenario: string,
  offered: Coordinate,
  accepted: boolean
): ObservationResult => {
  const verdict = validateCoordinate(offered);
  if (!verdict.ok) {
    return {
      scenario,
      passed: false,
      indeterminate: true,
      message: `the coordinate under test is itself invalid (${verdict.reason}), so ` +
        'acceptance proves nothing. Fix the fixture.',
    };
  }
  return accepted
    ? { scenario, passed: true, message: 'accepted a coordinate qualified only by a name' }
    : {
        scenario,
        passed: false,
        message:
          'refused a valid coordinate. An implementation must not require an ' +
          'organization identifier that most listings do not yet carry',
      };
};
