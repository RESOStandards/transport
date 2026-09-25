import { describe, expect, it } from 'vitest';
import {
  type Coordinate,
  observeAcceptsLegacy,
  observeRejectsIncomplete,
  validateCoordinate,
} from '../src/coordinate.js';

/**
 * The coordinate rule, in both directions.
 *
 * The specification had this backwards until 2026-09-24: it let `OfferUoi`
 * alone satisfy the coordinate and advised implementers to treat the
 * originating-system pair as values they merely accept from others. Followed
 * literally, that advice failed certification, because RESO versioning requires
 * a provider implementing a new element to also support the current standard.
 *
 * These tests exist so that correction cannot be quietly undone.
 */

const listing: Coordinate = { ListingId: '11284417' };

describe('validateCoordinate', () => {
  it('accepts a listing qualified by an originating system name', () => {
    expect(validateCoordinate({ ...listing, OfferOriginatingSystemName: 'MyMls' }).ok).toBe(true);
  });

  it('accepts a listing qualified by an originating system identifier', () => {
    expect(validateCoordinate({ ...listing, OfferOriginatingSystemId: 'M00000136' }).ok).toBe(true);
  });

  it('accepts the organization identifier alongside the pair', () => {
    const v = validateCoordinate({
      ...listing,
      OfferOriginatingSystemName: 'MyMls',
      OfferUoi: 'M00000136',
      OfferUsi: '50039',
    });
    expect(v.ok).toBe(true);
  });

  it('REFUSES the organization identifier on its own, which is the correction', () => {
    const v = validateCoordinate({ ...listing, OfferUoi: 'M00000136' });
    expect(v.ok).toBe(false);
    if (!v.ok) {
      expect(v.reason).toMatch(/additional, never a substitute/);
      // The diagnostic must name the specific mistake, because a generic
      // "incomplete coordinate" sends an implementer looking in the wrong place.
      expect(v.reason).toMatch(/OfferUoi/);
    }
  });

  it('refuses a listing identifier with no organization member at all', () => {
    const v = validateCoordinate(listing);
    expect(v.ok).toBe(false);
    if (!v.ok) expect(v.reason).toMatch(/no organization or system member/);
  });

  it('refuses an offer that names no listing', () => {
    const v = validateCoordinate({ OfferOriginatingSystemName: 'MyMls' });
    expect(v.ok).toBe(false);
    if (!v.ok) expect(v.reason).toMatch(/neither ListingId nor ListingKey/);
  });

  it('treats blank as absent, so whitespace cannot satisfy a requirement', () => {
    expect(validateCoordinate({ ListingId: '  ', OfferOriginatingSystemName: 'MyMls' }).ok).toBe(
      false
    );
    expect(validateCoordinate({ ...listing, OfferOriginatingSystemName: '   ' }).ok).toBe(false);
  });
});

describe('coordinate-needs-system-member — an incomplete coordinate must be refused', () => {
  const uoiOnly: Coordinate = { ...listing, OfferUoi: 'M00000136' };

  it('passes when the candidate refuses it', () => {
    const r = observeRejectsIncomplete('S3-09-a', uoiOnly, false);
    expect(r.passed).toBe(true);
  });

  it('fails when the candidate accepts it', () => {
    const r = observeRejectsIncomplete('S3-09-a', uoiOnly, true);
    expect(r.passed).toBe(false);
    expect(r.message).toMatch(/accepted an offer that/);
  });

  it('is indeterminate, not a pass, when the fixture is valid', () => {
    const valid: Coordinate = { ...listing, OfferOriginatingSystemName: 'MyMls' };
    const r = observeRejectsIncomplete('S3-09-a', valid, false);
    expect(r.indeterminate).toBe(true);
    expect(r.passed).toBe(false);
    expect(r.message).toMatch(/Fix the fixture/);
  });
});

describe('name-only-coordinate-accepted — a name-only coordinate must be accepted', () => {
  const nameOnly: Coordinate = { ...listing, OfferOriginatingSystemName: 'MyMls' };

  it('passes when the candidate accepts it', () => {
    expect(observeAcceptsLegacy('S3-28-a', nameOnly, true).passed).toBe(true);
  });

  it('fails when the candidate is too strict to accept it', () => {
    const r = observeAcceptsLegacy('S3-28-a', nameOnly, false);
    expect(r.passed).toBe(false);
    expect(r.message).toMatch(/must not require an organization identifier/);
  });

  it('is indeterminate when the fixture is itself invalid', () => {
    const r = observeAcceptsLegacy('S3-28-a', { ...listing, OfferUoi: 'M1' }, true);
    expect(r.indeterminate).toBe(true);
    expect(r.passed).toBe(false);
  });
});

describe('the two checks are genuinely opposed', () => {
  it('no single coordinate can satisfy both observations', () => {
    // S3-09 needs an invalid fixture, S3-28 needs a valid one. If a coordinate
    // ever satisfied both, one of the two checks would be vacuous.
    const cases: Coordinate[] = [
      { ...listing, OfferUoi: 'M1' },
      { ...listing, OfferOriginatingSystemName: 'MyMls' },
      listing,
      {},
    ];
    for (const c of cases) {
      const a = observeRejectsIncomplete('x', c, false);
      const b = observeAcceptsLegacy('y', c, true);
      expect(a.indeterminate === true || b.indeterminate === true).toBe(true);
    }
  });
});
