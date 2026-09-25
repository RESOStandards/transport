/**
 * Reading an offer payload, in either of the forms Section 2.5 permits.
 *
 * A submission may arrive on its own, or expanded into its `Offer` under
 * `Submissions`. A consumer must accept both and must not require the expanded
 * one, so this normalizes them to a single shape and every caller above it sees
 * one form. The alternative — each caller sniffing the document — is how half of
 * them end up supporting only the form their first integration happened to send.
 *
 * The property group is the same story one level down: it may travel inline under
 * `PropertyGroup` or by key alone, and a consumer must not require inline.
 *
 * Refusals name what was wrong rather than returning nothing, because a payload
 * that cannot be read is a certification observation and the reason is the part
 * worth reporting.
 */

/** One submission, normalized. Terms stay open: the Data Dictionary owns them. */
export interface ReadSubmission {
  readonly OfferSubmissionKey?: string;
  readonly OfferId: string;
  readonly OfferPropertyGroupKey?: string;
  readonly OfferSubmissionSequence?: number;
  /** Inline when the payload carried it, absent when it travelled by key alone. */
  readonly PropertyGroup?: Readonly<Record<string, unknown>>;
  readonly fields: Readonly<Record<string, unknown>>;
}

export interface ReadOffer {
  readonly OfferId: string;
  readonly submissions: readonly ReadSubmission[];
  /** Which form the document arrived in. Reported, never required. */
  readonly form: 'standalone' | 'expanded';
}

export type ReadResult =
  | { readonly ok: true; readonly value: ReadOffer }
  | { readonly ok: false; readonly reason: string };

const CONTEXT = '@reso.context';
const isObject = (v: unknown): v is Record<string, unknown> =>
  typeof v === 'object' && v !== null && !Array.isArray(v);

const contextOf = (doc: Record<string, unknown>): string =>
  typeof doc[CONTEXT] === 'string' ? doc[CONTEXT] : '';

/** Members that are structure rather than offer terms. */
const STRUCTURAL = new Set([CONTEXT, 'Submissions', 'PropertyGroup']);

const submissionOf = (raw: Record<string, unknown>, offerId: string): ReadSubmission => {
  const group = raw.PropertyGroup;
  return {
    ...(typeof raw.OfferSubmissionKey === 'string'
      ? { OfferSubmissionKey: raw.OfferSubmissionKey }
      : {}),
    OfferId: typeof raw.OfferId === 'string' ? raw.OfferId : offerId,
    ...(typeof raw.OfferPropertyGroupKey === 'string'
      ? { OfferPropertyGroupKey: raw.OfferPropertyGroupKey }
      : {}),
    ...(typeof raw.OfferSubmissionSequence === 'number'
      ? { OfferSubmissionSequence: raw.OfferSubmissionSequence }
      : {}),
    ...(isObject(group) ? { PropertyGroup: group } : {}),
    fields: Object.fromEntries(Object.entries(raw).filter(([k]) => !STRUCTURAL.has(k))),
  };
};

/**
 * Read a payload document.
 *
 * The form is decided by `@reso.context` and not by which members happen to be
 * present. Sniffing for a `Submissions` key would misread an offer that currently
 * has none as a standalone submission.
 */
export const readOfferDocument = (doc: unknown): ReadResult => {
  if (!isObject(doc)) return { ok: false, reason: 'payload is not a JSON object' };
  const context = contextOf(doc);
  if (context === '') {
    return { ok: false, reason: `payload carries no ${CONTEXT}, so its resource is unknown` };
  }

  if (context.endsWith(':offer')) {
    const raw = doc.Submissions;
    if (raw !== undefined && !Array.isArray(raw)) {
      return { ok: false, reason: 'Submissions is present but is not an array' };
    }
    const offerId = typeof doc.OfferId === 'string' ? doc.OfferId : '';
    if (offerId === '') return { ok: false, reason: 'offer payload carries no OfferId' };
    const items = (raw ?? []) as unknown[];
    if (!items.every(isObject)) {
      return { ok: false, reason: 'Submissions contains an element that is not an object' };
    }
    return {
      ok: true,
      value: {
        OfferId: offerId,
        // An offer with no submissions yet is a valid offer, not a broken one.
        submissions: items.map((i) => submissionOf(i, offerId)),
        form: 'expanded',
      },
    };
  }

  if (context.endsWith(':offersubmission')) {
    const offerId = typeof doc.OfferId === 'string' ? doc.OfferId : '';
    if (offerId === '') {
      return { ok: false, reason: 'submission payload carries no OfferId, so it correlates to no offer' };
    }
    return {
      ok: true,
      value: { OfferId: offerId, submissions: [submissionOf(doc, offerId)], form: 'standalone' },
    };
  }

  return { ok: false, reason: `unrecognized resource context: ${context}` };
};
