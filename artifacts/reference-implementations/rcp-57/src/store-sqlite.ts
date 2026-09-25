/**
 * SQLite store, for the open-source server.
 *
 * `node:sqlite` ships with Node, so persistence costs no dependency, no native
 * build and nothing to install in a container. That matters more than it looks:
 * the alternative was a native module, which brings a build step, a platform
 * matrix and an Electron ABI question, all for a table of offers.
 *
 * Two constraints are enforced by the schema rather than by code above it,
 * because a rule the database refuses to break cannot be broken by a caller
 * that forgot:
 *
 * - `(OfferId, OfferSubmissionKey)` is unique and nothing updates a submission
 *   row, so a turn cannot be rewritten.
 * - A submission's `OfferPropertyGroupKey` is a foreign key, so a submission
 *   whose property group does not resolve cannot be stored at all.
 */

import { DatabaseSync } from 'node:sqlite';
import {
  type CurrentState,
  inOrder,
  type OfferRecord,
  type OfferStore,
  type PartyRecord,
  type Side,
  type SubmissionRecord,
} from './store.js';

const SCHEMA = `
PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS property_group (
  OfferId               TEXT NOT NULL,
  OfferPropertyGroupKey TEXT NOT NULL,
  fields                TEXT NOT NULL,
  PRIMARY KEY (OfferId, OfferPropertyGroupKey)
);

CREATE TABLE IF NOT EXISTS offer (
  OfferId                    TEXT PRIMARY KEY,
  OfferKey                   TEXT NOT NULL,
  ListingId                  TEXT,
  ListingKey                 TEXT,
  OfferOriginatingSystemName TEXT,
  OfferOriginatingSystemId   TEXT,
  OfferUoi                   TEXT,
  OfferUsi                   TEXT,
  BuyerRef                   TEXT NOT NULL,
  ListingRef                 TEXT NOT NULL,
  EndedAs                    TEXT
);
CREATE INDEX IF NOT EXISTS offer_by_buyer ON offer (ListingRef, BuyerRef);

CREATE TABLE IF NOT EXISTS submission (
  OfferSubmissionKey      TEXT NOT NULL,
  OfferId                 TEXT NOT NULL REFERENCES offer (OfferId),
  OfferPropertyGroupKey   TEXT NOT NULL,
  OfferSubmissionSequence INTEGER NOT NULL,
  SubmittingUoi           TEXT NOT NULL,
  OfferSubmissionStatus   TEXT,
  OfferReceivedStatus     TEXT,
  terms                   TEXT NOT NULL,
  PRIMARY KEY (OfferId, OfferSubmissionKey),
  FOREIGN KEY (OfferId, OfferPropertyGroupKey)
    REFERENCES property_group (OfferId, OfferPropertyGroupKey)
);
CREATE INDEX IF NOT EXISTS submission_by_offer ON submission (OfferId, OfferSubmissionSequence);

CREATE TABLE IF NOT EXISTS party (
  OfferId    TEXT NOT NULL REFERENCES offer (OfferId),
  identifier TEXT NOT NULL,
  role       TEXT NOT NULL,
  PRIMARY KEY (OfferId, identifier)
);
`;

const asOffer = (r: Record<string, unknown>): OfferRecord => ({
  OfferKey: String(r.OfferKey),
  OfferId: String(r.OfferId),
  ...(r.ListingId == null ? {} : { ListingId: String(r.ListingId) }),
  ...(r.ListingKey == null ? {} : { ListingKey: String(r.ListingKey) }),
  ...(r.OfferOriginatingSystemName == null
    ? {}
    : { OfferOriginatingSystemName: String(r.OfferOriginatingSystemName) }),
  ...(r.OfferOriginatingSystemId == null
    ? {}
    : { OfferOriginatingSystemId: String(r.OfferOriginatingSystemId) }),
  ...(r.OfferUoi == null ? {} : { OfferUoi: String(r.OfferUoi) }),
  ...(r.OfferUsi == null ? {} : { OfferUsi: String(r.OfferUsi) }),
  BuyerRef: String(r.BuyerRef),
  ListingRef: String(r.ListingRef),
  ...(r.EndedAs == null ? {} : { EndedAs: String(r.EndedAs) as OfferRecord['EndedAs'] }),
});

const asSubmission = (r: Record<string, unknown>): SubmissionRecord => ({
  OfferSubmissionKey: String(r.OfferSubmissionKey),
  OfferId: String(r.OfferId),
  OfferPropertyGroupKey: String(r.OfferPropertyGroupKey),
  OfferSubmissionSequence: Number(r.OfferSubmissionSequence),
  SubmittingUoi: String(r.SubmittingUoi),
  ...(r.OfferSubmissionStatus == null
    ? {}
    : { OfferSubmissionStatus: String(r.OfferSubmissionStatus) }),
  ...(r.OfferReceivedStatus == null ? {} : { OfferReceivedStatus: String(r.OfferReceivedStatus) }),
  terms: JSON.parse(String(r.terms)) as Record<string, unknown>,
});

export const sqliteStore = (location = ':memory:'): OfferStore => {
  const db = new DatabaseSync(location);
  db.exec(SCHEMA);

  const submissionsFor = (offerId: string): readonly SubmissionRecord[] =>
    inOrder(
      db
        .prepare('SELECT * FROM submission WHERE OfferId = ?')
        .all(offerId)
        .map((r) => asSubmission(r as Record<string, unknown>))
    );

  const currentOf = (offerId: string): SubmissionRecord | undefined => {
    const all = submissionsFor(offerId);
    return all.length === 0 ? undefined : all[all.length - 1];
  };

  return {
    putPropertyGroup: (offerId, g) => {
      db.prepare(
        'INSERT OR REPLACE INTO property_group (OfferId, OfferPropertyGroupKey, fields) VALUES (?, ?, ?)'
      ).run(offerId, g.OfferPropertyGroupKey, JSON.stringify(g.fields));
    },
    getPropertyGroup: (offerId, key) => {
      const r = db
        .prepare('SELECT * FROM property_group WHERE OfferId = ? AND OfferPropertyGroupKey = ?')
        .get(offerId, key) as Record<string, unknown> | undefined;
      return r === undefined
        ? undefined
        : {
            OfferPropertyGroupKey: String(r.OfferPropertyGroupKey),
            fields: JSON.parse(String(r.fields)) as Record<string, unknown>,
          };
    },

    putOffer: (o) => {
      db.prepare(
        `INSERT OR REPLACE INTO offer
         (OfferId, OfferKey, ListingId, ListingKey, OfferOriginatingSystemName,
          OfferOriginatingSystemId, OfferUoi, OfferUsi, BuyerRef, ListingRef, EndedAs)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`
      ).run(
        o.OfferId,
        o.OfferKey,
        o.ListingId ?? null,
        o.ListingKey ?? null,
        o.OfferOriginatingSystemName ?? null,
        o.OfferOriginatingSystemId ?? null,
        o.OfferUoi ?? null,
        o.OfferUsi ?? null,
        o.BuyerRef,
        o.ListingRef,
        o.EndedAs ?? null
      );
    },
    getOffer: (offerId) => {
      const r = db.prepare('SELECT * FROM offer WHERE OfferId = ?').get(offerId) as
        | Record<string, unknown>
        | undefined;
      return r === undefined ? undefined : asOffer(r);
    },
    offersForBuyer: (listingRef, buyerRef) =>
      db
        .prepare('SELECT * FROM offer WHERE ListingRef = ? AND BuyerRef = ?')
        .all(listingRef, buyerRef)
        .map((r) => asOffer(r as Record<string, unknown>)),
    endOffer: (offerId, endedAs) => {
      db.prepare('UPDATE offer SET EndedAs = ? WHERE OfferId = ?').run(endedAs, offerId);
    },

    appendSubmission: (s) => {
      db.prepare(
        `INSERT INTO submission
         (OfferSubmissionKey, OfferId, OfferPropertyGroupKey, OfferSubmissionSequence,
          SubmittingUoi, OfferSubmissionStatus, OfferReceivedStatus, terms)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)`
      ).run(
        s.OfferSubmissionKey,
        s.OfferId,
        s.OfferPropertyGroupKey,
        s.OfferSubmissionSequence,
        s.SubmittingUoi,
        s.OfferSubmissionStatus ?? null,
        s.OfferReceivedStatus ?? null,
        JSON.stringify(s.terms)
      );
    },
    submissionsFor,
    highestSequence: (offerId) => {
      const r = db
        .prepare('SELECT MAX(OfferSubmissionSequence) AS hi FROM submission WHERE OfferId = ?')
        .get(offerId) as { hi: number | null } | undefined;
      return r?.hi ?? 0;
    },
    currentState: (offerId): CurrentState | undefined => {
      const cur = currentOf(offerId);
      return cur === undefined
        ? undefined
        : {
            sequence: cur.OfferSubmissionSequence,
            ...(cur.OfferSubmissionStatus === undefined
              ? {}
              : { submitting: cur.OfferSubmissionStatus }),
            ...(cur.OfferReceivedStatus === undefined ? {} : { receiving: cur.OfferReceivedStatus }),
          };
    },
    setStatusOnCurrent: (offerId, side: Side, status) => {
      const cur = currentOf(offerId);
      if (cur === undefined) throw new Error(`no submission to set status on: ${offerId}`);
      const column = side === 'submitting' ? 'OfferSubmissionStatus' : 'OfferReceivedStatus';
      db.prepare(
        `UPDATE submission SET ${column} = ? WHERE OfferId = ? AND OfferSubmissionKey = ?`
      ).run(status, offerId, cur.OfferSubmissionKey);
    },

    addParty: (p) => {
      db.prepare(
        'INSERT OR REPLACE INTO party (OfferId, identifier, role) VALUES (?, ?, ?)'
      ).run(p.OfferId, p.identifier, p.role);
    },
    partiesOf: (offerId): readonly PartyRecord[] =>
      db
        .prepare('SELECT * FROM party WHERE OfferId = ?')
        .all(offerId)
        .map((r) => {
          const x = r as Record<string, unknown>;
          return {
            OfferId: String(x.OfferId),
            identifier: String(x.identifier),
            role: String(x.role) as Side,
          };
        }),

    close: () => db.close(),
  };
};
