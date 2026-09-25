import { readFile } from 'node:fs/promises';
import { beforeAll, describe, expect, it } from 'vitest';
import { tokenResolver } from '../src/auth.js';
import { createHub } from '../src/hub.js';
import { metadataFromSpec, type MetadataReport } from '../src/metadata.js';
import { buyerFieldNames, createOfferServer, listen } from '../src/server.js';
import { sqliteStore } from '../src/store-sqlite.js';
import { specPath } from '../src/paths.js';

/**
 * The model against what the server actually serves.
 *
 * Two defects got through before this existed, and both were the same shape: the
 * implementation used a member name the specification does not define, and the
 * tests agreed because they used the same name. `PropertyGroup` and `Submissions`
 * appeared in payloads that no field table declared, and the withholding path
 * hid `BuyerName` and `CoBuyerName`, neither of which exists.
 *
 * Nothing catches that class except comparing the payload to the model, which is
 * what this does. The model is generated from the specification's field tables, so
 * the comparison is against the specification and not against a second copy of it.
 */


/** RCF structural members, which are not fields of any resource. */
const STRUCTURAL = new Set(['@reso.context']);

let model: MetadataReport;
let payload: Record<string, unknown>;

const fieldsOf = (resource: string): Set<string> =>
  new Set(model.fields.filter((f) => f.resourceName === resource).map((f) => f.fieldName));

beforeAll(async () => {
  model = metadataFromSpec(await readFile(specPath, 'utf-8'), '2026-09-24');

  const store = sqliteStore();
  const hub = createHub(store);
  hub.publishListing('11284417', ['M00000001']);
  const r = hub.submitOffer({
    coordinate: {
      ListingId: '11284417',
      ListingKey: 'MRED-L-11284417',
      OfferOriginatingSystemName: 'Midwest Real Estate Data',
      OfferUoi: 'M00000136',
    },
    buyerRef: 'dana',
    submittingUoi: 'M00000136',
    propertyGroup: {
      OfferPropertyGroupKey: 'PG-1',
      fields: { StreetNumber: '1803', StreetName: 'Bayshore Rd', City: 'Chicago' },
    },
    terms: {
      PurchasePrice: 530000,
      EarnestMoney: 15000,
      BuyerFinancing: ['Conventional'],
      OfferBuyerLegalName: 'Dana R. Whitfield',
    },
  });
  if (!r.ok) throw new Error('setup failed');
  hub.counter(r.value.offerId, { submittingUoi: 'M00000001', terms: { PurchasePrice: 545000 } });

  const started = await listen(
    createOfferServer({ store, hub, resolve: tokenResolver(new Map([['t', 'M00000136']])) })
  );
  const res = await fetch(`${started.url}/offers/${encodeURIComponent(r.value.offerId)}/payload`, {
    headers: { authorization: 'Bearer t' },
  });
  payload = (await res.json()) as Record<string, unknown>;
  await started.close();
  store.close();
});

describe('either-payload-form — a served payload carries only declared members', () => {
  it('declares every member of the Offer document', () => {
    const declared = fieldsOf('Offer');
    const undeclared = Object.keys(payload).filter(
      (k) => !STRUCTURAL.has(k) && !declared.has(k)
    );
    expect(undeclared).toEqual([]);
  });

  it('declares every member of each expanded submission', () => {
    const declared = fieldsOf('OfferSubmission');
    const subs = payload.Submissions as Record<string, unknown>[];
    expect(subs.length).toBeGreaterThan(1);
    const undeclared = [
      ...new Set(subs.flatMap((s) => Object.keys(s))),
    ].filter((k) => !STRUCTURAL.has(k) && !declared.has(k));
    expect(undeclared).toEqual([]);
  });

  it('declares every member of the inline property group', () => {
    const declared = fieldsOf('OfferPropertyGroup');
    const subs = payload.Submissions as Record<string, unknown>[];
    const group = subs[0].PropertyGroup as Record<string, unknown>;
    expect(group).toBeDefined();
    expect(Object.keys(group).filter((k) => !declared.has(k))).toEqual([]);
  });

  it('declares the expansion properties themselves, with their targets', () => {
    const byName = new Map(model.fields.map((f) => [`${f.resourceName}.${f.fieldName}`, f]));
    const submissions = byName.get('Offer.Submissions');
    const group = byName.get('OfferSubmission.PropertyGroup');
    expect(submissions?.sourceResource).toBe('OfferSubmission');
    expect(submissions?.isCollection).toBe(true);
    expect(group?.sourceResource).toBe('OfferPropertyGroup');
    expect(group?.isCollection).toBe(false);
  });

  it('serves the expansion as an array and the group as an object', () => {
    const subs = payload.Submissions as Record<string, unknown>[];
    expect(Array.isArray(subs)).toBe(true);
    expect(Array.isArray(subs[0].PropertyGroup)).toBe(false);
    // A collection-valued expansion served as a bare object, or a single-valued
    // one served as an array, is the mismatch the isCollection flag exists for.
    expect(typeof subs[0].PropertyGroup).toBe('object');
  });

  it('names the Offer resource in its context, not OfferSubmission', () => {
    expect(payload['@reso.context']).toBe('urn:reso:metadata:2.1:resource:offer');
  });
});

describe('withheld-buyer-fields-not-an-error — the withholdable fields are the ones the model defines', () => {
  it('withholds exactly the buyer and co-buyer fields of the model', () => {
    // The list in the server is spelled out. This is what keeps it honest: it
    // must equal what Section 2.5 actually defines, not what looked plausible.
    const fromModel = model.fields
      .filter((f) => f.resourceName === 'OfferSubmission')
      .map((f) => f.fieldName)
      .filter((n) => /^Offer(Co)?Buyer/.test(n))
      .sort();
    expect([...buyerFieldNames].sort()).toEqual(fromModel);
  });

  it('withholds a field the model actually defines', () => {
    const declared = fieldsOf('OfferSubmission');
    for (const name of buyerFieldNames) expect(declared.has(name)).toBe(true);
  });
});
