/**
 * The public surface of the RCP-57 reference implementation.
 *
 * `package.json` declared `main`, `types` and `exports` pointing here before this
 * file existed, so anything importing the package failed to resolve. The
 * declaration is the promise; this is the thing promised.
 */

export { bearerOf, decide, isPartyTo, statusFor, tokenResolver } from './auth.js';
export type { Decision, NonPartyAnswer, Requester, ResolveRequester } from './auth.js';

export { coverage, extractChecks } from './checks.js';
export type {
  CoverageReport, ObservationKind, ObservationResult, Scenario, SpecCheck,
} from './checks.js';

export { validateCoordinate } from './coordinate.js';
export type { Coordinate } from './coordinate.js';

export { createHub } from './hub.js';
export type { Hub, Outcome, Refusal, SubmitInput, Terms } from './hub.js';

export { metadataFromSpec } from './metadata.js';
export type { MetadataField, MetadataLookup, MetadataReport } from './metadata.js';

export { readOfferDocument } from './read-payload.js';
export type { ReadOffer, ReadResult, ReadSubmission } from './read-payload.js';

export { scenarios } from './scenarios.js';

export { nextSequence, orderSubmissions } from './sequence.js';
export type { Ordered, Submission } from './sequence.js';

export { buyerFieldNames, createOfferServer, listen } from './server.js';
export type { ServerConfig } from './server.js';

export { inOrder } from './store.js';
export type {
  CurrentState, OfferRecord, OfferStore, PartyRecord, PropertyGroupRecord, Side, SubmissionRecord,
} from './store.js';

export { sqliteStore } from './store-sqlite.js';

export { isVerified, readSuiteResults, stalenessOf } from './verified.js';
export type { Executed, SuiteResults } from './verified.js';
