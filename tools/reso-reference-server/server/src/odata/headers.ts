import type { Response } from 'express';

/** Options for setting OData response headers. */
export interface ODataHeaderOptions {
  readonly entityId?: string;
  readonly locationUrl?: string;
  readonly preferenceApplied?: 'return=representation' | 'return=minimal';
}

/** Sets standard OData 4.01 response headers on an Express response. */
export const setODataHeaders = (res: Response, opts: ODataHeaderOptions = {}): void => {
  res.set('OData-Version', '4.01');
  if (opts.entityId) res.set('EntityId', opts.entityId);
  if (opts.locationUrl) res.set('Location', opts.locationUrl);
  if (opts.preferenceApplied) res.set('Preference-Applied', opts.preferenceApplied);
};
