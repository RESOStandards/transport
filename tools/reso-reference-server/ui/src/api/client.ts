import type { ODataCollectionResponse, ODataError, ResourceName } from '../types';

/** Query parameters for collection requests. */
export interface CollectionParams {
  readonly $filter?: string;
  readonly $select?: string;
  readonly $orderby?: string;
  readonly $top?: number;
  readonly $skip?: number;
  readonly $count?: boolean;
  readonly $expand?: string;
}

/** Builds a query string from OData params. */
const buildQueryString = (params: CollectionParams): string => {
  const parts: string[] = [];
  if (params.$filter) parts.push(`$filter=${encodeURIComponent(params.$filter)}`);
  if (params.$select) parts.push(`$select=${encodeURIComponent(params.$select)}`);
  if (params.$orderby) parts.push(`$orderby=${encodeURIComponent(params.$orderby)}`);
  if (params.$top !== undefined) parts.push(`$top=${params.$top}`);
  if (params.$skip !== undefined) parts.push(`$skip=${params.$skip}`);
  if (params.$count) parts.push('$count=true');
  if (params.$expand) parts.push(`$expand=${encodeURIComponent(params.$expand)}`);
  return parts.length > 0 ? `?${parts.join('&')}` : '';
};

/** Extracts an OData error from a failed response, or builds a generic one. */
const parseError = async (res: Response): Promise<ODataError> => {
  try {
    const body = await res.json();
    if (body?.error) return body as ODataError;
  } catch {
    // Ignore parse errors
  }
  return {
    error: {
      code: String(res.status),
      message: res.statusText || 'Request failed',
      details: []
    }
  };
};

/** Queries a resource collection. */
export const queryCollection = async (resource: ResourceName, params: CollectionParams = {}): Promise<ODataCollectionResponse> => {
  const url = `/${resource}${buildQueryString(params)}`;
  const res = await fetch(url, {
    headers: { Accept: 'application/json', 'OData-Version': '4.01' }
  });
  if (!res.ok) throw await parseError(res);
  return res.json();
};

/** Fetches a collection response from a raw URL (e.g., an @odata.nextLink). */
export const fetchCollectionByUrl = async (url: string): Promise<ODataCollectionResponse> => {
  const res = await fetch(url, {
    headers: { Accept: 'application/json', 'OData-Version': '4.01' }
  });
  if (!res.ok) throw await parseError(res);
  return res.json();
};

/** Reads a single entity by key. */
export const readEntity = async (
  resource: ResourceName,
  key: string,
  params?: { $select?: string; $expand?: string }
): Promise<Record<string, unknown>> => {
  const qs: string[] = [];
  if (params?.$select) qs.push(`$select=${encodeURIComponent(params.$select)}`);
  if (params?.$expand) qs.push(`$expand=${encodeURIComponent(params.$expand)}`);
  const queryStr = qs.length > 0 ? `?${qs.join('&')}` : '';
  const url = `/${resource}('${encodeURIComponent(key)}')${queryStr}`;
  const res = await fetch(url, {
    headers: { Accept: 'application/json', 'OData-Version': '4.01' }
  });
  if (!res.ok) throw await parseError(res);
  return res.json();
};

/** Creates a new entity. Returns the created record (with server-generated key). */
export const createEntity = async (resource: ResourceName, body: Record<string, unknown>): Promise<Record<string, unknown>> => {
  const res = await fetch(`/${resource}`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      Accept: 'application/json',
      'OData-Version': '4.01',
      Prefer: 'return=representation'
    },
    body: JSON.stringify(body)
  });
  if (!res.ok) throw await parseError(res);
  if (res.status === 204) return {};
  return res.json();
};

/** Updates an entity by key (PATCH — merge semantics). */
export const updateEntity = async (
  resource: ResourceName,
  key: string,
  body: Record<string, unknown>
): Promise<Record<string, unknown>> => {
  const res = await fetch(`/${resource}('${encodeURIComponent(key)}')`, {
    method: 'PATCH',
    headers: {
      'Content-Type': 'application/json',
      Accept: 'application/json',
      'OData-Version': '4.01',
      Prefer: 'return=representation'
    },
    body: JSON.stringify(body)
  });
  if (!res.ok) throw await parseError(res);
  if (res.status === 204) return {};
  return res.json();
};

/** Deletes an entity by key. */
export const deleteEntity = async (resource: ResourceName, key: string): Promise<void> => {
  const res = await fetch(`/${resource}('${encodeURIComponent(key)}')`, {
    method: 'DELETE',
    headers: { 'OData-Version': '4.01' }
  });
  if (!res.ok) throw await parseError(res);
};
