/** Status of a single resource in the data generator. */
export interface ResourceStatus {
  readonly resource: string;
  readonly fields: number;
  readonly count: number;
}

/** Response from GET /admin/data-generator/status. */
export interface GeneratorStatusResponse {
  readonly resources: ReadonlyArray<ResourceStatus>;
}

/** Related record result from the generator. */
export interface RelatedResult {
  readonly resource: string;
  readonly created: number;
  readonly failed: number;
}

/** Response from POST /admin/data-generator. */
export interface GenerateResponse {
  readonly resource: string;
  readonly created: number;
  readonly failed: number;
  readonly errors: ReadonlyArray<string>;
  readonly relatedResults: ReadonlyArray<RelatedResult>;
  readonly durationMs: number;
}

/** Request body for POST /admin/data-generator. */
export interface GenerateRequest {
  readonly resource: string;
  readonly count: number;
  readonly relatedRecords?: Readonly<Record<string, number>>;
}

const ADMIN_TOKEN_KEY = 'reso-admin-token';

/** Gets the stored admin token, or null if not set. */
export const getAdminToken = (): string | null => localStorage.getItem(ADMIN_TOKEN_KEY);

/** Stores the admin token for subsequent requests. */
export const setAdminToken = (token: string): void => localStorage.setItem(ADMIN_TOKEN_KEY, token);

/** Clears the stored admin token. */
export const clearAdminToken = (): void => localStorage.removeItem(ADMIN_TOKEN_KEY);

/** Makes an authenticated request to an admin endpoint. */
const adminFetch = async (url: string, options: RequestInit = {}): Promise<Response> => {
  const token = getAdminToken();
  const headers: Record<string, string> = {
    Accept: 'application/json',
    ...((options.headers ?? {}) as Record<string, string>)
  };
  if (token) {
    headers.Authorization = `Bearer ${token}`;
  }

  return fetch(url, { ...options, headers });
};

/** Fetches the data generator status (available resources and counts). */
export const getGeneratorStatus = async (): Promise<GeneratorStatusResponse> => {
  const res = await adminFetch('/admin/data-generator/status');
  if (!res.ok) {
    const errorBody = await res.json().catch(() => ({}));
    throw new Error((errorBody as { error?: { message?: string } })?.error?.message ?? `HTTP ${res.status}`);
  }
  return res.json();
};

/** Triggers data generation for a resource. */
export const generateData = async (request: GenerateRequest): Promise<GenerateResponse> => {
  const res = await adminFetch('/admin/data-generator', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(request)
  });
  if (!res.ok) {
    const errorBody = await res.json().catch(() => ({}));
    throw new Error((errorBody as { error?: { message?: string } })?.error?.message ?? `HTTP ${res.status}`);
  }
  return res.json();
};
