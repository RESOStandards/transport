/**
 * OData error response parsing and type guards.
 */

import type { ODataErrorBody, ODataErrorDetail, ODataResponse } from '../types.js';

/**
 * Type guard: check if a body is an OData error response.
 */
export const isODataError = (body: unknown): body is ODataErrorBody => {
  if (typeof body !== 'object' || body === null) return false;
  const obj = body as Record<string, unknown>;
  if (typeof obj.error !== 'object' || obj.error === null) return false;
  const error = obj.error as Record<string, unknown>;
  return typeof error.code === 'string' && typeof error.message === 'string';
};

/**
 * Parse an OData error from a response body.
 * Returns null if the body is not an OData error.
 */
export const parseODataError = (response: ODataResponse): ODataErrorBody | null => {
  if (isODataError(response.body)) {
    return response.body;
  }
  return null;
};

/**
 * Extract error detail targets from an OData error response.
 * Returns an empty array if there are no details.
 */
export const getErrorTargets = (error: ODataErrorBody): ReadonlyArray<string> => {
  const details = error.error.details ?? [];
  return details.filter((d): d is ODataErrorDetail & { target: string } => typeof d.target === 'string').map(d => d.target);
};
