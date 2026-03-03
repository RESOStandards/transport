import { describe, expect, it } from 'vitest';
import type { EntityType, ODataResponse } from '../src/lib/types.js';
import {
  validateEmptyResponse,
  validateEntityIdHeader,
  validateJsonResponse,
  validateLocationHeader,
  validateODataAnnotation,
  validateODataError,
  validateODataVersionHeader,
  validatePreferenceApplied,
  validateResponseContainsPayload,
  validateStatusCode,
  validateStatusCodeRange
} from '../src/lib/validators.js';

const makeResponse = (overrides: Partial<ODataResponse> = {}): ODataResponse => ({
  status: 200,
  headers: { 'odata-version': '4.01' },
  body: {},
  rawBody: '{}',
  ...overrides
});

const sampleEntityType: EntityType = {
  name: 'Property',
  keyProperties: ['ListingKey'],
  properties: [
    { name: 'ListingKey', type: 'Edm.String' },
    { name: 'ListPrice', type: 'Edm.Decimal' },
    { name: 'BedroomsTotal', type: 'Edm.Int64' }
  ]
};

describe('validateStatusCode', () => {
  it('passes when status is in allowed list', () => {
    const result = validateStatusCode(makeResponse({ status: 201 }), [201, 204]);
    expect(result.status).toBe('pass');
  });

  it('fails when status is not in allowed list', () => {
    const result = validateStatusCode(makeResponse({ status: 500 }), [201, 204]);
    expect(result.status).toBe('fail');
  });
});

describe('validateStatusCodeRange', () => {
  it('passes for status in range', () => {
    const result = validateStatusCodeRange(makeResponse({ status: 404 }), 400, 499);
    expect(result.status).toBe('pass');
  });

  it('fails for status outside range', () => {
    const result = validateStatusCodeRange(makeResponse({ status: 200 }), 400, 499);
    expect(result.status).toBe('fail');
  });
});

describe('validateODataVersionHeader', () => {
  it('passes for 4.01', () => {
    const result = validateODataVersionHeader(makeResponse({ headers: { 'odata-version': '4.01' } }));
    expect(result.status).toBe('pass');
  });

  it('passes for 4.0', () => {
    const result = validateODataVersionHeader(makeResponse({ headers: { 'odata-version': '4.0' } }));
    expect(result.status).toBe('pass');
  });

  it('fails for missing header', () => {
    const result = validateODataVersionHeader(makeResponse({ headers: {} }));
    expect(result.status).toBe('fail');
  });
});

describe('validateEntityIdHeader', () => {
  it('skips for non-204 responses', () => {
    const result = validateEntityIdHeader(makeResponse({ status: 201 }));
    expect(result.status).toBe('skip');
  });

  it('passes for 204 with EntityId', () => {
    const result = validateEntityIdHeader(makeResponse({ status: 204, headers: { entityid: '123' } }));
    expect(result.status).toBe('pass');
  });

  it('fails for 204 without EntityId', () => {
    const result = validateEntityIdHeader(makeResponse({ status: 204, headers: {} }));
    expect(result.status).toBe('fail');
  });
});

describe('validateLocationHeader', () => {
  it('passes with valid Location header', () => {
    const results = validateLocationHeader(
      makeResponse({
        headers: { location: "https://api.reso.org/Property('123')" }
      }),
      'Property'
    );
    expect(results).toHaveLength(3);
    expect(results.every(r => r.status === 'pass')).toBe(true);
  });

  it('fails when Location is missing', () => {
    const results = validateLocationHeader(makeResponse({ headers: {} }), 'Property');
    expect(results).toHaveLength(1);
    expect(results[0].status).toBe('fail');
  });

  it('fails when Location is not a valid URL', () => {
    const results = validateLocationHeader(makeResponse({ headers: { location: 'not-a-url' } }), 'Property');
    expect(results[1].status).toBe('fail');
  });
});

describe('validatePreferenceApplied', () => {
  it('passes when preference matches', () => {
    const result = validatePreferenceApplied(
      makeResponse({
        headers: { 'preference-applied': 'return=representation' }
      }),
      'return=representation'
    );
    expect(result.status).toBe('pass');
  });

  it('fails when preference does not match', () => {
    const result = validatePreferenceApplied(
      makeResponse({
        headers: { 'preference-applied': 'return=minimal' }
      }),
      'return=representation'
    );
    expect(result.status).toBe('fail');
  });
});

describe('validateJsonResponse', () => {
  it('passes for object body', () => {
    const result = validateJsonResponse(makeResponse({ body: { a: 1 } }));
    expect(result.status).toBe('pass');
  });

  it('fails for null body', () => {
    const result = validateJsonResponse(makeResponse({ body: null }));
    expect(result.status).toBe('fail');
  });
});

describe('validateEmptyResponse', () => {
  it('passes for empty string body', () => {
    const result = validateEmptyResponse(makeResponse({ rawBody: '' }));
    expect(result.status).toBe('pass');
  });

  it('fails for non-empty body', () => {
    const result = validateEmptyResponse(makeResponse({ rawBody: '{"foo":"bar"}' }));
    expect(result.status).toBe('fail');
  });
});

describe('validateODataAnnotation', () => {
  it('passes when MUST annotation is present and valid URL', () => {
    const results = validateODataAnnotation(
      makeResponse({
        body: { '@odata.editLink': "https://api.reso.org/Property('123')" }
      }),
      '@odata.editLink',
      'MUST'
    );
    expect(results.every(r => r.status === 'pass')).toBe(true);
  });

  it('fails when MUST annotation is missing', () => {
    const results = validateODataAnnotation(makeResponse({ body: {} }), '@odata.editLink', 'MUST');
    expect(results[0].status).toBe('fail');
  });

  it('skips when MAY annotation is missing', () => {
    const results = validateODataAnnotation(makeResponse({ body: {} }), '@odata.context', 'MAY');
    expect(results[0].status).toBe('skip');
  });

  it('validates etag starts with W/', () => {
    const results = validateODataAnnotation(
      makeResponse({
        body: { '@odata.etag': 'W/"abc123"' }
      }),
      '@odata.etag',
      'MUST'
    );
    expect(results.every(r => r.status === 'pass')).toBe(true);
  });

  it('fails when etag does not start with W/', () => {
    const results = validateODataAnnotation(
      makeResponse({
        body: { '@odata.etag': 'abc123' }
      }),
      '@odata.etag',
      'MUST'
    );
    const etagCheck = results.find(r => r.description.includes('starts with'));
    expect(etagCheck?.status).toBe('fail');
  });
});

describe('validateResponseContainsPayload', () => {
  it('passes when all payload fields are present', () => {
    const results = validateResponseContainsPayload(
      { ListPrice: 100, BedroomsTotal: 3, Extra: 'field' },
      { ListPrice: 100, BedroomsTotal: 3 }
    );
    expect(results.every(r => r.status === 'pass')).toBe(true);
  });

  it('fails when a payload field is missing', () => {
    const results = validateResponseContainsPayload({ ListPrice: 100 }, { ListPrice: 100, BedroomsTotal: 3 });
    const bedroomCheck = results.find(r => r.description.includes('BedroomsTotal'));
    expect(bedroomCheck?.status).toBe('fail');
  });

  it('handles array values', () => {
    const results = validateResponseContainsPayload({ AccessibilityFeatures: ['A', 'B'] }, { AccessibilityFeatures: ['A', 'B'] });
    expect(results[0].status).toBe('pass');
  });

  it('ignores @-prefixed keys in payload', () => {
    const results = validateResponseContainsPayload({ ListPrice: 100 }, { '@reso.target': '123', ListPrice: 100 });
    expect(results).toHaveLength(1);
    expect(results[0].status).toBe('pass');
  });
});

describe('validateODataError', () => {
  it('passes for a well-formed error response', () => {
    const results = validateODataError(
      makeResponse({
        body: {
          error: {
            code: '20100',
            message: 'Errors',
            target: 'Create',
            details: [
              {
                code: '30212',
                target: 'ListPrice',
                message: 'List Price must be greater than 0'
              }
            ]
          }
        }
      }),
      sampleEntityType
    );
    expect(results.every(r => r.status === 'pass')).toBe(true);
  });

  it('fails when error object is missing', () => {
    const results = validateODataError(makeResponse({ body: {} }), sampleEntityType);
    expect(results.some(r => r.status === 'fail')).toBe(true);
  });

  it('fails when details target is not in metadata', () => {
    const results = validateODataError(
      makeResponse({
        body: {
          error: {
            code: '20100',
            message: 'Errors',
            details: [
              {
                code: '1',
                target: 'UnknownField',
                message: 'Some error'
              }
            ]
          }
        }
      }),
      sampleEntityType
    );
    const targetCheck = results.find(r => r.description.includes('UnknownField'));
    expect(targetCheck?.status).toBe('fail');
  });
});
