import type { ODataResponse, TestAssertion, TestStatus, EntityType } from "./types.js";

/** Helper to construct a TestAssertion with optional expected/actual/gherkinStep fields. */
const assertion = (
  description: string,
  status: TestStatus,
  opts?: {
    readonly expected?: string;
    readonly actual?: string;
    readonly gherkinStep?: string;
  },
): TestAssertion => ({
  description,
  status,
  ...opts,
});

/** Checks whether a string is a valid URL using the URL constructor. */
const isValidUrl = (value: string): boolean => {
  try {
    new URL(value);
    return true;
  } catch {
    return false;
  }
};

// ── Header Validators ──
// Each function validates a specific response header per the OData 4.01 specification
// and the Gherkin certification scenarios defined in RCP-010 Section 3.

/** Validates the OData-Version response header is "4.0" or "4.01". */
export function validateODataVersionHeader(
  response: ODataResponse,
): TestAssertion {
  const value = response.headers["odata-version"];
  const valid = value === "4.0" || value === "4.01";
  return assertion(
    "OData-Version header is 4.0 or 4.01",
    valid ? "pass" : "fail",
    {
      expected: "4.0 or 4.01",
      actual: value ?? "(missing)",
      gherkinStep:
        'the response header "OData-Version" "equals" one of the following values',
    },
  );
}

/** Validates the response status code is one of the allowed values. */
export function validateStatusCode(
  response: ODataResponse,
  allowed: ReadonlyArray<number>,
): TestAssertion {
  const valid = allowed.includes(response.status);
  return assertion(
    `Status code is one of [${allowed.join(", ")}]`,
    valid ? "pass" : "fail",
    {
      expected: allowed.join(" or "),
      actual: String(response.status),
      gherkinStep: `the server responds with one of the following status codes`,
    },
  );
}

/** Validates the response status code falls within an inclusive range (e.g. 400-499). */
export function validateStatusCodeRange(
  response: ODataResponse,
  min: number,
  max: number,
): TestAssertion {
  const valid = response.status >= min && response.status <= max;
  return assertion(
    `Status code is between ${min} and ${max}`,
    valid ? "pass" : "fail",
    {
      expected: `${min}-${max}`,
      actual: String(response.status),
      gherkinStep: `the server responds with a status code between ${min} and ${max}`,
    },
  );
}

/**
 * Validates the EntityId header is present when the response status is 204.
 * Per OData, EntityId MUST be present on 204 responses for create/update so the
 * client can identify the affected resource. Skipped for non-204 responses.
 */
export function validateEntityIdHeader(
  response: ODataResponse,
): TestAssertion {
  if (response.status !== 204) {
    return assertion(
      "EntityId header check (not applicable for non-204)",
      "skip",
      {
        gherkinStep:
          'the response header "EntityId" "MUST" "be present" if the response code was 204',
      },
    );
  }

  const value = response.headers["entityid"];
  return assertion(
    "EntityId header is present (required for 204)",
    value ? "pass" : "fail",
    {
      expected: "(non-empty)",
      actual: value ?? "(missing)",
      gherkinStep:
        'the response header "EntityId" "MUST" "be present" if the response code was 204',
    },
  );
}

/**
 * Validates the Location response header:
 * 1. Is present
 * 2. Is a valid URL
 * 3. References the resource being edited (contains the resource name)
 *
 * Returns up to 3 assertions. If the header is missing, only the presence check is returned.
 */
export function validateLocationHeader(
  response: ODataResponse,
  resource: string,
): ReadonlyArray<TestAssertion> {
  const value = response.headers["location"];

  const presentAssertion = assertion(
    "Location header is present",
    value ? "pass" : "fail",
    {
      expected: "(present)",
      actual: value ?? "(missing)",
      gherkinStep: 'the response header "Location" "MUST" "be present"',
    },
  );

  if (!value) {
    return [presentAssertion];
  }

  const urlAssertion = assertion(
    "Location header is a valid URL",
    isValidUrl(value) ? "pass" : "fail",
    {
      expected: "(valid URL)",
      actual: value,
      gherkinStep: 'the response header "Location" "is a valid URL"',
    },
  );

  const resourceAssertion = assertion(
    "Location header references the resource",
    value.includes(resource) ? "pass" : "fail",
    {
      expected: `URL containing "${resource}"`,
      actual: value,
      gherkinStep:
        'the response header "Location" "MUST" reference the resource being edited',
    },
  );

  return [presentAssertion, urlAssertion, resourceAssertion];
}

/** Validates the Preference-Applied header matches the expected value (return=representation or return=minimal). */
export function validatePreferenceApplied(
  response: ODataResponse,
  expected: "return=representation" | "return=minimal",
): TestAssertion {
  const value = response.headers["preference-applied"];
  const valid = value === expected;
  return assertion(
    `Preference-Applied header is ${expected}`,
    valid ? "pass" : "fail",
    {
      expected,
      actual: value ?? "(missing)",
      gherkinStep: `the Preference-Applied response header is \`${expected}\``,
    },
  );
}

// ── Body Validators ──
// Each function validates the structure or content of the JSON response body.

/** Validates that the response body was successfully parsed as a JSON object. */
export function validateJsonResponse(
  response: ODataResponse,
): TestAssertion {
  const valid = response.body !== null && typeof response.body === "object";
  return assertion("Response is valid JSON", valid ? "pass" : "fail", {
    expected: "(valid JSON object)",
    actual:
      response.body === null ? "(null/empty)" : typeof response.body,
    gherkinStep: "the response is valid JSON",
  });
}

/** Validates that the response body is empty (used for delete success verification). */
export function validateEmptyResponse(
  response: ODataResponse,
): TestAssertion {
  const empty =
    response.rawBody.trim() === "" ||
    response.rawBody.trim() === "{}" ||
    response.rawBody.trim() === "null";
  return assertion("Response body is empty", empty ? "pass" : "fail", {
    expected: "(empty body)",
    actual:
      response.rawBody.length > 100
        ? `${response.rawBody.slice(0, 100)}...`
        : response.rawBody,
    gherkinStep: 'the JSON response "MUST" be empty',
  });
}

/**
 * Validates an OData annotation in the response body (e.g. @odata.context, @odata.editLink).
 *
 * - For "MUST" annotations: fails if missing
 * - For "MAY" annotations: skips if missing
 * - If present: validates URL format for context/id/editLink, or W/ prefix for etag
 *
 * Returns one or more assertions depending on the annotation type.
 */
export function validateODataAnnotation(
  response: ODataResponse,
  annotationName: string,
  requirement: "MUST" | "MAY",
): ReadonlyArray<TestAssertion> {
  const body = response.body as Record<string, unknown> | null;
  if (!body || typeof body !== "object") {
    if (requirement === "MUST") {
      return [
        assertion(
          `Response contains ${annotationName}`,
          "fail",
          {
            expected: `(${annotationName} present)`,
            actual: "(no response body)",
            gherkinStep: `the JSON response "${requirement}" contain "${annotationName}"`,
          },
        ),
      ];
    }
    return [
      assertion(`Response contains ${annotationName}`, "skip", {
        gherkinStep: `the JSON response "${requirement}" contain "${annotationName}"`,
      }),
    ];
  }

  const value = body[annotationName];
  const results: TestAssertion[] = [];

  if (value === undefined) {
    results.push(
      assertion(
        `Response contains ${annotationName}`,
        requirement === "MUST" ? "fail" : "skip",
        {
          expected: `(${annotationName} present)`,
          actual: "(missing)",
          gherkinStep: `the JSON response "${requirement}" contain "${annotationName}"`,
        },
      ),
    );
    return results;
  }

  results.push(
    assertion(`Response contains ${annotationName}`, "pass", {
      gherkinStep: `the JSON response "${requirement}" contain "${annotationName}"`,
    }),
  );

  // URL validation for context, id, editLink
  if (["@odata.context", "@odata.id", "@odata.editLink"].includes(annotationName)) {
    const strValue = String(value);
    results.push(
      assertion(
        `${annotationName} is a valid URL`,
        isValidUrl(strValue) ? "pass" : "fail",
        {
          expected: "(valid URL)",
          actual: strValue,
          gherkinStep: `the JSON response value "${annotationName}" "is a valid URL"`,
        },
      ),
    );
  }

  // ETag validation
  if (annotationName === "@odata.etag") {
    const strValue = String(value);
    results.push(
      assertion(
        `${annotationName} starts with W/`,
        strValue.startsWith('W/') ? "pass" : "fail",
        {
          expected: 'W/...',
          actual: strValue,
          gherkinStep: `the JSON response value "${annotationName}" "starts with" "W/"`,
        },
      ),
    );
  }

  return results;
}

/**
 * Validates that every field in the request payload appears in the response body with a matching value.
 * Keys prefixed with `@` (OData annotations like @odata.context) are excluded from comparison.
 * Uses deep equality for arrays and objects, with numeric tolerance for decimal representation.
 */
export function validateResponseContainsPayload(
  responseBody: unknown,
  payload: Readonly<Record<string, unknown>>,
): ReadonlyArray<TestAssertion> {
  if (!responseBody || typeof responseBody !== "object") {
    return [
      assertion(
        "Response body contains payload data",
        "fail",
        {
          expected: "(response body with payload fields)",
          actual: "(no response body)",
          gherkinStep: "the JSON response contains the data in the payload",
        },
      ),
    ];
  }

  const body = responseBody as Record<string, unknown>;

  return Object.entries(payload)
    .filter(([key]) => !key.startsWith("@"))
    .map(([key, expectedValue]) => {
      const actualValue = body[key];
      const match = deepEqual(actualValue, expectedValue);
      return assertion(
        `Response contains ${key} matching payload`,
        match ? "pass" : "fail",
        {
          expected: JSON.stringify(expectedValue),
          actual: JSON.stringify(actualValue),
          gherkinStep: `the JSON response "MUST" contain all JSON data in the payload`,
        },
      );
    });
}

// ── Error Response Validators ──

/**
 * Validates the OData error response format per the OData 4.01 specification.
 *
 * Checks:
 * - `error.code` is a non-empty string
 * - `error.message` is a non-empty string
 * - `error.details` is an array
 * - Each detail's `target` references a field that exists in the entity type metadata
 *   (handles nested paths like "Media[1].Category" by extracting the root field name)
 * - Each detail's `message` is a non-empty string
 */
export function validateODataError(
  response: ODataResponse,
  entityType: EntityType,
): ReadonlyArray<TestAssertion> {
  const results: TestAssertion[] = [];
  const body = response.body as Record<string, unknown> | null;

  if (!body || typeof body !== "object") {
    results.push(
      assertion("Error response has body", "fail", {
        expected: "(error response body)",
        actual: "(empty)",
        gherkinStep: "the error response is in a valid format",
      }),
    );
    return results;
  }

  const error = body["error"] as Record<string, unknown> | undefined;
  if (!error || typeof error !== "object") {
    results.push(
      assertion("Error response contains 'error' object", "fail", {
        expected: '{ "error": { ... } }',
        actual: JSON.stringify(body).slice(0, 200),
        gherkinStep: "the error response is in a valid format",
      }),
    );
    return results;
  }

  // error.code
  const code = error["code"];
  results.push(
    assertion(
      "error.code is a non-empty string",
      typeof code === "string" && code.length > 0 ? "pass" : "fail",
      {
        expected: "(non-empty string)",
        actual: JSON.stringify(code),
        gherkinStep: "the error response is in a valid format",
      },
    ),
  );

  // error.message
  const message = error["message"];
  results.push(
    assertion(
      "error.message is a non-empty string",
      typeof message === "string" && message.length > 0 ? "pass" : "fail",
      {
        expected: "(non-empty string)",
        actual: JSON.stringify(message),
        gherkinStep: "the error response is in a valid format",
      },
    ),
  );

  // error.details
  const details = error["details"];
  if (!Array.isArray(details)) {
    results.push(
      assertion("error.details is an array", "fail", {
        expected: "(array)",
        actual: JSON.stringify(details),
        gherkinStep: "the error response is in a valid format",
      }),
    );
    return results;
  }

  results.push(assertion("error.details is an array", "pass"));

  const propertyNames = new Set(entityType.properties.map((p) => p.name));

  for (const detail of details as ReadonlyArray<Record<string, unknown>>) {
    const target = detail["target"];
    const detailMessage = detail["message"];

    // Validate target is a known field (handle nested like "Media[1].Category")
    const rootTarget =
      typeof target === "string" ? target.split("[")[0].split(".")[0] : "";
    results.push(
      assertion(
        `error.details target "${String(target)}" is in metadata`,
        typeof target === "string" && propertyNames.has(rootTarget)
          ? "pass"
          : "fail",
        {
          expected: "(field name from metadata)",
          actual: String(target),
          gherkinStep:
            'the values in the "target" field in the JSON payload "error.details" path are contained within the metadata',
        },
      ),
    );

    results.push(
      assertion(
        `error.details message for "${String(target)}" is non-empty`,
        typeof detailMessage === "string" && detailMessage.length > 0
          ? "pass"
          : "fail",
        {
          expected: "(non-empty string)",
          actual: JSON.stringify(detailMessage),
          gherkinStep:
            'the values in the "message" field in the JSON payload "error.details" path have non-zero length',
        },
      ),
    );
  }

  return results;
}

// ── Utility ──

/**
 * Deep equality comparison supporting objects, arrays, and numbers.
 * Uses Number.EPSILON tolerance for numeric comparisons to handle
 * floating-point representation differences (e.g. 123456.00 vs 123456).
 */
function deepEqual(a: unknown, b: unknown): boolean {
  if (a === b) return true;

  // Numeric comparison with tolerance for decimal representation
  if (typeof a === "number" && typeof b === "number") {
    return Math.abs(a - b) < Number.EPSILON;
  }

  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    return a.every((val, i) => deepEqual(val, b[i]));
  }

  if (
    a !== null &&
    b !== null &&
    typeof a === "object" &&
    typeof b === "object"
  ) {
    const keysA = Object.keys(a as Record<string, unknown>);
    const keysB = Object.keys(b as Record<string, unknown>);
    if (keysA.length !== keysB.length) return false;
    return keysA.every((key) =>
      deepEqual(
        (a as Record<string, unknown>)[key],
        (b as Record<string, unknown>)[key],
      ),
    );
  }

  return false;
}
