import { describe, it, expect } from "vitest";
import { extractAnnotations, isODataCollection, extractEntityData } from "../src/response/parser.js";
import { isODataError, parseODataError, getErrorTargets } from "../src/response/error.js";
import type { ODataResponse, ODataErrorBody } from "../src/types.js";

describe("extractAnnotations", () => {
  it("extracts all annotations", () => {
    const entity = {
      "@odata.context": "http://localhost:8080/$metadata#Property/$entity",
      "@odata.id": "Property('abc')",
      "@odata.editLink": "Property('abc')",
      "@odata.etag": 'W/"abc123"',
      ListPrice: 250000,
    };
    const annotations = extractAnnotations(entity);
    expect(annotations.context).toBe(
      "http://localhost:8080/$metadata#Property/$entity",
    );
    expect(annotations.id).toBe("Property('abc')");
    expect(annotations.editLink).toBe("Property('abc')");
    expect(annotations.etag).toBe('W/"abc123"');
  });

  it("returns empty object for no annotations", () => {
    const annotations = extractAnnotations({ ListPrice: 250000 });
    expect(annotations.context).toBeUndefined();
    expect(annotations.id).toBeUndefined();
  });
});

describe("isODataCollection", () => {
  it("returns true for collection", () => {
    expect(isODataCollection({ value: [{ id: 1 }] })).toBe(true);
  });

  it("returns false for non-collection", () => {
    expect(isODataCollection({ ListPrice: 250000 })).toBe(false);
  });

  it("returns false for null", () => {
    expect(isODataCollection(null)).toBe(false);
  });
});

describe("extractEntityData", () => {
  it("strips OData annotations", () => {
    const entity = {
      "@odata.context": "...",
      "@odata.id": "...",
      ListPrice: 250000,
      City: "Austin",
    };
    const data = extractEntityData(entity);
    expect(data).toEqual({ ListPrice: 250000, City: "Austin" });
    expect(data["@odata.context"]).toBeUndefined();
  });
});

describe("isODataError", () => {
  it("returns true for OData error body", () => {
    expect(
      isODataError({
        error: { code: "400", message: "Bad request" },
      }),
    ).toBe(true);
  });

  it("returns true with details", () => {
    expect(
      isODataError({
        error: {
          code: "400",
          message: "Validation failed",
          details: [{ target: "City", message: "Required" }],
        },
      }),
    ).toBe(true);
  });

  it("returns false for non-error", () => {
    expect(isODataError({ ListPrice: 250000 })).toBe(false);
    expect(isODataError(null)).toBe(false);
    expect(isODataError("string")).toBe(false);
  });
});

describe("parseODataError", () => {
  it("parses error from response", () => {
    const response: ODataResponse = {
      status: 400,
      headers: {},
      body: {
        error: {
          code: "400",
          message: "Validation failed",
          details: [{ target: "ListPrice", message: "Must be positive" }],
        },
      },
      rawBody: "{}",
    };
    const error = parseODataError(response);
    expect(error).not.toBeNull();
    expect(error?.error.code).toBe("400");
  });

  it("returns null for non-error response", () => {
    const response: ODataResponse = {
      status: 200,
      headers: {},
      body: { ListPrice: 250000 },
      rawBody: "{}",
    };
    expect(parseODataError(response)).toBeNull();
  });
});

describe("getErrorTargets", () => {
  it("extracts targets from error details", () => {
    const error: ODataErrorBody = {
      error: {
        code: "400",
        message: "Validation failed",
        details: [
          { target: "ListPrice", message: "Must be positive" },
          { target: "City", message: "Required" },
        ],
      },
    };
    expect(getErrorTargets(error)).toEqual(["ListPrice", "City"]);
  });

  it("returns empty array for no details", () => {
    const error: ODataErrorBody = {
      error: { code: "400", message: "Bad" },
    };
    expect(getErrorTargets(error)).toEqual([]);
  });
});
