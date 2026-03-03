import { describe, it, expect } from "vitest";
import { validateQueryOptions } from "../src/query/validator.js";
import type { CsdlEntityType } from "../src/csdl/types.js";

const entityType: CsdlEntityType = {
  name: "Property",
  key: ["ListingKey"],
  properties: [
    { name: "ListingKey", type: "Edm.String" },
    { name: "ListPrice", type: "Edm.Decimal" },
    { name: "City", type: "Edm.String" },
    { name: "BedroomsTotal", type: "Edm.Int64" },
  ],
  navigationProperties: [
    {
      name: "Media",
      type: "Collection(org.reso.metadata.Media)",
      isCollection: true,
      entityTypeName: "Media",
    },
    {
      name: "ListAgent",
      type: "org.reso.metadata.Member",
      isCollection: false,
      entityTypeName: "Member",
      nullable: true,
      partner: "Listings",
    },
  ],
};

describe("validateQueryOptions", () => {
  it("accepts empty options", () => {
    const result = validateQueryOptions({}, entityType);
    expect(result.valid).toBe(true);
  });

  it("accepts valid $select", () => {
    const result = validateQueryOptions(
      { $select: "ListPrice,City" },
      entityType,
    );
    expect(result.valid).toBe(true);
  });

  it("rejects unknown $select field", () => {
    const result = validateQueryOptions(
      { $select: "ListPrice,UnknownField" },
      entityType,
    );
    expect(result.valid).toBe(false);
    expect(result.errors[0].option).toBe("$select");
    expect(result.errors[0].message).toContain("UnknownField");
  });

  it("accepts valid $orderby", () => {
    const result = validateQueryOptions(
      { $orderby: "ListPrice desc" },
      entityType,
    );
    expect(result.valid).toBe(true);
  });

  it("rejects unknown $orderby field", () => {
    const result = validateQueryOptions(
      { $orderby: "Unknown asc" },
      entityType,
    );
    expect(result.valid).toBe(false);
    expect(result.errors[0].option).toBe("$orderby");
  });

  it("accepts valid $filter", () => {
    const result = validateQueryOptions(
      { $filter: "ListPrice gt 200000 and City eq 'Austin'" },
      entityType,
    );
    expect(result.valid).toBe(true);
  });

  it("rejects unknown $filter property", () => {
    const result = validateQueryOptions(
      { $filter: "UnknownField eq 'test'" },
      entityType,
    );
    expect(result.valid).toBe(false);
    expect(result.errors[0].option).toBe("$filter");
    expect(result.errors[0].message).toContain("UnknownField");
  });

  it("rejects invalid $filter syntax", () => {
    const result = validateQueryOptions(
      { $filter: "not valid odata at all !!!" },
      entityType,
    );
    expect(result.valid).toBe(false);
    expect(result.errors[0].option).toBe("$filter");
  });

  it("accepts valid $top", () => {
    const result = validateQueryOptions({ $top: 10 }, entityType);
    expect(result.valid).toBe(true);
  });

  it("rejects negative $top", () => {
    const result = validateQueryOptions({ $top: -1 }, entityType);
    expect(result.valid).toBe(false);
    expect(result.errors[0].option).toBe("$top");
  });

  it("accepts valid $skip", () => {
    const result = validateQueryOptions({ $skip: 0 }, entityType);
    expect(result.valid).toBe(true);
  });

  it("rejects negative $skip", () => {
    const result = validateQueryOptions({ $skip: -5 }, entityType);
    expect(result.valid).toBe(false);
    expect(result.errors[0].option).toBe("$skip");
  });

  it("accepts valid $expand with known navigation property", () => {
    const result = validateQueryOptions(
      { $expand: "Media" },
      entityType,
    );
    expect(result.valid).toBe(true);
  });

  it("accepts $expand with multiple navigation properties", () => {
    const result = validateQueryOptions(
      { $expand: "Media,ListAgent" },
      entityType,
    );
    expect(result.valid).toBe(true);
  });

  it("accepts $expand with nested options", () => {
    const result = validateQueryOptions(
      { $expand: "Media($select=MediaURL,MediaKey)" },
      entityType,
    );
    expect(result.valid).toBe(true);
  });

  it("rejects $expand with unknown navigation property", () => {
    const result = validateQueryOptions(
      { $expand: "UnknownNav" },
      entityType,
    );
    expect(result.valid).toBe(false);
    expect(result.errors[0].option).toBe("$expand");
    expect(result.errors[0].message).toContain("UnknownNav");
  });

  it("accumulates multiple errors", () => {
    const result = validateQueryOptions(
      {
        $select: "UnknownA,UnknownB",
        $top: -1,
      },
      entityType,
    );
    expect(result.valid).toBe(false);
    expect(result.errors.length).toBeGreaterThanOrEqual(3);
  });
});
