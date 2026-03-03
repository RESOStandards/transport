import { describe, it, expect } from "vitest";
import { edmTypeToSql, generateCreateTable } from "../src/db/schema-generator.js";
import type { ResoField } from "../src/metadata/types.js";

const makeField = (overrides: Partial<ResoField> & Pick<ResoField, "fieldName" | "type">): ResoField => ({
  resourceName: "Property",
  nullable: true,
  annotations: [],
  ...overrides,
});

describe("edmTypeToSql", () => {
  it("maps Edm.String to TEXT", () => {
    expect(edmTypeToSql(makeField({ fieldName: "City", type: "Edm.String" }))).toBe("TEXT");
  });

  it("maps Edm.String with maxLength to VARCHAR", () => {
    expect(
      edmTypeToSql(makeField({ fieldName: "City", type: "Edm.String", maxLength: 255 })),
    ).toBe("VARCHAR(255)");
  });

  it("maps Edm.Int64 to BIGINT", () => {
    expect(edmTypeToSql(makeField({ fieldName: "Beds", type: "Edm.Int64" }))).toBe("BIGINT");
  });

  it("maps Edm.Decimal with precision/scale", () => {
    expect(
      edmTypeToSql(makeField({ fieldName: "Price", type: "Edm.Decimal", precision: 14, scale: 2 })),
    ).toBe("NUMERIC(14,2)");
  });

  it("maps Edm.Boolean to BOOLEAN", () => {
    expect(edmTypeToSql(makeField({ fieldName: "Active", type: "Edm.Boolean" }))).toBe("BOOLEAN");
  });

  it("maps Edm.DateTimeOffset to TIMESTAMPTZ", () => {
    expect(
      edmTypeToSql(makeField({ fieldName: "ModTime", type: "Edm.DateTimeOffset" })),
    ).toBe("TIMESTAMPTZ");
  });

  it("maps enum types to TEXT", () => {
    expect(
      edmTypeToSql(makeField({ fieldName: "Status", type: "org.reso.metadata.enums.StandardStatus" })),
    ).toBe("TEXT");
  });

  it("maps collection fields to JSONB", () => {
    expect(
      edmTypeToSql(
        makeField({
          fieldName: "Features",
          type: "org.reso.metadata.enums.Features",
          isCollection: true,
        }),
      ),
    ).toBe("JSONB");
  });
});

describe("generateCreateTable", () => {
  it("generates valid DDL with primary key", () => {
    const fields: ResoField[] = [
      makeField({ fieldName: "ListingKey", type: "Edm.String", maxLength: 255 }),
      makeField({ fieldName: "ListPrice", type: "Edm.Decimal", precision: 14, scale: 2 }),
      makeField({ fieldName: "City", type: "Edm.String" }),
    ];

    const ddl = generateCreateTable("Property", "ListingKey", fields);

    expect(ddl).toContain('CREATE TABLE IF NOT EXISTS "Property"');
    expect(ddl).toContain('"ListingKey" VARCHAR(255) NOT NULL PRIMARY KEY');
    expect(ddl).toContain('"ListPrice" NUMERIC(14,2)');
    expect(ddl).toContain('"City" TEXT');
  });
});
