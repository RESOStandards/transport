import { describe, it, expect } from "vitest";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import {
  parseMetadataXml,
  getEntityType,
  validatePayloadAgainstMetadata,
} from "../src/lib/metadata.js";

const sampleXml = readFileSync(
  resolve(import.meta.dirname, "../sample-metadata.xml"),
  "utf-8",
);

describe("parseMetadataXml", () => {
  it("parses the sample metadata namespace", () => {
    const result = parseMetadataXml(sampleXml);
    expect(result.namespace).toBe("org.reso.metadata");
  });

  it("extracts both entity types", () => {
    const result = parseMetadataXml(sampleXml);
    expect(result.entityTypes).toHaveLength(2);
    expect(result.entityTypes.map((et) => et.name)).toEqual([
      "Property",
      "Lookup",
    ]);
  });

  it("extracts Property key properties", () => {
    const result = parseMetadataXml(sampleXml);
    const property = getEntityType(result, "Property");
    expect(property).toBeDefined();
    expect(property!.keyProperties).toEqual(["ListingKey"]);
  });

  it("extracts Property fields with correct types", () => {
    const result = parseMetadataXml(sampleXml);
    const property = getEntityType(result, "Property")!;

    const listPrice = property.properties.find((p) => p.name === "ListPrice");
    expect(listPrice).toBeDefined();
    expect(listPrice!.type).toBe("Edm.Decimal");
    expect(listPrice!.precision).toBe(14);
    expect(listPrice!.scale).toBe(2);

    const bedrooms = property.properties.find(
      (p) => p.name === "BedroomsTotal",
    );
    expect(bedrooms).toBeDefined();
    expect(bedrooms!.type).toBe("Edm.Int64");

    const accessibility = property.properties.find(
      (p) => p.name === "AccessibilityFeatures",
    );
    expect(accessibility).toBeDefined();
    expect(accessibility!.type).toBe("Collection(Edm.String)");
  });

  it("extracts annotations on properties", () => {
    const result = parseMetadataXml(sampleXml);
    const property = getEntityType(result, "Property")!;

    const status = property.properties.find(
      (p) => p.name === "StandardStatus",
    );
    expect(status).toBeDefined();
    expect(status!.annotations).toBeDefined();
    expect(status!.annotations!["RESO.OData.Metadata.LookupName"]).toBe(
      "StandardStatus",
    );
  });

  it("extracts Lookup entity type with nullable info", () => {
    const result = parseMetadataXml(sampleXml);
    const lookup = getEntityType(result, "Lookup");
    expect(lookup).toBeDefined();
    expect(lookup!.keyProperties).toEqual(["LookupKey"]);

    const lookupKey = lookup!.properties.find((p) => p.name === "LookupKey");
    expect(lookupKey!.nullable).toBe(false);

    const standardValue = lookup!.properties.find(
      (p) => p.name === "StandardLookupValue",
    );
    expect(standardValue!.nullable).toBeUndefined();
  });
});

describe("getEntityType", () => {
  it("returns undefined for unknown resource", () => {
    const result = parseMetadataXml(sampleXml);
    expect(getEntityType(result, "NonExistent")).toBeUndefined();
  });
});

describe("validatePayloadAgainstMetadata", () => {
  const metadata = parseMetadataXml(sampleXml);
  const entityType = getEntityType(metadata, "Property")!;

  it("validates a payload with known fields", () => {
    const result = validatePayloadAgainstMetadata(
      { ListPrice: 100, BedroomsTotal: 3 },
      entityType,
    );
    expect(result.valid).toBe(true);
    expect(result.unknownFields).toHaveLength(0);
  });

  it("reports unknown fields", () => {
    const result = validatePayloadAgainstMetadata(
      { ListPrice: 100, FakeField: "oops" },
      entityType,
    );
    expect(result.valid).toBe(false);
    expect(result.unknownFields).toContain("FakeField");
  });

  it("ignores @-prefixed keys (OData annotations)", () => {
    const result = validatePayloadAgainstMetadata(
      { "@reso.target": "12345", ListPrice: 100 },
      entityType,
    );
    expect(result.valid).toBe(true);
    expect(result.unknownFields).toHaveLength(0);
  });
});
