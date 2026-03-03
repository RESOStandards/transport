import { describe, it, expect } from "vitest";
import { resolve } from "node:path";
import {
  loadMetadata,
  getFieldsForResource,
  getLookupsForType,
  getKeyFieldForResource,
  isEnumType,
  getStandardName,
} from "../src/metadata/loader.js";

const metadataPath = resolve(import.meta.dirname, "../server-metadata.json");

describe("loadMetadata", () => {
  it("loads and parses the metadata file", async () => {
    const metadata = await loadMetadata(metadataPath);
    expect(metadata.version).toBeTruthy();
    expect(metadata.fields.length).toBeGreaterThan(0);
    expect(metadata.lookups.length).toBeGreaterThan(0);
    expect(metadata.resources.length).toBeGreaterThan(0);
  });
});

describe("getFieldsForResource", () => {
  it("returns fields for Property resource", async () => {
    const metadata = await loadMetadata(metadataPath);
    const fields = getFieldsForResource(metadata, "Property");
    expect(fields.length).toBeGreaterThan(100);
    expect(fields.every((f) => f.resourceName === "Property")).toBe(true);
  });

  it("returns fields for Media resource", async () => {
    const metadata = await loadMetadata(metadataPath);
    const fields = getFieldsForResource(metadata, "Media");
    expect(fields.length).toBeGreaterThan(0);

    const resourceNameField = fields.find((f) => f.fieldName === "ResourceName");
    expect(resourceNameField).toBeDefined();
    expect(resourceNameField!.type).toContain("enums");
  });

  it("returns empty array for unknown resource", async () => {
    const metadata = await loadMetadata(metadataPath);
    const fields = getFieldsForResource(metadata, "NonExistent");
    expect(fields).toHaveLength(0);
  });
});

describe("getLookupsForType", () => {
  it("returns lookup values for an enum type", async () => {
    const metadata = await loadMetadata(metadataPath);
    const lookups = getLookupsForType(metadata, "org.reso.metadata.enums.StandardStatus");
    expect(lookups.length).toBeGreaterThan(0);
    expect(lookups.every((l) => l.lookupName === "org.reso.metadata.enums.StandardStatus")).toBe(true);
  });
});

describe("getKeyFieldForResource", () => {
  it("returns ListingKey for Property", () => {
    expect(getKeyFieldForResource("Property")).toBe("ListingKey");
  });

  it("returns MemberKey for Member", () => {
    expect(getKeyFieldForResource("Member")).toBe("MemberKey");
  });

  it("returns undefined for unknown resource", () => {
    expect(getKeyFieldForResource("Unknown")).toBeUndefined();
  });
});

describe("isEnumType", () => {
  it("returns false for Edm primitives", () => {
    expect(isEnumType("Edm.String")).toBe(false);
    expect(isEnumType("Edm.Int64")).toBe(false);
    expect(isEnumType("Edm.Boolean")).toBe(false);
  });

  it("returns true for enum references", () => {
    expect(isEnumType("org.reso.metadata.enums.StandardStatus")).toBe(true);
  });
});

describe("getStandardName", () => {
  it("returns standard name from annotations", async () => {
    const metadata = await loadMetadata(metadataPath);
    const fields = getFieldsForResource(metadata, "Property");
    const listPrice = fields.find((f) => f.fieldName === "ListPrice");
    expect(listPrice).toBeDefined();
    const name = getStandardName(listPrice!);
    expect(name).toBe("List Price");
  });
});
