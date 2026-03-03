import { describe, it, expect } from "vitest";
import { resolve } from "node:path";
import { loadMetadata } from "../src/metadata/loader.js";
import { generateEdmx } from "../src/metadata/edmx-generator.js";

const metadataPath = resolve(import.meta.dirname, "../server-metadata.json");

describe("generateEdmx", () => {
  it("generates valid EDMX XML with proper structure", async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ["Property"]);

    expect(edmx).toContain('<?xml version="1.0"');
    expect(edmx).toContain('edmx:Edmx Version="4.0"');
    expect(edmx).toContain("edmx:DataServices");
    expect(edmx).toContain('Schema Namespace="org.reso.metadata"');
    expect(edmx).toContain('EntityType Name="Property"');
    expect(edmx).toContain('PropertyRef Name="ListingKey"');
    expect(edmx).toContain('Property Name="ListPrice"');
  });

  it("includes multiple entity types when given multiple resources", async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ["Property", "Member", "Office"]);

    expect(edmx).toContain('EntityType Name="Property"');
    expect(edmx).toContain('EntityType Name="Member"');
    expect(edmx).toContain('EntityType Name="Office"');
    expect(edmx).toContain('PropertyRef Name="ListingKey"');
    expect(edmx).toContain('PropertyRef Name="MemberKey"');
    expect(edmx).toContain('PropertyRef Name="OfficeKey"');
  });

  it("maps enum fields to Edm.String", async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ["Property"]);

    // StandardStatus is an enum field, should be mapped to Edm.String
    expect(edmx).toMatch(/Property Name="StandardStatus" Type="Edm\.String"/);
  });

  it("maps collection fields to Collection(Edm.String)", async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ["Property"]);

    // AccessibilityFeatures is a collection field
    expect(edmx).toMatch(
      /Property Name="AccessibilityFeatures" Type="Collection\(Edm\.String\)"/,
    );
  });

  it("includes lookup annotations for enum fields", async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ["Property"]);

    expect(edmx).toContain("RESO.OData.Metadata.LookupName");
  });

  it("can be parsed by fast-xml-parser with the same options as the test tool", async () => {
    // Dynamic import to avoid hard dependency
    const { XMLParser } = await import("fast-xml-parser");

    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ["Property"]);

    const parser = new XMLParser({
      ignoreAttributes: false,
      attributeNamePrefix: "@_",
      isArray: (name: string) =>
        ["EntityType", "Property", "PropertyRef", "Annotation"].includes(name),
    });

    const parsed = parser.parse(edmx);
    const schema = parsed?.["edmx:Edmx"]?.["edmx:DataServices"]?.["Schema"];
    expect(schema).toBeDefined();
    expect(schema["@_Namespace"]).toBe("org.reso.metadata");

    const entityTypes = schema["EntityType"];
    expect(Array.isArray(entityTypes)).toBe(true);
    expect(entityTypes.length).toBeGreaterThan(0);

    const property = entityTypes.find(
      (et: Record<string, unknown>) => et["@_Name"] === "Property",
    );
    expect(property).toBeDefined();

    const keyRefs = property["Key"]["PropertyRef"];
    expect(Array.isArray(keyRefs)).toBe(true);
    expect(keyRefs[0]["@_Name"]).toBe("ListingKey");

    const properties = property["Property"];
    expect(Array.isArray(properties)).toBe(true);
    expect(properties.length).toBeGreaterThan(100);
  });
});
