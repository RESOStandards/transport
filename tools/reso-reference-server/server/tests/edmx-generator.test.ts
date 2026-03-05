import { resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import { generateEdmx } from '../src/metadata/edmx-generator.js';
import { loadMetadata } from '../src/metadata/loader.js';

const metadataPath = resolve(import.meta.dirname, '../server-metadata.json');

describe('generateEdmx', () => {
  it('generates valid EDMX XML with proper structure', async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property']);

    expect(edmx).toContain('<?xml version="1.0"');
    expect(edmx).toContain('edmx:Edmx Version="4.0"');
    expect(edmx).toContain('edmx:DataServices');
    expect(edmx).toContain('Schema Namespace="org.reso.metadata"');
    expect(edmx).toContain('EntityType Name="Property"');
    expect(edmx).toContain('PropertyRef Name="ListingKey"');
    expect(edmx).toContain('Property Name="ListPrice"');
  });

  it('includes multiple entity types when given multiple resources', async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property', 'Member', 'Office']);

    expect(edmx).toContain('EntityType Name="Property"');
    expect(edmx).toContain('EntityType Name="Member"');
    expect(edmx).toContain('EntityType Name="Office"');
    expect(edmx).toContain('PropertyRef Name="ListingKey"');
    expect(edmx).toContain('PropertyRef Name="MemberKey"');
    expect(edmx).toContain('PropertyRef Name="OfficeKey"');
  });

  it('maps enum fields to Edm.String', async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property']);

    // StandardStatus is an enum field, should be mapped to Edm.String
    expect(edmx).toMatch(/Property Name="StandardStatus" Type="Edm\.String"/);
  });

  it('maps collection fields to Collection(Edm.String) with Nullable="false"', async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property']);

    // Collection fields return [] not null, so must be Nullable="false"
    expect(edmx).toMatch(/Property Name="AccessibilityFeatures" Type="Collection\(Edm\.String\)" Nullable="false"/);
  });

  it('omits Nullable="true" since it is the OData default', async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property']);

    // Nullable="true" should never appear — it's the default per the spec
    expect(edmx).not.toContain('Nullable="true"');
  });

  it('includes lookup annotations for enum fields', async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property']);

    expect(edmx).toContain('RESO.OData.Metadata.LookupName');
  });

  it('generates NavigationProperty elements for expansion fields', async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property', 'Media', 'PropertyRooms']);

    // Property should have NavigationProperty for Media (collection)
    expect(edmx).toMatch(/NavigationProperty Name="Media" Type="Collection\(org\.reso\.metadata\.Media\)"/);

    // Property should have NavigationProperty for Rooms (collection of PropertyRooms)
    expect(edmx).toMatch(/NavigationProperty Name="Rooms" Type="Collection\(org\.reso\.metadata\.PropertyRooms\)"/);
  });

  it('generates to-one NavigationProperty for single-valued expansions', async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property', 'Member']);

    // BuyerAgent is a to-one nav prop targeting Member
    expect(edmx).toMatch(/NavigationProperty Name="BuyerAgent" Type="org\.reso\.metadata\.Member"/);
    expect(edmx).toMatch(/NavigationProperty Name="ListAgent" Type="org\.reso\.metadata\.Member"/);
  });

  it('generates NavigationProperty for child resource back-references', async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property', 'PropertyRooms']);

    // PropertyRooms should have Listing nav prop back to Property
    expect(edmx).toMatch(/NavigationProperty Name="Listing" Type="org\.reso\.metadata\.Property"/);
  });

  it('generates EntityContainer with EntitySet for each resource', async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property', 'Member', 'Office']);

    expect(edmx).toContain('EntityContainer Name="Default"');
    expect(edmx).toContain('EntitySet Name="Property" EntityType="org.reso.metadata.Property"');
    expect(edmx).toContain('EntitySet Name="Member" EntityType="org.reso.metadata.Member"');
    expect(edmx).toContain('EntitySet Name="Office" EntityType="org.reso.metadata.Office"');
  });

  it('generates NavigationPropertyBinding in EntitySets for expansion fields', async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property', 'Media', 'Member']);

    // Property EntitySet should bind Media and BuyerAgent nav props
    expect(edmx).toContain('NavigationPropertyBinding Path="Media" Target="Media"');
    expect(edmx).toContain('NavigationPropertyBinding Path="BuyerAgent" Target="Member"');
  });

  it('omits NavigationPropertyBinding for targets not in the resource list', async () => {
    const metadata = await loadMetadata(metadataPath);
    // Media is NOT included — Property's Media binding should be omitted
    const edmx = generateEdmx(metadata, ['Property', 'Member']);

    expect(edmx).not.toContain('NavigationPropertyBinding Path="Media"');
    expect(edmx).toContain('NavigationPropertyBinding Path="BuyerAgent" Target="Member"');
  });

  it('can be parsed by fast-xml-parser with the same options as the test tool', async () => {
    // Dynamic import to avoid hard dependency
    const { XMLParser } = await import('fast-xml-parser');

    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property']);

    const parser = new XMLParser({
      ignoreAttributes: false,
      attributeNamePrefix: '@_',
      isArray: (name: string) => ['EntityType', 'Property', 'PropertyRef', 'Annotation'].includes(name)
    });

    const parsed = parser.parse(edmx);
    const schema = parsed?.['edmx:Edmx']?.['edmx:DataServices']?.Schema;
    expect(schema).toBeDefined();
    expect(schema['@_Namespace']).toBe('org.reso.metadata');

    const entityTypes = schema.EntityType;
    expect(Array.isArray(entityTypes)).toBe(true);
    expect(entityTypes.length).toBeGreaterThan(0);

    const property = entityTypes.find((et: Record<string, unknown>) => et['@_Name'] === 'Property');
    expect(property).toBeDefined();

    const keyRefs = property.Key.PropertyRef;
    expect(Array.isArray(keyRefs)).toBe(true);
    expect(keyRefs[0]['@_Name']).toBe('ListingKey');

    const properties = property.Property;
    expect(Array.isArray(properties)).toBe(true);
    expect(properties.length).toBeGreaterThan(100);
  });
});

describe('generateEdmx (enum-type mode)', () => {
  it('maps enum fields to fully-qualified type names', async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property'], 'enum-type');

    expect(edmx).toMatch(/Property Name="StandardStatus" Type="org\.reso\.metadata\.enums\.StandardStatus"/);
  });

  it('maps collection enum fields to Collection(fully-qualified) with Nullable="false"', async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property'], 'enum-type');

    expect(edmx).toMatch(
      /Property Name="AccessibilityFeatures" Type="Collection\(org\.reso\.metadata\.enums\.AccessibilityFeatures\)" Nullable="false"/
    );
  });

  it('does not include LookupName annotations', async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property'], 'enum-type');

    expect(edmx).not.toContain('RESO.OData.Metadata.LookupName');
  });

  it('generates a second Schema block with EnumType definitions', async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property'], 'enum-type');

    expect(edmx).toContain('Schema Namespace="org.reso.metadata.enums"');
    expect(edmx).toContain('EnumType Name="StandardStatus"');
    expect(edmx).toMatch(/<Member Name="Active" Value="\d+"/);
  });

  it('EnumType members have sequential integer values starting from 0', async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property'], 'enum-type');

    // Extract StandardStatus EnumType block and check sequential values
    const enumMatch = edmx.match(/<EnumType Name="StandardStatus">([\s\S]*?)<\/EnumType>/);
    expect(enumMatch).toBeTruthy();
    const memberMatches = [...enumMatch![1].matchAll(/Value="(\d+)"/g)];
    expect(memberMatches.length).toBeGreaterThan(0);
    for (let i = 0; i < memberMatches.length; i++) {
      expect(memberMatches[i][1]).toBe(String(i));
    }
  });

  it('only includes enum types referenced by active resources', async () => {
    const metadata = await loadMetadata(metadataPath);
    // Only include Member — should not include Property-specific enums
    const edmx = generateEdmx(metadata, ['Member'], 'enum-type');

    expect(edmx).toContain('EnumType Name="MemberStatus"');
    // PropertyType is only used by Property, not Member
    expect(edmx).not.toContain('EnumType Name="PropertyType"');
  });

  it('non-enum fields remain unchanged', async () => {
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property'], 'enum-type');

    expect(edmx).toMatch(/Property Name="ListPrice" Type="Edm\.Decimal"/);
    expect(edmx).toMatch(/Property Name="ListingKey" Type="Edm\.String"/);
  });

  it('can be parsed by fast-xml-parser', async () => {
    const { XMLParser } = await import('fast-xml-parser');
    const metadata = await loadMetadata(metadataPath);
    const edmx = generateEdmx(metadata, ['Property'], 'enum-type');

    const parser = new XMLParser({
      ignoreAttributes: false,
      attributeNamePrefix: '@_',
      isArray: (name: string) => ['Schema', 'EntityType', 'EnumType', 'Property', 'PropertyRef', 'Annotation', 'Member'].includes(name)
    });

    const parsed = parser.parse(edmx);
    const schemas = parsed?.['edmx:Edmx']?.['edmx:DataServices']?.Schema;
    expect(Array.isArray(schemas)).toBe(true);
    expect(schemas.length).toBe(2);

    // First schema: entity types
    expect(schemas[0]['@_Namespace']).toBe('org.reso.metadata');
    expect(schemas[0].EntityType).toBeDefined();

    // Second schema: enum types
    expect(schemas[1]['@_Namespace']).toBe('org.reso.metadata.enums');
    expect(schemas[1].EnumType).toBeDefined();
    expect(schemas[1].EnumType.length).toBeGreaterThan(0);

    const statusEnum = schemas[1].EnumType.find((e: Record<string, unknown>) => e['@_Name'] === 'StandardStatus');
    expect(statusEnum).toBeDefined();
    expect(statusEnum.Member.length).toBeGreaterThan(0);
    expect(statusEnum.Member[0]['@_Name']).toBeDefined();
    expect(statusEnum.Member[0]['@_Value']).toBeDefined();
  });

  it('string mode (default) is unchanged', async () => {
    const metadata = await loadMetadata(metadataPath);
    // No third argument — defaults to 'string'
    const edmx = generateEdmx(metadata, ['Property']);

    expect(edmx).toMatch(/Property Name="StandardStatus" Type="Edm\.String"/);
    expect(edmx).toContain('RESO.OData.Metadata.LookupName');
    expect(edmx).not.toContain('EnumType');
    expect(edmx).not.toContain('org.reso.metadata.enums');
  });
});
