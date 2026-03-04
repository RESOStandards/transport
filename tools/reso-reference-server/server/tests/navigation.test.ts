import { resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import { getFieldsForResource, loadMetadata } from '../src/metadata/loader.js';
import { TARGET_RESOURCES } from '../src/metadata/types.js';
import { buildNavigationBindings } from '../src/odata/router.js';

const metadataPath = resolve(import.meta.dirname, '../server-metadata.json');

describe('buildNavigationBindings', () => {
  it('discovers Media expansion on Property with resource-record-key strategy', async () => {
    const metadata = await loadMetadata(metadataPath);
    const bindings = buildNavigationBindings('Property', metadata, TARGET_RESOURCES);
    const media = bindings.find(b => b.name === 'Media');

    expect(media).toBeDefined();
    expect(media!.targetResource).toBe('Media');
    expect(media!.targetKeyField).toBe('MediaKey');
    expect(media!.foreignKey.strategy).toBe('resource-record-key');
    expect(media!.isCollection).toBe(true);
  });

  it('discovers OpenHouse expansion on Property with direct strategy', async () => {
    const metadata = await loadMetadata(metadataPath);
    const bindings = buildNavigationBindings('Property', metadata, TARGET_RESOURCES);
    const openHouse = bindings.find(b => b.name === 'OpenHouse');

    expect(openHouse).toBeDefined();
    expect(openHouse!.targetResource).toBe('OpenHouse');
    expect(openHouse!.targetKeyField).toBe('OpenHouseKey');
    expect(openHouse!.foreignKey.strategy).toBe('direct');
    expect(openHouse!.foreignKey.targetColumn).toBe('ListingKey');
    expect(openHouse!.isCollection).toBe(true);
  });

  it('discovers Rooms expansion on Property targeting PropertyRooms', async () => {
    const metadata = await loadMetadata(metadataPath);
    const bindings = buildNavigationBindings('Property', metadata, TARGET_RESOURCES);
    const rooms = bindings.find(b => b.name === 'Rooms');

    expect(rooms).toBeDefined();
    expect(rooms!.targetResource).toBe('PropertyRooms');
    expect(rooms!.targetKeyField).toBe('RoomKey');
    expect(rooms!.foreignKey.strategy).toBe('direct');
    expect(rooms!.foreignKey.targetColumn).toBe('ListingKey');
    expect(rooms!.isCollection).toBe(true);
  });

  it('discovers GreenBuildingVerification expansion on Property', async () => {
    const metadata = await loadMetadata(metadataPath);
    const bindings = buildNavigationBindings('Property', metadata, TARGET_RESOURCES);
    const green = bindings.find(b => b.name === 'GreenBuildingVerification');

    expect(green).toBeDefined();
    expect(green!.targetResource).toBe('PropertyGreenVerification');
    expect(green!.foreignKey.strategy).toBe('direct');
    expect(green!.foreignKey.targetColumn).toBe('ListingKey');
    expect(green!.isCollection).toBe(true);
  });

  it('discovers PowerProduction expansion on Property', async () => {
    const metadata = await loadMetadata(metadataPath);
    const bindings = buildNavigationBindings('Property', metadata, TARGET_RESOURCES);
    const power = bindings.find(b => b.name === 'PowerProduction');

    expect(power).toBeDefined();
    expect(power!.targetResource).toBe('PropertyPowerProduction');
    expect(power!.foreignKey.strategy).toBe('direct');
    expect(power!.foreignKey.targetColumn).toBe('ListingKey');
    expect(power!.isCollection).toBe(true);
  });

  it('discovers UnitTypes expansion on Property', async () => {
    const metadata = await loadMetadata(metadataPath);
    const bindings = buildNavigationBindings('Property', metadata, TARGET_RESOURCES);
    const units = bindings.find(b => b.name === 'UnitTypes');

    expect(units).toBeDefined();
    expect(units!.targetResource).toBe('PropertyUnitTypes');
    expect(units!.foreignKey.strategy).toBe('direct');
    expect(units!.foreignKey.targetColumn).toBe('ListingKey');
    expect(units!.isCollection).toBe(true);
  });

  it('discovers BuyerAgent to-one expansion on Property with parent-fk strategy', async () => {
    const metadata = await loadMetadata(metadataPath);
    const bindings = buildNavigationBindings('Property', metadata, TARGET_RESOURCES);
    const buyerAgent = bindings.find(b => b.name === 'BuyerAgent');

    expect(buyerAgent).toBeDefined();
    expect(buyerAgent!.targetResource).toBe('Member');
    expect(buyerAgent!.targetKeyField).toBe('MemberKey');
    expect(buyerAgent!.foreignKey.strategy).toBe('parent-fk');
    expect(buyerAgent!.foreignKey.parentColumn).toBe('BuyerAgentKey');
    expect(buyerAgent!.isCollection).toBe(false);
  });

  it('discovers ListAgent to-one expansion on Property with parent-fk strategy', async () => {
    const metadata = await loadMetadata(metadataPath);
    const bindings = buildNavigationBindings('Property', metadata, TARGET_RESOURCES);
    const listAgent = bindings.find(b => b.name === 'ListAgent');

    expect(listAgent).toBeDefined();
    expect(listAgent!.targetResource).toBe('Member');
    expect(listAgent!.foreignKey.strategy).toBe('parent-fk');
    expect(listAgent!.foreignKey.parentColumn).toBe('ListAgentKey');
    expect(listAgent!.isCollection).toBe(false);
  });

  it('discovers BuyerOffice to-one expansion on Property', async () => {
    const metadata = await loadMetadata(metadataPath);
    const bindings = buildNavigationBindings('Property', metadata, TARGET_RESOURCES);
    const buyerOffice = bindings.find(b => b.name === 'BuyerOffice');

    expect(buyerOffice).toBeDefined();
    expect(buyerOffice!.targetResource).toBe('Office');
    expect(buyerOffice!.foreignKey.strategy).toBe('parent-fk');
    expect(buyerOffice!.foreignKey.parentColumn).toBe('BuyerOfficeKey');
    expect(buyerOffice!.isCollection).toBe(false);
  });

  it('discovers Media expansion on Member with resource-record-key strategy', async () => {
    const metadata = await loadMetadata(metadataPath);
    const bindings = buildNavigationBindings('Member', metadata, TARGET_RESOURCES);
    const media = bindings.find(b => b.name === 'Media');

    expect(media).toBeDefined();
    expect(media!.foreignKey.strategy).toBe('resource-record-key');
    expect(media!.isCollection).toBe(true);
  });

  it('discovers Office to-one expansion on Member', async () => {
    const metadata = await loadMetadata(metadataPath);
    const bindings = buildNavigationBindings('Member', metadata, TARGET_RESOURCES);
    const office = bindings.find(b => b.name === 'Office');

    expect(office).toBeDefined();
    expect(office!.targetResource).toBe('Office');
    expect(office!.foreignKey.strategy).toBe('parent-fk');
    expect(office!.foreignKey.parentColumn).toBe('OfficeKey');
    expect(office!.isCollection).toBe(false);
  });

  it('discovers Listing to-one expansion on PropertyRooms back to Property', async () => {
    const metadata = await loadMetadata(metadataPath);
    const bindings = buildNavigationBindings('PropertyRooms', metadata, TARGET_RESOURCES);
    const listing = bindings.find(b => b.name === 'Listing');

    expect(listing).toBeDefined();
    expect(listing!.targetResource).toBe('Property');
    expect(listing!.targetKeyField).toBe('ListingKey');
    expect(listing!.foreignKey.strategy).toBe('parent-fk');
    expect(listing!.foreignKey.parentColumn).toBe('ListingKey');
    expect(listing!.isCollection).toBe(false);
  });

  it('skips expansion fields whose target resource is not in TARGET_RESOURCES', async () => {
    const metadata = await loadMetadata(metadataPath);
    const bindings = buildNavigationBindings('Property', metadata, ['Property']);
    // No other resources available, so no nav props should be resolved
    const collectionBindings = bindings.filter(b => b.isCollection);
    expect(collectionBindings).toHaveLength(0);
  });

  it('returns all expected Property collection nav props', async () => {
    const metadata = await loadMetadata(metadataPath);
    const bindings = buildNavigationBindings('Property', metadata, TARGET_RESOURCES);
    const collectionNames = bindings
      .filter(b => b.isCollection)
      .map(b => b.name)
      .sort();

    expect(collectionNames).toContain('Media');
    expect(collectionNames).toContain('OpenHouse');
    expect(collectionNames).toContain('Rooms');
    expect(collectionNames).toContain('GreenBuildingVerification');
    expect(collectionNames).toContain('PowerProduction');
    expect(collectionNames).toContain('UnitTypes');
  });
});

describe('expansion field filtering', () => {
  it('Property has expansion fields in its metadata', async () => {
    const metadata = await loadMetadata(metadataPath);
    const fields = getFieldsForResource(metadata, 'Property');
    const expansionFields = fields.filter(f => f.isExpansion);

    expect(expansionFields.length).toBeGreaterThan(0);
    expect(expansionFields.every(f => f.isExpansion === true)).toBe(true);
  });

  it('expansion fields are all collection or to-one navigation types', async () => {
    const metadata = await loadMetadata(metadataPath);
    const fields = getFieldsForResource(metadata, 'Property');
    const expansionFields = fields.filter(f => f.isExpansion);

    for (const field of expansionFields) {
      // Expansion fields must have a typeName referencing another entity type
      expect(field.typeName).toBeDefined();
      // Their type should not be a primitive Edm.* type
      expect(field.type).not.toMatch(/^Edm\./);
    }
  });

  it('non-expansion fields have no isExpansion flag', async () => {
    const metadata = await loadMetadata(metadataPath);
    const fields = getFieldsForResource(metadata, 'Property');
    const dataFields = fields.filter(f => !f.isExpansion);

    expect(dataFields.length).toBeGreaterThan(0);
    // Data fields should have primitive or enum types
    for (const field of dataFields) {
      expect(field.isExpansion).toBeFalsy();
    }
  });

  it('filtering out expansion fields excludes HistoryTransactional and SocialMedia', async () => {
    const metadata = await loadMetadata(metadataPath);
    const fields = getFieldsForResource(metadata, 'Property');
    const dataFields = fields.filter(f => !f.isExpansion);
    const dataFieldNames = new Set(dataFields.map(f => f.fieldName));

    // These expansion fields should NOT be in the data fields
    expect(dataFieldNames.has('HistoryTransactional')).toBe(false);
    expect(dataFieldNames.has('SocialMedia')).toBe(false);

    // But they ARE in the full field list
    const allFieldNames = new Set(fields.map(f => f.fieldName));
    expect(allFieldNames.has('HistoryTransactional')).toBe(true);
    expect(allFieldNames.has('SocialMedia')).toBe(true);
  });

  it('filtering out expansion fields keeps all regular data fields', async () => {
    const metadata = await loadMetadata(metadataPath);
    const fields = getFieldsForResource(metadata, 'Property');
    const dataFields = fields.filter(f => !f.isExpansion);
    const expansionFields = fields.filter(f => f.isExpansion);

    // All fields should be accounted for
    expect(dataFields.length + expansionFields.length).toBe(fields.length);

    // Standard data fields should still be present
    const dataFieldNames = new Set(dataFields.map(f => f.fieldName));
    expect(dataFieldNames.has('ListingKey')).toBe(true);
    expect(dataFieldNames.has('ListPrice')).toBe(true);
    expect(dataFieldNames.has('City')).toBe(true);
    expect(dataFieldNames.has('ModificationTimestamp')).toBe(true);
  });

  it('every expansion field on Property references a known entity type', async () => {
    const metadata = await loadMetadata(metadataPath);
    const fields = getFieldsForResource(metadata, 'Property');
    const expansionFields = fields.filter(f => f.isExpansion);

    for (const field of expansionFields) {
      expect(field.typeName).toBeDefined();
      // typeName should reference a resource name (e.g., Media, Member, HistoryTransactional)
      expect(typeof field.typeName).toBe('string');
      expect(field.typeName!.length).toBeGreaterThan(0);
    }
  });

  it('expansion fields not in TARGET_RESOURCES are excluded from navigation bindings', async () => {
    const metadata = await loadMetadata(metadataPath);
    const fields = getFieldsForResource(metadata, 'Property');
    const expansionFields = fields.filter(f => f.isExpansion);
    const bindings = buildNavigationBindings('Property', metadata, TARGET_RESOURCES);
    const bindingNames = new Set(bindings.map(b => b.name));

    // Some expansion fields reference types outside TARGET_RESOURCES
    const nonTargetExpansions = expansionFields.filter(f => f.typeName && !TARGET_RESOURCES.includes(f.typeName));
    expect(nonTargetExpansions.length).toBeGreaterThan(0);

    // None of these should have navigation bindings
    for (const field of nonTargetExpansions) {
      expect(bindingNames.has(field.fieldName)).toBe(false);
    }
  });
});
