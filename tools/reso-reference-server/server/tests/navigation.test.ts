import { resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import { loadMetadata } from '../src/metadata/loader.js';
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
