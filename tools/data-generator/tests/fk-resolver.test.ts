import { describe, expect, it } from 'vitest';
import { buildDependencyGraph, buildMultiResourcePlan, discoverForeignKeys, topologicalSort } from '../src/fk-resolver.js';
import type { ResoField } from '../src/generators/types.js';
import { KEY_FIELD_MAP } from '../src/generators/types.js';

/** Helper to create a regular field. */
const makeField = (resourceName: string, fieldName: string, type = 'Edm.String'): ResoField => ({
  resourceName,
  fieldName,
  type,
  nullable: true,
  annotations: []
});

/** Helper to create a to-one expansion field. */
const makeExpansion = (resourceName: string, fieldName: string, typeName: string): ResoField => ({
  resourceName,
  fieldName,
  type: `org.reso.metadata.${typeName}`,
  typeName,
  nullable: true,
  isExpansion: true,
  isCollection: false,
  annotations: []
});

/** Helper to create a to-many collection expansion field. */
const makeCollectionExpansion = (resourceName: string, fieldName: string, typeName: string): ResoField => ({
  resourceName,
  fieldName,
  type: `org.reso.metadata.${typeName}`,
  typeName,
  nullable: true,
  isExpansion: true,
  isCollection: true,
  annotations: []
});

// Realistic field sets for testing
const PROPERTY_FIELDS: ReadonlyArray<ResoField> = [
  makeField('Property', 'ListingKey'),
  makeField('Property', 'ListPrice', 'Edm.Decimal'),
  makeField('Property', 'ListAgentKey'),
  makeField('Property', 'BuyerAgentKey'),
  makeField('Property', 'ListOfficeKey'),
  makeField('Property', 'BuyerOfficeKey'),
  makeField('Property', 'ListTeamKey'),
  makeExpansion('Property', 'ListAgent', 'Member'),
  makeExpansion('Property', 'BuyerAgent', 'Member'),
  makeExpansion('Property', 'ListOffice', 'Office'),
  makeExpansion('Property', 'BuyerOffice', 'Office'),
  makeExpansion('Property', 'ListTeam', 'Teams'),
  makeCollectionExpansion('Property', 'Media', 'Media'),
  makeCollectionExpansion('Property', 'OpenHouse', 'OpenHouse')
];

const MEMBER_FIELDS: ReadonlyArray<ResoField> = [
  makeField('Member', 'MemberKey'),
  makeField('Member', 'MemberFirstName'),
  makeField('Member', 'OfficeKey'),
  makeExpansion('Member', 'Office', 'Office'),
  makeCollectionExpansion('Member', 'Media', 'Media')
];

const OFFICE_FIELDS: ReadonlyArray<ResoField> = [
  makeField('Office', 'OfficeKey'),
  makeField('Office', 'OfficeName'),
  makeField('Office', 'OfficeBrokerKey'),
  makeField('Office', 'OfficeManagerKey'),
  makeField('Office', 'MainOfficeKey'),
  makeExpansion('Office', 'OfficeBroker', 'Member'),
  makeExpansion('Office', 'OfficeManager', 'Member'),
  makeExpansion('Office', 'MainOffice', 'Office')
];

const TEAMS_FIELDS: ReadonlyArray<ResoField> = [
  makeField('Teams', 'TeamKey'),
  makeField('Teams', 'TeamLeadKey'),
  makeExpansion('Teams', 'TeamLead', 'Member')
];

const TEAM_MEMBERS_FIELDS: ReadonlyArray<ResoField> = [
  makeField('TeamMembers', 'TeamMemberKey'),
  makeField('TeamMembers', 'MemberKey'),
  makeField('TeamMembers', 'TeamKey'),
  makeExpansion('TeamMembers', 'Member', 'Member')
  // Note: no nav prop for Teams — TeamKey is an implicit FK
];

const MEDIA_FIELDS: ReadonlyArray<ResoField> = [
  makeField('Media', 'MediaKey'),
  makeField('Media', 'MediaURL'),
  makeField('Media', 'ResourceName'),
  makeField('Media', 'ResourceRecordKey'),
  makeField('Media', 'ChangedByMemberKey'),
  makeExpansion('Media', 'ChangedByMember', 'Member')
];

const OUID_FIELDS: ReadonlyArray<ResoField> = [
  makeField('OUID', 'OrganizationUniqueIdKey'),
  makeField('OUID', 'ChangedByMemberKey'),
  makeExpansion('OUID', 'ChangedByMember', 'Member')
];

const ALL_FIELDS: Record<string, ReadonlyArray<ResoField>> = {
  Property: PROPERTY_FIELDS,
  Member: MEMBER_FIELDS,
  Office: OFFICE_FIELDS,
  Teams: TEAMS_FIELDS,
  TeamMembers: TEAM_MEMBERS_FIELDS,
  Media: MEDIA_FIELDS,
  OUID: OUID_FIELDS
};

// ---------------------------------------------------------------------------
// discoverForeignKeys
// ---------------------------------------------------------------------------

describe('discoverForeignKeys', () => {
  it('discovers to-one FK bindings from nav prop expansion fields', () => {
    const bindings = discoverForeignKeys('Property', PROPERTY_FIELDS, KEY_FIELD_MAP);
    expect(bindings).toContainEqual({
      fkColumn: 'ListAgentKey',
      targetResource: 'Member',
      targetKeyField: 'MemberKey',
      navPropName: 'ListAgent'
    });
    expect(bindings).toContainEqual({
      fkColumn: 'BuyerAgentKey',
      targetResource: 'Member',
      targetKeyField: 'MemberKey',
      navPropName: 'BuyerAgent'
    });
    expect(bindings).toContainEqual({
      fkColumn: 'ListOfficeKey',
      targetResource: 'Office',
      targetKeyField: 'OfficeKey',
      navPropName: 'ListOffice'
    });
  });

  it('ignores collection (to-many) expansion fields', () => {
    const bindings = discoverForeignKeys('Property', PROPERTY_FIELDS, KEY_FIELD_MAP);
    const mediaBinding = bindings.find(b => b.navPropName === 'Media');
    expect(mediaBinding).toBeUndefined();
  });

  it('discovers self-referencing FK (Office.MainOfficeKey → Office)', () => {
    const bindings = discoverForeignKeys('Office', OFFICE_FIELDS, KEY_FIELD_MAP);
    expect(bindings).toContainEqual({
      fkColumn: 'MainOfficeKey',
      targetResource: 'Office',
      targetKeyField: 'OfficeKey',
      navPropName: 'MainOffice'
    });
  });

  it('discovers Member.OfficeKey → Office', () => {
    const bindings = discoverForeignKeys('Member', MEMBER_FIELDS, KEY_FIELD_MAP);
    expect(bindings).toContainEqual({
      fkColumn: 'OfficeKey',
      targetResource: 'Office',
      targetKeyField: 'OfficeKey',
      navPropName: 'Office'
    });
  });

  it('discovers implicit FK by convention (TeamMembers.TeamKey → Teams)', () => {
    const bindings = discoverForeignKeys('TeamMembers', TEAM_MEMBERS_FIELDS, KEY_FIELD_MAP);
    const teamBinding = bindings.find(b => b.fkColumn === 'TeamKey');
    expect(teamBinding).toBeDefined();
    expect(teamBinding?.targetResource).toBe('Teams');
    expect(teamBinding?.targetKeyField).toBe('TeamKey');
  });

  it('discovers both nav-prop and implicit FKs on TeamMembers', () => {
    const bindings = discoverForeignKeys('TeamMembers', TEAM_MEMBERS_FIELDS, KEY_FIELD_MAP);
    expect(bindings).toHaveLength(2);
    expect(bindings.map(b => b.fkColumn).sort()).toEqual(['MemberKey', 'TeamKey']);
  });

  it('does not treat own PK as a FK', () => {
    const bindings = discoverForeignKeys('Member', MEMBER_FIELDS, KEY_FIELD_MAP);
    const selfBinding = bindings.find(b => b.fkColumn === 'MemberKey');
    expect(selfBinding).toBeUndefined();
  });

  it('returns empty array when no FK bindings exist', () => {
    const fields: ResoField[] = [makeField('Simple', 'SimpleKey'), makeField('Simple', 'Name')];
    const bindings = discoverForeignKeys('Simple', fields, { Simple: 'SimpleKey' });
    expect(bindings).toHaveLength(0);
  });

  it('skips expansion fields whose target is not in the key field map', () => {
    const fields: ResoField[] = [
      makeField('Foo', 'FooKey'),
      makeField('Foo', 'UnknownThingKey'),
      makeExpansion('Foo', 'UnknownThing', 'SomeUnknownResource')
    ];
    const bindings = discoverForeignKeys('Foo', fields, { Foo: 'FooKey' });
    expect(bindings).toHaveLength(0);
  });
});

// ---------------------------------------------------------------------------
// buildDependencyGraph
// ---------------------------------------------------------------------------

describe('buildDependencyGraph', () => {
  it('builds edges for all to-one FK dependencies', () => {
    const deps = buildDependencyGraph(['Property', 'Member', 'Office'], ALL_FIELDS, KEY_FIELD_MAP);

    const propertyToMember = deps.find(d => d.sourceResource === 'Property' && d.targetResource === 'Member');
    expect(propertyToMember).toBeDefined();
    expect(propertyToMember!.bindings.length).toBeGreaterThanOrEqual(2); // ListAgent, BuyerAgent

    const memberToOffice = deps.find(d => d.sourceResource === 'Member' && d.targetResource === 'Office');
    expect(memberToOffice).toBeDefined();

    const officeToMember = deps.find(d => d.sourceResource === 'Office' && d.targetResource === 'Member');
    expect(officeToMember).toBeDefined();
  });

  it('only includes edges where both source and target are in the resource list', () => {
    const deps = buildDependencyGraph(['Property', 'Member'], ALL_FIELDS, KEY_FIELD_MAP);
    const toOffice = deps.find(d => d.targetResource === 'Office');
    expect(toOffice).toBeUndefined();
  });

  it('includes self-referencing edges', () => {
    const deps = buildDependencyGraph(['Office', 'Member'], ALL_FIELDS, KEY_FIELD_MAP);
    const selfRef = deps.find(d => d.sourceResource === 'Office' && d.targetResource === 'Office');
    expect(selfRef).toBeDefined();
    expect(selfRef!.bindings).toContainEqual(expect.objectContaining({ fkColumn: 'MainOfficeKey' }));
  });
});

// ---------------------------------------------------------------------------
// topologicalSort
// ---------------------------------------------------------------------------

describe('topologicalSort', () => {
  it('sorts acyclic dependencies correctly', () => {
    const deps = buildDependencyGraph(['Property', 'Member', 'Office'], ALL_FIELDS, KEY_FIELD_MAP);
    // Remove the Office→Member edges to make it acyclic for this test
    const acyclicDeps = deps.filter(d => !(d.sourceResource === 'Office' && d.targetResource === 'Member'));
    const { order } = topologicalSort(acyclicDeps, ['Property', 'Member', 'Office']);

    // Office must come before Member (Member depends on Office)
    expect(order.indexOf('Office')).toBeLessThan(order.indexOf('Member'));
    // Member must come before Property (Property depends on Member)
    expect(order.indexOf('Member')).toBeLessThan(order.indexOf('Property'));
  });

  it('detects and breaks Office ↔ Member cycle', () => {
    const deps = buildDependencyGraph(['Office', 'Member'], ALL_FIELDS, KEY_FIELD_MAP);
    const { order, deferredEdges } = topologicalSort(deps, ['Office', 'Member']);

    // Both resources should be in the output
    expect(order).toContain('Office');
    expect(order).toContain('Member');
    expect(order).toHaveLength(2);

    // One edge should be deferred (the back-fill edge)
    expect(deferredEdges.length).toBeGreaterThan(0);
  });

  it('handles self-references without error', () => {
    const deps = buildDependencyGraph(['Office'], ALL_FIELDS, KEY_FIELD_MAP);
    const { order } = topologicalSort(deps, ['Office']);
    expect(order).toContain('Office');
  });

  it('returns all resources even when no dependencies exist', () => {
    const { order } = topologicalSort([], ['A', 'B', 'C']);
    expect(order).toHaveLength(3);
    expect(order).toContain('A');
    expect(order).toContain('B');
    expect(order).toContain('C');
  });

  it('produces deterministic output', () => {
    const deps = buildDependencyGraph(['Property', 'Member', 'Office', 'Teams', 'OUID'], ALL_FIELDS, KEY_FIELD_MAP);
    const result1 = topologicalSort(deps, ['Property', 'Member', 'Office', 'Teams', 'OUID']);
    const result2 = topologicalSort(deps, ['Property', 'Member', 'Office', 'Teams', 'OUID']);
    expect(result1.order).toEqual(result2.order);
  });
});

// ---------------------------------------------------------------------------
// buildMultiResourcePlan
// ---------------------------------------------------------------------------

describe('buildMultiResourcePlan', () => {
  it('includes transitive dependencies for Property', () => {
    const plan = buildMultiResourcePlan('Property', 10, undefined, ALL_FIELDS, KEY_FIELD_MAP);
    const resourcesInPlan = plan.phases.map(p => p.resource);
    expect(resourcesInPlan).toContain('Property');
    expect(resourcesInPlan).toContain('Member');
    expect(resourcesInPlan).toContain('Office');
  });

  it('puts Member before Office in the phases (Member is more depended upon)', () => {
    const plan = buildMultiResourcePlan('Property', 10, undefined, ALL_FIELDS, KEY_FIELD_MAP);
    const resources = plan.phases.map(p => p.resource);
    // Member has more incoming edges (Property, Office, Media, Teams, OUID, TeamMembers all depend on it)
    // so the cycle breaker defers Member→Office, putting Member first
    expect(resources.indexOf('Member')).toBeLessThan(resources.indexOf('Office'));
  });

  it('puts Member before Property in the phases', () => {
    const plan = buildMultiResourcePlan('Property', 10, undefined, ALL_FIELDS, KEY_FIELD_MAP);
    const resources = plan.phases.map(p => p.resource);
    expect(resources.indexOf('Member')).toBeLessThan(resources.indexOf('Property'));
  });

  it('generates a back-fill phase for the Office ↔ Member cycle', () => {
    const plan = buildMultiResourcePlan('Property', 10, undefined, ALL_FIELDS, KEY_FIELD_MAP);
    expect(plan.backFillPhases.length).toBeGreaterThan(0);
    // Member has more incoming edges, so Member→Office is deferred.
    // Member is created first (without OfficeKey), then back-filled.
    const memberBackFill = plan.backFillPhases.find(b => b.resource === 'Member');
    expect(memberBackFill).toBeDefined();
    expect(memberBackFill!.fkBindings.some(b => b.targetResource === 'Office')).toBe(true);
  });

  it('assigns correct FK bindings to Property phase', () => {
    const plan = buildMultiResourcePlan('Property', 10, undefined, ALL_FIELDS, KEY_FIELD_MAP);
    const propertyPhase = plan.phases.find(p => p.resource === 'Property');
    expect(propertyPhase).toBeDefined();
    const fkColumns = propertyPhase!.fkBindings.map(b => b.fkColumn);
    expect(fkColumns).toContain('ListAgentKey');
    expect(fkColumns).toContain('ListOfficeKey');
  });

  it('sets the requested resource count correctly', () => {
    const plan = buildMultiResourcePlan('Property', 50, undefined, ALL_FIELDS, KEY_FIELD_MAP);
    const propertyPhase = plan.phases.find(p => p.resource === 'Property');
    expect(propertyPhase?.count).toBe(50);
  });

  it('assigns dependency counts based on ratios', () => {
    const plan = buildMultiResourcePlan('Property', 50, undefined, ALL_FIELDS, KEY_FIELD_MAP);
    const officePhase = plan.phases.find(p => p.resource === 'Office');
    const memberPhase = plan.phases.find(p => p.resource === 'Member');
    expect(officePhase?.count).toBe(10); // ceil(50/5)
    expect(memberPhase?.count).toBe(25); // ceil(50/2)
  });

  it('returns empty back-fill for resources with no cycles', () => {
    // Use simplified Office without OfficeBroker/OfficeManager FKs (no cycle)
    const simpleOfficeFields: ReadonlyArray<ResoField> = [makeField('Office', 'OfficeKey'), makeField('Office', 'OfficeName')];
    const plan = buildMultiResourcePlan('Member', 10, undefined, { Member: MEMBER_FIELDS, Office: simpleOfficeFields }, KEY_FIELD_MAP);
    expect(plan.backFillPhases).toHaveLength(0);
  });

  it('includes child collection dependency resources', () => {
    // Media depends on Member (ChangedByMember), so Member should be in the plan
    // even if the main resource is just Property
    const plan = buildMultiResourcePlan('Property', 10, { Media: 5 }, ALL_FIELDS, KEY_FIELD_MAP);
    const resources = plan.phases.map(p => p.resource);
    expect(resources).toContain('Member');
  });

  it('preserves relatedRecords in the plan', () => {
    const plan = buildMultiResourcePlan('Property', 10, { Media: 5, OpenHouse: 2 }, ALL_FIELDS, KEY_FIELD_MAP);
    expect(plan.relatedRecords).toEqual({ Media: 5, OpenHouse: 2 });
  });

  it('handles TeamMembers with implicit TeamKey FK', () => {
    const plan = buildMultiResourcePlan('TeamMembers', 10, undefined, ALL_FIELDS, KEY_FIELD_MAP);
    const resources = plan.phases.map(p => p.resource);
    expect(resources).toContain('Teams');
    expect(resources).toContain('Member');
    expect(resources.indexOf('Teams')).toBeLessThan(resources.indexOf('TeamMembers'));
    expect(resources.indexOf('Member')).toBeLessThan(resources.indexOf('TeamMembers'));
  });
});
