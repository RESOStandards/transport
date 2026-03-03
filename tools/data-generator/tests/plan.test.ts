import { describe, expect, it } from 'vitest';
import type { ResoField } from '../src/generators/types.js';
import { buildSeedPlan, getDefaultRelatedCount, getRelatedResources, isChildResource } from '../src/plan.js';

const makeField = (resourceName: string, fieldName: string, type = 'Edm.String'): ResoField => ({
  resourceName,
  fieldName,
  type,
  nullable: true,
  annotations: []
});

const SAMPLE_FIELDS_BY_RESOURCE: Record<string, ReadonlyArray<ResoField>> = {
  Property: [makeField('Property', 'ListingKey'), makeField('Property', 'ListPrice', 'Edm.Decimal'), makeField('Property', 'City')],
  Media: [
    makeField('Media', 'MediaKey'),
    makeField('Media', 'MediaURL'),
    makeField('Media', 'ResourceName'),
    makeField('Media', 'ResourceRecordKey')
  ],
  OpenHouse: [
    makeField('OpenHouse', 'OpenHouseKey'),
    makeField('OpenHouse', 'OpenHouseDate', 'Edm.Date'),
    makeField('OpenHouse', 'ResourceName'),
    makeField('OpenHouse', 'ResourceRecordKey')
  ],
  Member: [makeField('Member', 'MemberKey'), makeField('Member', 'MemberFirstName'), makeField('Member', 'MemberLastName')],
  Office: [makeField('Office', 'OfficeKey'), makeField('Office', 'OfficeName')]
};

describe('getRelatedResources', () => {
  it('returns resources with ResourceName and ResourceRecordKey fields', () => {
    const related = getRelatedResources('Property', SAMPLE_FIELDS_BY_RESOURCE);
    expect(related).toContain('Media');
    expect(related).toContain('OpenHouse');
    expect(related).not.toContain('Property');
    expect(related).not.toContain('Member');
    expect(related).not.toContain('Office');
  });

  it('does not include the parent resource itself', () => {
    const related = getRelatedResources('Media', SAMPLE_FIELDS_BY_RESOURCE);
    expect(related).not.toContain('Media');
    expect(related).toContain('OpenHouse');
  });

  it('returns empty array when no child resources exist', () => {
    const fields: Record<string, ReadonlyArray<ResoField>> = {
      Property: [makeField('Property', 'ListingKey')],
      Member: [makeField('Member', 'MemberKey')]
    };
    const related = getRelatedResources('Property', fields);
    expect(related).toHaveLength(0);
  });
});

describe('getDefaultRelatedCount', () => {
  it('returns 5 for Media', () => {
    expect(getDefaultRelatedCount('Media')).toBe(5);
  });

  it('returns 2 for OpenHouse', () => {
    expect(getDefaultRelatedCount('OpenHouse')).toBe(2);
  });

  it('returns 2 for Showing', () => {
    expect(getDefaultRelatedCount('Showing')).toBe(2);
  });

  it('returns 2 for unknown resources', () => {
    expect(getDefaultRelatedCount('SomeOtherResource')).toBe(2);
  });
});

describe('isChildResource', () => {
  it('returns true for known child resources', () => {
    expect(isChildResource('Media')).toBe(true);
    expect(isChildResource('OpenHouse')).toBe(true);
    expect(isChildResource('Showing')).toBe(true);
  });

  it('returns false for non-child resources', () => {
    expect(isChildResource('Property')).toBe(false);
    expect(isChildResource('Member')).toBe(false);
    expect(isChildResource('Office')).toBe(false);
  });
});

describe('buildSeedPlan', () => {
  it('creates a plan with the parent resource', () => {
    const plan = buildSeedPlan({ resource: 'Property', count: 10 });
    expect(plan.resources).toHaveLength(1);
    expect(plan.resources[0].resource).toBe('Property');
    expect(plan.resources[0].count).toBe(10);
  });

  it('includes related record configuration', () => {
    const plan = buildSeedPlan({
      resource: 'Property',
      count: 5,
      relatedRecords: { Media: 3, OpenHouse: 2 }
    });
    expect(plan.resources[0].relatedRecords).toEqual({ Media: 3, OpenHouse: 2 });
  });
});
