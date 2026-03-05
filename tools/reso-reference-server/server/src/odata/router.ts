import { Router } from 'express';
import type { DataAccessLayer, NavigationPropertyBinding, ResourceContext } from '../db/data-access.js';
import { getFieldsForResource, getKeyFieldForResource } from '../metadata/loader.js';
import type { ResoMetadata } from '../metadata/types.js';
import { KEY_FIELD_MAP } from '../metadata/types.js';
import { collectionHandler, createHandler, deleteHandler, readHandler, updateHandler } from './handlers.js';

/**
 * Build navigation property bindings for a resource from isExpansion metadata.
 *
 * Each field with isExpansion=true defines a navigation property. The FK
 * strategy is determined by inspecting the target resource's fields:
 *
 * 1. resource-record-key — target has ResourceName + ResourceRecordKey
 *    (polymorphic child, e.g. Media belongs to Property, Member, Office, ...)
 *
 * 2. direct — target has the parent's key field directly
 *    (dedicated child, e.g. OpenHouse.ListingKey → Property.ListingKey)
 *
 * 3. parent-fk — parent has {NavPropName}Key referencing the target's key
 *    (to-one lookup, e.g. Property.BuyerAgentKey → Member.MemberKey)
 */
export const buildNavigationBindings = (
  resource: string,
  metadata: ResoMetadata,
  targetResources: ReadonlyArray<string>
): ReadonlyArray<NavigationPropertyBinding> => {
  const parentFields = getFieldsForResource(metadata, resource);
  const expansionFields = parentFields.filter(f => f.isExpansion);
  const parentKeyField = KEY_FIELD_MAP[resource];
  const targetResourceSet = new Set(targetResources);
  const bindings: NavigationPropertyBinding[] = [];

  for (const field of expansionFields) {
    const targetResource = field.typeName;
    if (!targetResource || !targetResourceSet.has(targetResource)) continue;

    const targetKeyField = KEY_FIELD_MAP[targetResource];
    if (!targetKeyField) continue;

    const targetFields = getFieldsForResource(metadata, targetResource);
    const hasResourceRecordKey =
      targetFields.some(f => f.fieldName === 'ResourceName') && targetFields.some(f => f.fieldName === 'ResourceRecordKey');

    let foreignKey: NavigationPropertyBinding['foreignKey'];

    if (field.isCollection && hasResourceRecordKey) {
      // Polymorphic child (Media, HistoryTransactional, SocialMedia)
      foreignKey = { strategy: 'resource-record-key' };
    } else if (field.isCollection && parentKeyField && targetFields.some(f => f.fieldName === parentKeyField)) {
      // Dedicated child with parent's key as direct FK (OpenHouse.ListingKey, Showing.ListingKey)
      foreignKey = { strategy: 'direct', targetColumn: parentKeyField };
    } else if (!field.isCollection) {
      // To-one lookup: parent has {NavPropName}Key → target's key
      const parentFkColumn = `${field.fieldName}Key`;
      if (parentFields.some(f => f.fieldName === parentFkColumn)) {
        foreignKey = { strategy: 'parent-fk', parentColumn: parentFkColumn };
      } else {
        continue; // Can't resolve FK, skip
      }
    } else {
      continue; // Can't resolve FK, skip
    }

    bindings.push({
      name: field.fieldName,
      targetResource,
      targetKeyField,
      targetFields,
      foreignKey,
      isCollection: field.isCollection ?? false
    });
  }

  return bindings;
};

/**
 * Creates an Express router with dynamic OData routes for all target resources.
 *
 * For each resource, registers:
 * - GET /{Resource} — collection query (with $filter, $select, $orderby, etc.)
 * - POST /{Resource} — create
 * - GET /{Resource}('key') — read (regex route)
 * - PATCH /{Resource}('key') — update (regex route)
 * - DELETE /{Resource}('key') — delete (regex route)
 */
export const createODataRouter = (
  metadata: ResoMetadata,
  dal: DataAccessLayer,
  baseUrl: string,
  targetResources: ReadonlyArray<string>,
  readOnlyResources: ReadonlySet<string> = new Set()
): Router => {
  const router = Router();

  for (const resource of targetResources) {
    const fields = getFieldsForResource(metadata, resource);
    const keyField = getKeyFieldForResource(resource);

    if (!keyField) {
      console.warn(`No key field mapping for resource "${resource}", skipping`);
      continue;
    }

    if (fields.length === 0) {
      console.warn(`No fields found for resource "${resource}", skipping`);
      continue;
    }

    const navigationBindings = buildNavigationBindings(resource, metadata, targetResources);

    const resourceCtx: ResourceContext = {
      resource,
      keyField,
      fields,
      navigationBindings
    };

    const ctx = { resourceCtx, dal, baseUrl };

    // OData key pattern: /{Resource}('key')
    const keyPattern = new RegExp(`^/${resource}\\('([^']+)'\\)$`);

    // Collection route must be registered first (exact match)
    const isReadOnly = readOnlyResources.has(resource);
    router.get(`/${resource}`, collectionHandler(ctx));
    if (!isReadOnly) router.post(`/${resource}`, createHandler(ctx));
    router.get(keyPattern, readHandler(ctx));
    if (!isReadOnly) router.patch(keyPattern, updateHandler(ctx));
    if (!isReadOnly) router.delete(keyPattern, deleteHandler(ctx));

    console.log(
      `  Registered routes for ${resource} (${fields.length} fields, key: ${keyField}${isReadOnly ? ', read-only' : ''}, navProps: ${navigationBindings.map(b => b.name).join(', ') || 'none'})`
    );
  }

  return router;
};
