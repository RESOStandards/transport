import { Router } from 'express';
import type { DataAccessLayer, NavigationPropertyBinding, ResourceContext } from '../db/data-access.js';
import { getFieldsForResource, getKeyFieldForResource } from '../metadata/loader.js';
import type { ResoMetadata } from '../metadata/types.js';
import { KEY_FIELD_MAP } from '../metadata/types.js';
import { collectionHandler, createHandler, deleteHandler, readHandler, updateHandler } from './handlers.js';

/**
 * Build navigation property bindings for a resource based on RESO conventions.
 *
 * RESO uses the ResourceName + ResourceRecordKey pattern for child resources
 * like Media, which have columns pointing back to the parent resource.
 */
const buildNavigationBindings = (
  resource: string,
  metadata: ResoMetadata,
  targetResources: ReadonlyArray<string>
): ReadonlyArray<NavigationPropertyBinding> => {
  const bindings: NavigationPropertyBinding[] = [];

  // Check which target resources have ResourceName/ResourceRecordKey fields,
  // indicating they can be child resources via the RESO FK convention.
  for (const targetResource of targetResources) {
    if (targetResource === resource) continue;

    const targetFields = getFieldsForResource(metadata, targetResource);
    const hasResourceName = targetFields.some(f => f.fieldName === 'ResourceName');
    const hasResourceRecordKey = targetFields.some(f => f.fieldName === 'ResourceRecordKey');

    if (hasResourceName && hasResourceRecordKey) {
      const targetKeyField = KEY_FIELD_MAP[targetResource];
      if (!targetKeyField) continue;

      bindings.push({
        name: targetResource,
        targetResource,
        targetKeyField,
        targetFields,
        foreignKey: { strategy: 'resource-record-key' },
        isCollection: true
      });
    }
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
  targetResources: ReadonlyArray<string>
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
    router.get(`/${resource}`, collectionHandler(ctx));
    router.post(`/${resource}`, createHandler(ctx));
    router.get(keyPattern, readHandler(ctx));
    router.patch(keyPattern, updateHandler(ctx));
    router.delete(keyPattern, deleteHandler(ctx));

    console.log(
      `  Registered routes for ${resource} (${fields.length} fields, key: ${keyField}, navProps: ${navigationBindings.map(b => b.name).join(', ') || 'none'})`
    );
  }

  return router;
};
