/**
 * EntityEvent DAL decorator — wraps a DataAccessLayer to automatically capture
 * insert, update, and delete operations as EntityEvent records.
 *
 * The decorator is backend-agnostic. Each database backend provides its own
 * EntityEventWriter that handles sequence generation and persistence.
 */

import type { DataAccessLayer, EntityRecord, ResourceContext, SingleResult } from './data-access.js';

/** Writes a single EntityEvent record to the underlying store. */
export interface EntityEventWriter {
  readonly writeEvent: (resourceName: string, resourceRecordKey: string, resourceRecordUrl?: string) => Promise<void>;
}

/** Configuration for the EntityEvent DAL decorator. */
export interface EntityEventConfig {
  readonly baseUrl: string;
  readonly includeResourceRecordUrl: boolean;
}

/** Builds a ResourceRecordUrl for a given resource and key. */
const buildResourceRecordUrl = (baseUrl: string, resourceName: string, key: string): string =>
  `${baseUrl}/${resourceName}('${encodeURIComponent(key)}')`;

/**
 * Wraps a DataAccessLayer to automatically write EntityEvent records after
 * successful insert, update, and delete operations.
 *
 * - Skips EntityEvent writes for the EntityEvent resource itself (no recursion).
 * - Read operations (queryCollection, readByKey) pass through unchanged.
 * - Only writes an event when the underlying operation succeeds.
 */
export const createEntityEventDal = (inner: DataAccessLayer, writer: EntityEventWriter, config: EntityEventConfig): DataAccessLayer => {
  const maybeWriteEvent = async (resourceName: string, key: string): Promise<void> => {
    const url = config.includeResourceRecordUrl ? buildResourceRecordUrl(config.baseUrl, resourceName, key) : undefined;
    await writer.writeEvent(resourceName, key, url);
  };

  return {
    queryCollection: inner.queryCollection,

    readByKey: inner.readByKey,

    insert: async (ctx: ResourceContext, record: Readonly<Record<string, unknown>>): Promise<EntityRecord> => {
      const result = await inner.insert(ctx, record);
      if (ctx.resource !== 'EntityEvent') {
        const key = String(result[ctx.keyField] ?? '');
        await maybeWriteEvent(ctx.resource, key);
      }
      return result;
    },

    update: async (ctx: ResourceContext, keyValue: string, updates: Readonly<Record<string, unknown>>): Promise<SingleResult> => {
      const result = await inner.update(ctx, keyValue, updates);
      if (result !== undefined && ctx.resource !== 'EntityEvent') {
        await maybeWriteEvent(ctx.resource, keyValue);
      }
      return result;
    },

    deleteByKey: async (ctx: ResourceContext, keyValue: string): Promise<boolean> => {
      const deleted = await inner.deleteByKey(ctx, keyValue);
      if (deleted && ctx.resource !== 'EntityEvent') {
        await maybeWriteEvent(ctx.resource, keyValue);
      }
      return deleted;
    }
  };
};
