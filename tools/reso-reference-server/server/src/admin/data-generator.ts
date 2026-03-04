import { randomUUID } from 'node:crypto';
import { getDefaultRelatedCount, getGenerator, getRelatedResources } from '@reso/data-generator';
import type { ResoField, ResoLookup } from '@reso/data-generator';
import type { RequestHandler } from 'express';
import type { DataAccessLayer, ResourceContext } from '../db/data-access.js';
import { getFieldsForResource, getKeyFieldForResource, getLookupsForType, isEnumType } from '../metadata/loader.js';
import type { ResoMetadata } from '../metadata/types.js';
import { TARGET_RESOURCES } from '../metadata/types.js';

/** Request body for the data generator endpoint. */
interface GenerateRequest {
  readonly resource: string;
  readonly count: number;
  readonly relatedRecords?: Readonly<Record<string, number>>;
}

/** Response from the data generator endpoint. */
interface GenerateResponse {
  readonly resource: string;
  readonly created: number;
  readonly failed: number;
  readonly errors: ReadonlyArray<string>;
  readonly relatedResults: ReadonlyArray<{
    readonly resource: string;
    readonly created: number;
    readonly failed: number;
  }>;
  readonly durationMs: number;
}

/** Builds a ResourceContext for a given resource name. */
const buildResourceContext = (metadata: ResoMetadata, resource: string): ResourceContext | null => {
  const keyField = getKeyFieldForResource(resource);
  if (!keyField) return null;

  const fields = getFieldsForResource(metadata, resource);
  if (fields.length === 0) return null;

  return {
    resource,
    keyField,
    fields,
    navigationBindings: []
  };
};

/** Collects all lookup values for fields of a resource into a lookup map. */
const buildLookupMap = (metadata: ResoMetadata, fields: ReadonlyArray<ResoField>): Record<string, ReadonlyArray<ResoLookup>> => {
  const lookupMap: Record<string, ResoLookup[]> = {};
  for (const field of fields) {
    if (isEnumType(field.type) && !lookupMap[field.type]) {
      const lookups = getLookupsForType(metadata, field.type);
      if (lookups.length > 0) {
        lookupMap[field.type] = [...lookups];
      }
    }
  }
  return lookupMap;
};

/**
 * Creates a POST handler for generating seed data.
 *
 * Unlike the CLI tool which POSTs via HTTP, this endpoint uses the DAL
 * directly for performance since we're already inside the server process.
 */
export const createDataGeneratorHandler =
  (metadata: ResoMetadata, dal: DataAccessLayer): RequestHandler =>
  async (req, res) => {
    const startTime = Date.now();
    const body = req.body as GenerateRequest;

    if (!body.resource || typeof body.resource !== 'string') {
      res.status(400).json({ error: { code: '40000', message: 'Missing required field: resource', details: [] } });
      return;
    }

    if (!body.count || typeof body.count !== 'number' || body.count < 1) {
      res
        .status(400)
        .json({ error: { code: '40001', message: 'Missing or invalid field: count (must be a positive integer)', details: [] } });
      return;
    }

    const resourceCtx = buildResourceContext(metadata, body.resource);
    if (!resourceCtx) {
      res.status(400).json({
        error: { code: '40002', message: `Unknown resource "${body.resource}". Available: ${TARGET_RESOURCES.join(', ')}`, details: [] }
      });
      return;
    }

    try {
      const fields = getFieldsForResource(metadata, body.resource);
      const lookups = buildLookupMap(metadata, fields);

      // Generate parent records
      const generator = getGenerator(body.resource);
      const records = generator(fields, lookups, body.count);

      let created = 0;
      let failed = 0;
      const errors: string[] = [];
      const parentKeys: string[] = [];

      for (let i = 0; i < records.length; i++) {
        try {
          const key = randomUUID();
          const record = {
            ...records[i],
            [resourceCtx.keyField]: key,
            ModificationTimestamp: new Date().toISOString()
          };
          await dal.insert(resourceCtx, record);
          created++;
          parentKeys.push(key);
        } catch (err) {
          failed++;
          errors.push(`Record ${i + 1}: ${err instanceof Error ? err.message : 'Insert failed'}`);
        }
      }

      // Generate related records
      const relatedResults: Array<{ resource: string; created: number; failed: number }> = [];

      if (body.relatedRecords) {
        for (const [relatedResource, relatedCount] of Object.entries(body.relatedRecords)) {
          const relatedCtx = buildResourceContext(metadata, relatedResource);
          if (!relatedCtx) {
            errors.push(`Unknown related resource: ${relatedResource}`);
            continue;
          }

          const relatedFields = getFieldsForResource(metadata, relatedResource);
          const relatedLookups = buildLookupMap(metadata, relatedFields);
          const relatedGenerator = getGenerator(relatedResource);
          let relatedCreated = 0;
          let relatedFailed = 0;

          for (const parentKey of parentKeys) {
            const relatedRecords = relatedGenerator(relatedFields, relatedLookups, relatedCount, body.resource, parentKey);
            for (const relRecord of relatedRecords) {
              try {
                const key = randomUUID();
                const record = {
                  ...relRecord,
                  [relatedCtx.keyField]: key,
                  ModificationTimestamp: new Date().toISOString()
                };
                await dal.insert(relatedCtx, record);
                relatedCreated++;
              } catch (err) {
                relatedFailed++;
                errors.push(`${relatedResource}: ${err instanceof Error ? err.message : 'Insert failed'}`);
              }
            }
          }

          relatedResults.push({ resource: relatedResource, created: relatedCreated, failed: relatedFailed });
        }
      }

      const response: GenerateResponse = {
        resource: body.resource,
        created,
        failed,
        errors: errors.slice(0, 20),
        relatedResults,
        durationMs: Date.now() - startTime
      };

      res.json(response);
    } catch (err) {
      res.status(500).json({
        error: {
          code: '50000',
          message: err instanceof Error ? err.message : 'Internal server error',
          details: []
        }
      });
    }
  };

/**
 * Creates a GET handler for returning data generator status.
 * Returns available resources and their current record counts.
 */
export const createDataGeneratorStatusHandler =
  (metadata: ResoMetadata, dal: DataAccessLayer): RequestHandler =>
  async (_req, res) => {
    try {
      // Build fieldsByResource map for getRelatedResources
      const fieldsByResource: Record<string, ReadonlyArray<ResoField>> = {};
      for (const resource of TARGET_RESOURCES) {
        fieldsByResource[resource] = getFieldsForResource(metadata, resource);
      }

      const resources: Array<{
        resource: string;
        fields: number;
        count: number;
        relatedResources: ReadonlyArray<{ resource: string; defaultCount: number }>;
      }> = [];

      for (const resource of TARGET_RESOURCES) {
        const resourceCtx = buildResourceContext(metadata, resource);
        if (!resourceCtx) continue;

        const result = await dal.queryCollection(resourceCtx, { $count: true, $top: 0 });
        const related = getRelatedResources(resource, fieldsByResource);
        resources.push({
          resource,
          fields: resourceCtx.fields.length,
          count: result.count ?? 0,
          relatedResources: related.map(r => ({ resource: r, defaultCount: getDefaultRelatedCount(r) }))
        });
      }

      res.json({ resources });
    } catch (err) {
      res.status(500).json({
        error: {
          code: '50000',
          message: err instanceof Error ? err.message : 'Internal server error',
          details: []
        }
      });
    }
  };
