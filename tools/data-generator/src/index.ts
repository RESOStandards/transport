export type {
  AuthConfig,
  BackFillPhase,
  ForeignKeyBinding,
  GeneratorConfig,
  MultiResourceSeedPlan,
  ResourceDependency,
  ResoAnnotation,
  ResoField,
  ResoLookup,
  SeedOptions,
  SeedPhase,
  SeedPlan,
  SeedResult
} from './generators/types.js';
export { KEY_FIELD_MAP } from './generators/types.js';

export type { BatchCreateResult, OutputFormat, OutputOptions } from './client.js';
export { patchRecordsViaHttp } from './client.js';
export { buildDependencyGraph, buildMultiResourcePlan, discoverForeignKeys, topologicalSort } from './fk-resolver.js';
export { getGenerator } from './generators/index.js';
export { buildSeedPlan, getDefaultRelatedCount, getRelatedResources } from './plan.js';

import { randomUUID } from 'node:crypto';
import { createRecordsViaHttp, generateCurlScript, patchRecordsViaHttp, writeRecordsAsJson } from './client.js';
import type { OutputOptions } from './client.js';
import { buildMultiResourcePlan } from './fk-resolver.js';
import { getGenerator } from './generators/index.js';
import type { ForeignKeyBinding, SeedOptions, SeedResult } from './generators/types.js';
import { KEY_FIELD_MAP } from './generators/types.js';

/** Progress callback for tracking generation progress. */
export type ProgressCallback = (message: string, completed: number, total: number) => void;

/** Picks a random element from an array. */
const pickRandom = <T>(arr: ReadonlyArray<T>): T => arr[Math.floor(Math.random() * arr.length)];

/** Injects FK values into records from the key pool. */
const injectForeignKeys = (
  records: Record<string, unknown>[],
  bindings: ReadonlyArray<ForeignKeyBinding>,
  keyPool: Readonly<Record<string, ReadonlyArray<string>>>
): void => {
  for (const record of records) {
    for (const binding of bindings) {
      const pool = keyPool[binding.targetResource];
      if (pool && pool.length > 0) {
        record[binding.fkColumn] = pickRandom(pool);
      }
    }
  }
};

/**
 * Generates seed data with automatic dependency resolution.
 *
 * Builds a multi-resource plan from metadata, creates dependency resources
 * in topological order, and injects FK values from a key pool. For offline
 * modes (JSON/curl), all keys are pre-generated so all FKs can be filled
 * upfront. For HTTP mode, back-fill PATCHes are used for cyclic dependencies.
 */
const generateWithDependencies = async (
  options: SeedOptions,
  output: OutputOptions,
  onProgress?: ProgressCallback
): Promise<SeedResult> => {
  const startTime = Date.now();
  let totalCreated = 0;
  let totalFailed = 0;
  const allErrors: string[] = [];

  const { resource, count, relatedRecords, fieldsByResource, lookupsByType } = options;
  const plan = buildMultiResourcePlan(resource, count, relatedRecords, fieldsByResource);
  const keyPool: Record<string, string[]> = {};
  const isOffline = output.format === 'json' || output.format === 'curl';
  const curlRecordSets: Array<{ resource: string; records: ReadonlyArray<Record<string, unknown>> }> = [];

  // Pre-generate synthetic keys for offline modes (all FKs can be injected upfront)
  if (isOffline) {
    for (const phase of plan.phases) {
      keyPool[phase.resource] = Array.from({ length: phase.count }, () => randomUUID());
    }
  }

  // Execute each phase in dependency order
  for (const phase of plan.phases) {
    const fields = fieldsByResource[phase.resource];
    if (!fields || fields.length === 0) {
      allErrors.push(`No fields found for resource "${phase.resource}", skipping`);
      continue;
    }

    const generator = getGenerator(phase.resource);
    onProgress?.(`Generating ${phase.count} ${phase.resource} records...`, 0, phase.count);
    const records = [...generator(fields, lookupsByType, phase.count)];

    // Collect FK bindings to inject
    // For offline modes: include deferred (back-fill) FKs since all keys are pre-generated
    const bindings = [...phase.fkBindings];
    if (isOffline) {
      const backFill = plan.backFillPhases.find(b => b.resource === phase.resource);
      if (backFill) bindings.push(...backFill.fkBindings);
    }

    // Assign synthetic PK for offline modes
    const keyField = KEY_FIELD_MAP[phase.resource];
    if (isOffline && keyField) {
      for (let i = 0; i < records.length; i++) {
        records[i][keyField] = keyPool[phase.resource][i];
      }
    }

    // Inject FK values from key pool
    injectForeignKeys(records, bindings, keyPool);

    // Output records
    if (output.format === 'http') {
      const result = await createRecordsViaHttp(
        output.serverUrl,
        phase.resource,
        records,
        options.auth,
        (completed, total) => onProgress?.(`Creating ${phase.resource} records`, completed, total),
        keyField
      );
      totalCreated += result.created;
      totalFailed += result.failed;
      allErrors.push(...result.errors);
      keyPool[phase.resource] = [...result.keys];
    } else if (output.format === 'json') {
      const result = await writeRecordsAsJson(output.outputDir, phase.resource, records, (completed, total) =>
        onProgress?.(`Writing ${phase.resource} JSON files`, completed, total)
      );
      totalCreated += result.created;
      totalFailed += result.failed;
      allErrors.push(...result.errors);
    } else {
      curlRecordSets.push({ resource: phase.resource, records });
      totalCreated += records.length;
    }
  }

  // Back-fill deferred FKs (HTTP only — offline modes already have all FKs)
  if (output.format === 'http') {
    for (const backFill of plan.backFillPhases) {
      const pool = keyPool[backFill.resource];
      if (!pool || pool.length === 0) continue;

      const patches = pool.map(key => {
        const updates: Record<string, unknown> = {};
        for (const binding of backFill.fkBindings) {
          const targetPool = keyPool[binding.targetResource];
          if (targetPool && targetPool.length > 0) {
            updates[binding.fkColumn] = pickRandom(targetPool);
          }
        }
        return { key, updates };
      });

      onProgress?.(`Back-filling ${backFill.resource} FKs...`, 0, patches.length);
      const result = await patchRecordsViaHttp(output.serverUrl, backFill.resource, patches, options.auth, (completed, total) =>
        onProgress?.(`Back-filling ${backFill.resource}`, completed, total)
      );
      totalFailed += result.failed;
      allErrors.push(...result.errors);
    }
  }

  // Generate child collection records
  if (relatedRecords && Object.keys(relatedRecords).length > 0) {
    const parentKeys = keyPool[resource] ?? [];
    if (parentKeys.length > 0) {
      const allRelatedForCurl: Record<string, Record<string, unknown>[]> = {};

      for (const [relatedResource, relatedCount] of Object.entries(relatedRecords)) {
        const relatedFields = fieldsByResource[relatedResource];
        if (!relatedFields || relatedFields.length === 0) {
          allErrors.push(`No fields found for related resource "${relatedResource}", skipping`);
          continue;
        }

        const relatedGenerator = getGenerator(relatedResource);
        onProgress?.(`Generating ${relatedResource} records...`, 0, parentKeys.length);

        for (let parentIdx = 0; parentIdx < parentKeys.length; parentIdx++) {
          const parentKey = parentKeys[parentIdx];
          const records = relatedGenerator(relatedFields, lookupsByType, relatedCount, resource, parentKey);

          if (output.format === 'http') {
            const result = await createRecordsViaHttp(
              output.serverUrl,
              relatedResource,
              records,
              options.auth,
              (completed, total) =>
                onProgress?.(`Creating ${relatedResource} for ${resource} ${parentIdx + 1}/${parentKeys.length}`, completed, total),
              KEY_FIELD_MAP[relatedResource]
            );
            totalCreated += result.created;
            totalFailed += result.failed;
            allErrors.push(...result.errors);
          } else if (output.format === 'json') {
            const result = await writeRecordsAsJson(output.outputDir, relatedResource, records, (completed, total) =>
              onProgress?.(`Writing ${relatedResource} JSON for ${resource} ${parentIdx + 1}/${parentKeys.length}`, completed, total)
            );
            totalCreated += result.created;
            totalFailed += result.failed;
            allErrors.push(...result.errors);
          } else {
            if (!allRelatedForCurl[relatedResource]) allRelatedForCurl[relatedResource] = [];
            allRelatedForCurl[relatedResource].push(...records);
            totalCreated += records.length;
          }
        }
      }

      if (output.format === 'curl') {
        for (const [res, recs] of Object.entries(allRelatedForCurl)) {
          curlRecordSets.push({ resource: res, records: recs });
        }
      }
    }
  }

  // Write curl script
  if (output.format === 'curl') {
    try {
      await generateCurlScript(output.outputFile, output.serverUrl, options.auth, curlRecordSets);
    } catch (err) {
      allErrors.push(`Failed to write curl script: ${err instanceof Error ? err.message : 'Unknown error'}`);
      totalFailed++;
    }
  }

  return {
    created: totalCreated,
    failed: totalFailed,
    errors: allErrors,
    durationMs: Date.now() - startTime
  };
};

/**
 * Generates seed data for a RESO resource and its related records.
 *
 * Supports three output modes:
 * - `http` — POSTs records to an OData server via the Add/Edit API
 * - `json` — Writes records as JSON files to a directory structure
 * - `curl` — Generates a seed.sh script with curl commands
 *
 * When `resolveDependencies` is true, automatically creates dependency
 * resources (Member, Office, etc.) in the correct order with valid FK linkages.
 */
export const generateSeedData = async (options: SeedOptions, output: OutputOptions, onProgress?: ProgressCallback): Promise<SeedResult> => {
  if (options.resolveDependencies) {
    return generateWithDependencies(options, output, onProgress);
  }

  const startTime = Date.now();
  let totalCreated = 0;
  let totalFailed = 0;
  const allErrors: string[] = [];

  const { resource, count, relatedRecords, fieldsByResource, lookupsByType } = options;

  // Get parent resource fields
  const parentFields = fieldsByResource[resource];
  if (!parentFields || parentFields.length === 0) {
    return { created: 0, failed: 0, errors: [`No fields found for resource "${resource}"`], durationMs: 0 };
  }

  // Generate parent records
  const parentGenerator = getGenerator(resource);
  const parentRecords = parentGenerator(parentFields, lookupsByType, count);
  onProgress?.(`Generating ${count} ${resource} records...`, 0, count);

  // For curl output, collect all records then write script at the end
  const curlRecordSets: Array<{ resource: string; records: ReadonlyArray<Record<string, unknown>> }> = [];

  if (output.format === 'curl') {
    curlRecordSets.push({ resource, records: parentRecords });
  }

  // Create parent records and collect their keys for related record generation
  const parentKeys: string[] = [];

  if (output.format === 'http') {
    const result = await createRecordsViaHttp(output.serverUrl, resource, parentRecords, options.auth, (completed, total) =>
      onProgress?.(`Creating ${resource} records`, completed, total)
    );
    totalCreated += result.created;
    totalFailed += result.failed;
    allErrors.push(...result.errors);
    parentKeys.push(...result.keys);
  } else if (output.format === 'json') {
    const result = await writeRecordsAsJson(output.outputDir, resource, parentRecords, (completed, total) =>
      onProgress?.(`Writing ${resource} JSON files`, completed, total)
    );
    totalCreated += result.created;
    totalFailed += result.failed;
    allErrors.push(...result.errors);
    // Use index-based keys for JSON output since server doesn't assign keys
    parentKeys.push(...parentRecords.map((_, i) => `${resource}-${String(i + 1).padStart(4, '0')}`));
  } else {
    // curl mode — keys are synthetic since we're just generating a script
    parentKeys.push(...parentRecords.map((_, i) => `${resource}-${String(i + 1).padStart(4, '0')}`));
    totalCreated += parentRecords.length;
  }

  // Generate related records for each parent
  if (relatedRecords && Object.keys(relatedRecords).length > 0) {
    for (const [relatedResource, relatedCount] of Object.entries(relatedRecords)) {
      const relatedFields = fieldsByResource[relatedResource];
      if (!relatedFields || relatedFields.length === 0) {
        allErrors.push(`No fields found for related resource "${relatedResource}", skipping`);
        continue;
      }

      const relatedGenerator = getGenerator(relatedResource);
      onProgress?.(`Generating ${relatedResource} records for ${parentKeys.length} ${resource} parents...`, 0, parentKeys.length);

      // Collect all related records for curl mode
      const allRelatedRecords: Record<string, unknown>[] = [];

      for (let parentIdx = 0; parentIdx < parentKeys.length; parentIdx++) {
        const parentKey = parentKeys[parentIdx];
        const records = relatedGenerator(relatedFields, lookupsByType, relatedCount, resource, parentKey);

        if (output.format === 'http') {
          const result = await createRecordsViaHttp(output.serverUrl, relatedResource, records, options.auth, (completed, total) =>
            onProgress?.(`Creating ${relatedResource} for ${resource} ${parentIdx + 1}/${parentKeys.length}`, completed, total)
          );
          totalCreated += result.created;
          totalFailed += result.failed;
          allErrors.push(...result.errors);
        } else if (output.format === 'json') {
          const result = await writeRecordsAsJson(output.outputDir, relatedResource, records, (completed, total) =>
            onProgress?.(`Writing ${relatedResource} JSON for ${resource} ${parentIdx + 1}/${parentKeys.length}`, completed, total)
          );
          totalCreated += result.created;
          totalFailed += result.failed;
          allErrors.push(...result.errors);
        } else {
          allRelatedRecords.push(...records);
          totalCreated += records.length;
        }
      }

      if (output.format === 'curl' && allRelatedRecords.length > 0) {
        curlRecordSets.push({ resource: relatedResource, records: allRelatedRecords });
      }
    }
  }

  // Write curl script if in curl mode
  if (output.format === 'curl') {
    try {
      await generateCurlScript(output.outputFile, output.serverUrl, options.auth, curlRecordSets);
    } catch (err) {
      allErrors.push(`Failed to write curl script: ${err instanceof Error ? err.message : 'Unknown error'}`);
      totalFailed++;
    }
  }

  return {
    created: totalCreated,
    failed: totalFailed,
    errors: allErrors,
    durationMs: Date.now() - startTime
  };
};
