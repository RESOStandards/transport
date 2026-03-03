import { randomUUID } from 'node:crypto';
import type { RequestHandler } from 'express';
import type { CollectionQueryOptions, DataAccessLayer, ResourceContext } from '../db/data-access.js';
import { buildAnnotations } from './annotations.js';
import { buildODataError, buildValidationError } from './errors.js';
import { setODataHeaders } from './headers.js';
import { validateRequestBody } from './validation.js';

/** Extracts the OData key from a URL path like `/Property('12345')`. */
const extractKey = (path: string): string | undefined => {
  const match = path.match(/\('([^']+)'\)/);
  return match?.[1];
};

/** Determines if the Prefer header requests minimal response. */
const prefersMinimal = (prefer: string | undefined): boolean => prefer?.includes('return=minimal') ?? false;

/** Options shared by all handler factories. */
export interface HandlerContext {
  readonly resourceCtx: ResourceContext;
  readonly dal: DataAccessLayer;
  readonly baseUrl: string;
}

/** Creates a POST handler for creating new records. */
export const createHandler =
  (ctx: HandlerContext): RequestHandler =>
  async (req, res) => {
    try {
      const body = req.body as Record<string, unknown>;

      const failures = validateRequestBody(body, ctx.resourceCtx.fields);
      if (failures.length > 0) {
        setODataHeaders(res);
        res.status(400).json(buildValidationError(failures, 'Create'));
        return;
      }

      const key = randomUUID();
      const record = {
        ...body,
        [ctx.resourceCtx.keyField]: key,
        ModificationTimestamp: new Date().toISOString()
      };

      const row = await ctx.dal.insert(ctx.resourceCtx, record);

      const locationUrl = `${ctx.baseUrl}/${ctx.resourceCtx.resource}('${key}')`;

      if (prefersMinimal(req.headers.prefer as string | undefined)) {
        setODataHeaders(res, {
          entityId: key,
          locationUrl,
          preferenceApplied: 'return=minimal'
        });
        res.status(204).send();
        return;
      }

      setODataHeaders(res, {
        entityId: key,
        locationUrl,
        preferenceApplied: 'return=representation'
      });
      res.status(201).json({
        ...buildAnnotations(ctx.baseUrl, ctx.resourceCtx.resource, key),
        ...row
      });
    } catch (err) {
      setODataHeaders(res);
      res.status(500).json(buildODataError('50000', err instanceof Error ? err.message : 'Internal server error', [], 'Create'));
    }
  };

/** Creates a GET handler for retrieving a single record by key. */
export const readHandler =
  (ctx: HandlerContext): RequestHandler =>
  async (req, res) => {
    try {
      const key = extractKey(req.path);
      if (!key) {
        setODataHeaders(res);
        res
          .status(400)
          .json(buildValidationError([{ field: 'key', reason: "Missing resource key in URL. Use the format /Resource('key')." }], 'Read'));
        return;
      }

      const selectParam = req.query.$select as string | undefined;
      const expandParam = req.query.$expand as string | undefined;

      const row = await ctx.dal.readByKey(ctx.resourceCtx, key, {
        $select: selectParam,
        $expand: expandParam
      });

      if (!row) {
        setODataHeaders(res);
        res.status(404).json(buildODataError('40400', `No ${ctx.resourceCtx.resource} record found with key '${key}'.`, [], 'Read'));
        return;
      }

      setODataHeaders(res);
      res.status(200).json({
        ...buildAnnotations(ctx.baseUrl, ctx.resourceCtx.resource, key),
        ...row
      });
    } catch (err) {
      setODataHeaders(res);
      res.status(500).json(buildODataError('50000', err instanceof Error ? err.message : 'Internal server error', [], 'Read'));
    }
  };

/** Creates a PATCH handler for updating an existing record (merge semantics). */
export const updateHandler =
  (ctx: HandlerContext): RequestHandler =>
  async (req, res) => {
    try {
      const key = extractKey(req.path);
      if (!key) {
        setODataHeaders(res);
        res
          .status(400)
          .json(
            buildValidationError([{ field: 'key', reason: "Missing resource key in URL. Use the format /Resource('key')." }], 'Update')
          );
        return;
      }

      const body = req.body as Record<string, unknown>;

      const failures = validateRequestBody(body, ctx.resourceCtx.fields);
      if (failures.length > 0) {
        setODataHeaders(res);
        res.status(400).json(buildValidationError(failures, 'Update'));
        return;
      }

      const updates = {
        ...body,
        ModificationTimestamp: new Date().toISOString()
      };

      const row = await ctx.dal.update(ctx.resourceCtx, key, updates);
      const locationUrl = `${ctx.baseUrl}/${ctx.resourceCtx.resource}('${key}')`;

      if (!row) {
        // Record doesn't exist — insert it instead (upsert semantics)
        const record = { ...updates, [ctx.resourceCtx.keyField]: key };
        const newRow = await ctx.dal.insert(ctx.resourceCtx, record);

        if (prefersMinimal(req.headers.prefer as string | undefined)) {
          setODataHeaders(res, {
            entityId: key,
            locationUrl,
            preferenceApplied: 'return=minimal'
          });
          res.status(204).send();
          return;
        }

        setODataHeaders(res, {
          entityId: key,
          locationUrl,
          preferenceApplied: 'return=representation'
        });
        res.status(200).json({
          ...buildAnnotations(ctx.baseUrl, ctx.resourceCtx.resource, key),
          ...newRow
        });
        return;
      }

      if (prefersMinimal(req.headers.prefer as string | undefined)) {
        setODataHeaders(res, {
          entityId: key,
          locationUrl,
          preferenceApplied: 'return=minimal'
        });
        res.status(204).send();
        return;
      }

      setODataHeaders(res, {
        entityId: key,
        locationUrl,
        preferenceApplied: 'return=representation'
      });
      res.status(200).json({
        ...buildAnnotations(ctx.baseUrl, ctx.resourceCtx.resource, key),
        ...row
      });
    } catch (err) {
      setODataHeaders(res);
      res.status(500).json(buildODataError('50000', err instanceof Error ? err.message : 'Internal server error', [], 'Update'));
    }
  };

/** Creates a DELETE handler for removing a record by key. */
export const deleteHandler =
  (ctx: HandlerContext): RequestHandler =>
  async (req, res) => {
    try {
      const key = extractKey(req.path);
      if (!key) {
        setODataHeaders(res);
        res.status(400).json(buildODataError('20100', "Missing resource key in URL. Use the format /Resource('key').", [], 'Delete'));
        return;
      }

      const deleted = await ctx.dal.deleteByKey(ctx.resourceCtx, key);
      if (!deleted) {
        setODataHeaders(res);
        res.status(404).json(buildODataError('40400', `No ${ctx.resourceCtx.resource} record found with key '${key}'.`, [], 'Delete'));
        return;
      }

      setODataHeaders(res);
      res.status(204).send();
    } catch (err) {
      setODataHeaders(res);
      res.status(500).json(buildODataError('50000', err instanceof Error ? err.message : 'Internal server error', [], 'Delete'));
    }
  };

/** Creates a GET handler for querying a collection of entities. */
export const collectionHandler =
  (ctx: HandlerContext): RequestHandler =>
  async (req, res) => {
    try {
      const options: CollectionQueryOptions = {
        ...(req.query.$filter && { $filter: req.query.$filter as string }),
        ...(req.query.$select && { $select: req.query.$select as string }),
        ...(req.query.$orderby && { $orderby: req.query.$orderby as string }),
        ...(req.query.$top && { $top: Number(req.query.$top) }),
        ...(req.query.$skip && { $skip: Number(req.query.$skip) }),
        ...(req.query.$count === 'true' && { $count: true }),
        ...(req.query.$expand && { $expand: req.query.$expand as string })
      };

      const result = await ctx.dal.queryCollection(ctx.resourceCtx, options);

      const body: Record<string, unknown> = {
        '@odata.context': `${ctx.baseUrl}/$metadata#${ctx.resourceCtx.resource}`,
        value: result.value
      };
      if (result.count !== undefined) {
        body['@odata.count'] = result.count;
      }

      setODataHeaders(res);
      res.status(200).json(body);
    } catch (err) {
      setODataHeaders(res);
      res.status(500).json(buildODataError('50000', err instanceof Error ? err.message : 'Internal server error', [], 'Query'));
    }
  };
