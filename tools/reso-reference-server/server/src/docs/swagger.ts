import { Router } from "express";
import swaggerUi from "swagger-ui-express";

/**
 * Creates an Express router that serves Swagger UI at /api-docs.
 * Also serves the raw OpenAPI JSON spec at /api-docs/spec.json.
 */
export const createSwaggerRouter = (spec: Record<string, unknown>): Router => {
  const router = Router();

  router.get("/api-docs/spec.json", (_req, res) => {
    res.json(spec);
  });

  router.use("/api-docs", swaggerUi.serve, swaggerUi.setup(spec));

  return router;
};
