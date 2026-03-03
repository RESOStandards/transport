/**
 * Mock OData server route handlers for Add/Edit compliance testing.
 *
 * Uses an in-memory Map as a record store. Negative numeric field values
 * trigger 400 validation errors (matching the spec's known-bad payload convention).
 */
import type { Request, Response } from "express";

/** In-memory store for created/updated records, keyed by record ID. */
const store = new Map<string, Record<string, unknown>>();

/** Extracts the OData key from a URL path like `/Property('12345')`. Returns undefined if no key is present. */
const extractKey = (path: string): string | undefined => {
  const match = path.match(/\('([^']+)'\)/);
  return match?.[1];
};

/** Generates a weak ETag from the current timestamp, base64-encoded. */
const generateEtag = (): string =>
  `W/"${Buffer.from(new Date().toISOString()).toString("base64")}"`;

/** Finds the first field with a negative numeric value in the request body. Used to trigger validation errors for known-bad payloads. */
const findNegativeNumericField = (
  body: Record<string, unknown>,
): [string, unknown] | undefined =>
  Object.entries(body).find(
    ([, v]) => typeof v === "number" && v < 0,
  );

/** Constructs an OData-compliant error response body with error.code, error.message, and error.details. */
const makeErrorResponse = (
  target: string,
  fieldName: string,
  message: string,
): Record<string, unknown> => ({
  error: {
    code: "20100",
    message: "Validation failed",
    target,
    details: [
      {
        code: "30212",
        target: fieldName,
        message,
      },
    ],
  },
});

/** Returns an Express handler that serves the OData XML metadata document at /$metadata. */
export const handleMetadata = (metadataXml: string) =>
  (_req: Request, res: Response): void => {
    res.type("application/xml").send(metadataXml);
  };

/**
 * Handles POST requests to create a new record. Generates a timestamp-based key,
 * stores the record, and returns 201 (representation) or 204 (minimal) based on
 * the Prefer header. Returns 400 with OData error if any numeric field is negative.
 */
export const handleCreate = (resource: string) =>
  (req: Request, res: Response): void => {
    const prefer = req.headers["prefer"] as string | undefined;
    const body = req.body as Record<string, unknown>;

    const negativeField = findNegativeNumericField(body);
    if (negativeField) {
      res.set("OData-Version", "4.01");
      res.status(400).json(
        makeErrorResponse(
          "Create",
          negativeField[0],
          `${negativeField[0]} must be greater than or equal to 0`,
        ),
      );
      return;
    }

    const key = String(Date.now());
    const record: Record<string, unknown> = {
      ...body,
      ListingKey: key,
      StandardStatus: "Coming Soon",
      ModificationTimestamp: new Date().toISOString(),
    };
    store.set(key, record);

    const port = req.socket.localPort ?? 8800;
    const locationUrl = `http://localhost:${port}/${resource}('${key}')`;

    res.set({
      "OData-Version": "4.01",
      EntityId: key,
      Location: locationUrl,
    });

    if (prefer?.includes("return=minimal")) {
      res.set("Preference-Applied", "return=minimal");
      res.status(204).send();
      return;
    }

    res.set("Preference-Applied", "return=representation");
    res.status(201).json({
      "@odata.context": `http://localhost:${port}/$metadata#${resource}/$entity`,
      "@odata.id": locationUrl,
      "@odata.editLink": locationUrl,
      "@odata.etag": generateEtag(),
      ...record,
    });
  };

/**
 * Handles PATCH requests to update an existing record. Merges the request body
 * with the existing record (or creates one if not found). Returns 200 (representation)
 * or 204 (minimal) based on the Prefer header. Returns 400 if any numeric field is negative.
 */
export const handleUpdate = (resource: string) =>
  (req: Request, res: Response): void => {
    const prefer = req.headers["prefer"] as string | undefined;
    const body = req.body as Record<string, unknown>;
    const key = extractKey(req.path);

    if (!key) {
      res.set("OData-Version", "4.01");
      res.status(400).json(
        makeErrorResponse("Update", "key", "Missing resource key in URL"),
      );
      return;
    }

    const negativeField = findNegativeNumericField(body);
    if (negativeField) {
      res.set("OData-Version", "4.01");
      res.status(400).json(
        makeErrorResponse(
          "Update",
          negativeField[0],
          `${negativeField[0]} must be greater than or equal to 0`,
        ),
      );
      return;
    }

    // Merge with existing record or create new one
    const existing = store.get(key) ?? { ListingKey: key };
    const record: Record<string, unknown> = {
      ...existing,
      ...body,
      ModificationTimestamp: new Date().toISOString(),
    };
    store.set(key, record);

    const port = req.socket.localPort ?? 8800;
    const locationUrl = `http://localhost:${port}/${resource}('${key}')`;

    res.set({
      "OData-Version": "4.01",
      EntityId: key,
      Location: locationUrl,
    });

    if (prefer?.includes("return=minimal")) {
      res.set("Preference-Applied", "return=minimal");
      res.status(204).send();
      return;
    }

    res.set("Preference-Applied", "return=representation");
    res.status(200).json({
      "@odata.context": `http://localhost:${port}/$metadata#${resource}/$entity`,
      "@odata.id": locationUrl,
      "@odata.editLink": locationUrl,
      "@odata.etag": generateEtag(),
      ...record,
    });
  };

/** Handles DELETE requests. Removes the record from the store and returns 204, or 404 if not found. */
export const handleDelete = () =>
  (req: Request, res: Response): void => {
    const key = extractKey(req.path);

    if (!key || !store.has(key)) {
      res.status(404);
      res.set("OData-Version", "4.01");
      res.send();
      return;
    }

    store.delete(key);
    res.set("OData-Version", "4.01");
    res.status(204).send();
  };

/** Handles GET requests for a single record by key. Returns 200 with OData annotations, or 404 if not found. */
export const handleGet = (resource: string) =>
  (req: Request, res: Response): void => {
    const key = extractKey(req.path);

    if (!key || !store.has(key)) {
      res.set("OData-Version", "4.01");
      res.status(404).send();
      return;
    }

    const record = store.get(key)!;
    const port = req.socket.localPort ?? 8800;
    const locationUrl = `http://localhost:${port}/${resource}('${key}')`;

    res.set("OData-Version", "4.01");
    res.status(200).json({
      "@odata.context": `http://localhost:${port}/$metadata#${resource}/$entity`,
      "@odata.id": locationUrl,
      "@odata.editLink": locationUrl,
      "@odata.etag": generateEtag(),
      ...record,
    });
  };

/** Clears all records from the in-memory store. Used in tests to reset state between runs. */
export const resetStore = (): void => {
  store.clear();
};

/** Pre-populates a record in the store for testing. Used to set up known records before running update/delete scenarios. */
export const seedStore = (key: string, record: Record<string, unknown>): void => {
  store.set(key, record);
};

/**
 * Handles OAuth2 Client Credentials token requests at /oauth/token.
 * Accepts any client_id/client_secret and returns a mock access token.
 * Used for testing the OAuth2 flow without a real authorization server.
 */
export const handleTokenEndpoint = () =>
  (_req: Request, res: Response): void => {
    res.json({
      access_token: "mock-access-token",
      token_type: "Bearer",
      expires_in: 3600,
    });
  };
