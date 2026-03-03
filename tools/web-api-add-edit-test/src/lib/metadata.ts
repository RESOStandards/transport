/**
 * Metadata loading, parsing, and validation — delegates XML parsing to
 * @reso/odata-client's CSDL parser and adapts the types for the test tool.
 */

import { readFile } from 'node:fs/promises';
import { fetchRawMetadata, parseCsdlXml } from '@reso/odata-client';
import type { CsdlEntityType, CsdlProperty, CsdlSchema } from '@reso/odata-client';
import type { EntityProperty, EntityType, ParsedMetadata } from './types.js';

// ── Type adapters (CsdlEntityType → EntityType) ──

/** Convert a CsdlProperty to the test tool's EntityProperty. */
const adaptProperty = (prop: CsdlProperty): EntityProperty => ({
  name: prop.name,
  type: prop.type,
  ...(prop.nullable !== undefined && { nullable: prop.nullable }),
  ...(prop.maxLength !== undefined && { maxLength: prop.maxLength }),
  ...(prop.precision !== undefined && { precision: prop.precision }),
  ...(prop.scale !== undefined && { scale: prop.scale }),
  ...(prop.annotations !== undefined && { annotations: prop.annotations })
});

/** Convert a CsdlEntityType to the test tool's EntityType. */
const adaptEntityType = (et: CsdlEntityType): EntityType => ({
  name: et.name,
  keyProperties: [...et.key],
  properties: et.properties.map(adaptProperty)
});

/** Convert a CsdlSchema to the test tool's ParsedMetadata. */
const adaptSchema = (schema: CsdlSchema): ParsedMetadata => ({
  namespace: schema.namespace,
  entityTypes: schema.entityTypes.map(adaptEntityType)
});

// ── Public API (same interface as before) ──

/**
 * Fetches OData XML metadata from a server's `/$metadata` endpoint.
 * Requires a bearer token for authorization.
 * Returns the raw XML string.
 */
export const fetchMetadata = async (serverUrl: string, authToken: string): Promise<string> =>
  fetchRawMetadata(serverUrl.replace(/\/$/, ''), authToken);

/** Reads OData XML metadata from a local file. */
export const loadMetadataFromFile = async (filePath: string): Promise<string> => readFile(filePath, 'utf-8');

/**
 * Parses an OData EDMX XML metadata document into a structured representation.
 * Delegates to @reso/odata-client's parseCsdlXml and adapts the types.
 */
export const parseMetadataXml = (xml: string): ParsedMetadata => {
  const schema = parseCsdlXml(xml);
  return adaptSchema(schema);
};

/** Finds an entity type by name in parsed metadata. Returns undefined if not found. */
export const getEntityType = (metadata: ParsedMetadata, resourceName: string): EntityType | undefined =>
  metadata.entityTypes.find(et => et.name === resourceName);

/**
 * Validates that all fields in a payload exist in the entity type's property definitions.
 * Keys prefixed with `@` (OData annotations) are ignored.
 * Returns the list of unknown fields, if any.
 */
export const validatePayloadAgainstMetadata = (
  payload: Record<string, unknown>,
  entityType: EntityType
): { readonly valid: boolean; readonly unknownFields: ReadonlyArray<string> } => {
  const propertyNames = new Set(entityType.properties.map(p => p.name));
  const unknownFields = Object.keys(payload)
    .filter(key => !key.startsWith('@'))
    .filter(key => !propertyNames.has(key));

  return {
    valid: unknownFields.length === 0,
    unknownFields
  };
};
