/**
 * Metadata loading, parsing, and validation — delegates XML parsing to
 * @reso/odata-client's CSDL parser and type validation to @reso/validation.
 */

import { readFile } from 'node:fs/promises';
import { fetchRawMetadata, parseCsdlXml } from '@reso/odata-client';
import type { CsdlEntityType, CsdlProperty, CsdlSchema } from '@reso/odata-client';
import { type ResoField, type ValidationFailure, validateRecord } from '@reso/validation';
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

/**
 * Converts an EntityProperty to a ResoField for use with @reso/validation.
 * Handles Collection() type syntax by extracting the inner type and setting isCollection.
 */
const toResoField = (prop: EntityProperty, resourceName: string): ResoField => {
  const isCollection = prop.type.startsWith('Collection(');
  const type = isCollection ? prop.type.slice(11, -1) : prop.type;

  return {
    resourceName,
    fieldName: prop.name,
    type,
    nullable: prop.nullable,
    isCollection: isCollection || undefined,
    maxLength: prop.maxLength,
    precision: prop.precision,
    scale: prop.scale,
    annotations: prop.annotations ? Object.entries(prop.annotations).map(([term, value]) => ({ term, value })) : []
  };
};

/** Converts an EntityType's properties to ResoField[] for validation. */
export const toResoFields = (entityType: EntityType): ReadonlyArray<ResoField> =>
  entityType.properties.map(p => toResoField(p, entityType.name));

// ── Public API ──

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
 * Validates a payload against the entity type's metadata using @reso/validation.
 *
 * Performs two levels of validation:
 * 1. Unknown field detection (fields not in metadata)
 * 2. Type/value validation via @reso/validation (type mismatches, negative numerics,
 *    MaxLength, integer enforcement, collection/enum checks)
 *
 * Keys prefixed with `@` (OData annotations) are ignored.
 */
export const validatePayloadAgainstMetadata = (
  payload: Record<string, unknown>,
  entityType: EntityType
): {
  readonly valid: boolean;
  readonly unknownFields: ReadonlyArray<string>;
  readonly failures: ReadonlyArray<ValidationFailure>;
} => {
  const resoFields = toResoFields(entityType);
  const failures = validateRecord(payload, resoFields);
  const unknownFields = failures.filter(f => f.reason.includes('not a recognized field')).map(f => f.field);

  return {
    valid: failures.length === 0,
    unknownFields,
    failures
  };
};
