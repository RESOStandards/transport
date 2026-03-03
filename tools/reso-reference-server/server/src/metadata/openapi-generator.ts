import { getFieldsForResource, getKeyFieldForResource, isEnumType } from './loader.js';
import type { ResoField, ResoMetadata } from './types.js';

/** Maps a RESO field type to an OpenAPI schema type. */
const toOpenApiType = (field: ResoField): { type: string; format?: string; items?: { type: string } } => {
  if (field.isCollection) {
    return { type: 'array', items: { type: 'string' } };
  }
  if (isEnumType(field.type)) {
    return { type: 'string' };
  }

  switch (field.type) {
    case 'Edm.String':
      return { type: 'string' };
    case 'Edm.Int64':
    case 'Edm.Int32':
    case 'Edm.Int16':
    case 'Edm.Byte':
      return { type: 'integer' };
    case 'Edm.Decimal':
    case 'Edm.Double':
    case 'Edm.Single':
      return { type: 'number' };
    case 'Edm.Boolean':
      return { type: 'boolean' };
    case 'Edm.Date':
      return { type: 'string', format: 'date' };
    case 'Edm.DateTimeOffset':
      return { type: 'string', format: 'date-time' };
    default:
      return { type: 'string' };
  }
};

/** Builds an OpenAPI schema properties object from field definitions. */
const buildSchemaProperties = (fields: ReadonlyArray<ResoField>): Record<string, unknown> =>
  Object.fromEntries(
    fields.map(field => {
      const schema = toOpenApiType(field);
      return [
        field.fieldName,
        {
          ...schema,
          ...(field.maxLength !== undefined && { maxLength: field.maxLength }),
          ...(field.nullable !== undefined && { nullable: field.nullable })
        }
      ];
    })
  );

/** Generates CRUD path operations for a resource. */
const buildResourcePaths = (resource: string, keyField: string, fields: ReadonlyArray<ResoField>): Record<string, unknown> => {
  const schemaRef = `#/components/schemas/${resource}`;

  return {
    [`/${resource}`]: {
      post: {
        tags: [resource],
        summary: `Create a new ${resource} record`,
        requestBody: {
          required: true,
          content: {
            'application/json': { schema: { $ref: schemaRef } }
          }
        },
        parameters: [
          {
            name: 'Prefer',
            in: 'header',
            schema: {
              type: 'string',
              enum: ['return=representation', 'return=minimal']
            }
          }
        ],
        responses: {
          '201': {
            description: 'Created (return=representation)',
            content: { 'application/json': { schema: { $ref: schemaRef } } }
          },
          '204': { description: 'Created (return=minimal)' },
          '400': { description: 'Validation error' }
        }
      }
    },
    [`/${resource}('{${keyField}}')`]: {
      get: {
        tags: [resource],
        summary: `Get a ${resource} record by key`,
        parameters: [
          {
            name: keyField,
            in: 'path',
            required: true,
            schema: { type: 'string' }
          }
        ],
        responses: {
          '200': {
            description: 'Success',
            content: { 'application/json': { schema: { $ref: schemaRef } } }
          },
          '404': { description: 'Not found' }
        }
      },
      patch: {
        tags: [resource],
        summary: `Update a ${resource} record`,
        parameters: [
          {
            name: keyField,
            in: 'path',
            required: true,
            schema: { type: 'string' }
          },
          {
            name: 'Prefer',
            in: 'header',
            schema: {
              type: 'string',
              enum: ['return=representation', 'return=minimal']
            }
          }
        ],
        requestBody: {
          required: true,
          content: {
            'application/json': { schema: { $ref: schemaRef } }
          }
        },
        responses: {
          '200': {
            description: 'Updated (return=representation)',
            content: { 'application/json': { schema: { $ref: schemaRef } } }
          },
          '204': { description: 'Updated (return=minimal)' },
          '400': { description: 'Validation error' }
        }
      },
      delete: {
        tags: [resource],
        summary: `Delete a ${resource} record`,
        parameters: [
          {
            name: keyField,
            in: 'path',
            required: true,
            schema: { type: 'string' }
          }
        ],
        responses: {
          '204': { description: 'Deleted' },
          '404': { description: 'Not found' }
        }
      }
    }
  };
};

/**
 * Generates an OpenAPI 3.0.3 specification from RESO metadata.
 * Includes CRUD paths and component schemas for all target resources.
 */
export const generateOpenApiSpec = (
  metadata: ResoMetadata,
  targetResources: ReadonlyArray<string>,
  serverUrl: string
): Record<string, unknown> => {
  const paths: Record<string, unknown> = {};
  const schemas: Record<string, unknown> = {};

  for (const resource of targetResources) {
    const fields = getFieldsForResource(metadata, resource);
    const keyField = getKeyFieldForResource(resource);
    if (!keyField || fields.length === 0) continue;

    const resourcePaths = buildResourcePaths(resource, keyField, fields);
    Object.assign(paths, resourcePaths);

    schemas[resource] = {
      type: 'object',
      properties: buildSchemaProperties(fields)
    };
  }

  return {
    openapi: '3.0.3',
    info: {
      title: 'RESO Reference OData Server',
      description: `RESO Data Dictionary v${metadata.version} reference implementation`,
      version: metadata.version
    },
    servers: [{ url: serverUrl }],
    paths,
    components: { schemas }
  };
};
