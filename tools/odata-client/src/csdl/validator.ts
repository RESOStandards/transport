/**
 * CSDL schema validator — checks structural requirements of a parsed
 * OData metadata document.
 */

import type { CsdlSchema, CsdlValidationError, CsdlValidationResult } from './types.js';

/** Valid OData primitive type prefixes. */
const EDM_TYPES = new Set([
  'Edm.Binary',
  'Edm.Boolean',
  'Edm.Byte',
  'Edm.Date',
  'Edm.DateTimeOffset',
  'Edm.Decimal',
  'Edm.Double',
  'Edm.Duration',
  'Edm.Guid',
  'Edm.Int16',
  'Edm.Int32',
  'Edm.Int64',
  'Edm.SByte',
  'Edm.Single',
  'Edm.Stream',
  'Edm.String',
  'Edm.TimeOfDay',
  'Edm.Geography',
  'Edm.GeographyPoint',
  'Edm.GeographyLineString',
  'Edm.GeographyPolygon',
  'Edm.GeographyMultiPoint',
  'Edm.GeographyMultiLineString',
  'Edm.GeographyMultiPolygon',
  'Edm.GeographyCollection',
  'Edm.Geometry',
  'Edm.GeometryPoint',
  'Edm.GeometryLineString',
  'Edm.GeometryPolygon',
  'Edm.GeometryMultiPoint',
  'Edm.GeometryMultiLineString',
  'Edm.GeometryMultiPolygon',
  'Edm.GeometryCollection'
]);

const isEdmPrimitive = (type: string): boolean => EDM_TYPES.has(type);

const isCollectionType = (type: string): boolean => type.startsWith('Collection(') && type.endsWith(')');

const unwrapCollection = (type: string): string => type.slice('Collection('.length, -1);

/**
 * Validate a parsed CSDL schema for structural correctness.
 *
 * Checks:
 * - Namespace is present
 * - Entity types have at least one key property (unless abstract or derived)
 * - Key properties exist in the entity type's properties
 * - Property types are valid (Edm primitive, Collection, or namespace-qualified)
 * - Entity sets reference valid entity types (if entity container exists)
 * - Navigation property targets reference valid entity types
 * - BaseType references are valid (warning)
 */
export const validateCsdl = (schema: CsdlSchema): CsdlValidationResult => {
  const errors: CsdlValidationError[] = [];

  // Check namespace
  if (!schema.namespace) {
    errors.push({
      path: 'Schema',
      message: 'Schema namespace is missing or empty'
    });
  }

  // Build set of known type names for reference checking
  const knownTypeNames = new Set([
    ...schema.entityTypes.map(et => `${schema.namespace}.${et.name}`),
    ...schema.enumTypes.map(et => `${schema.namespace}.${et.name}`),
    ...schema.complexTypes.map(ct => `${schema.namespace}.${ct.name}`)
  ]);

  /**
   * Check whether a property type is valid: either an Edm primitive,
   * a known schema type, or an externally namespace-qualified type.
   */
  const validatePropertyType = (propType: string, propPath: string): void => {
    let typeToCheck = propType;

    if (isCollectionType(typeToCheck)) {
      typeToCheck = unwrapCollection(typeToCheck);
    }

    if (!isEdmPrimitive(typeToCheck) && !knownTypeNames.has(typeToCheck)) {
      // Allow namespace-qualified types we haven't seen (external references)
      if (!typeToCheck.includes('.')) {
        errors.push({
          path: propPath,
          message: `Property type '${propType}' is not a valid Edm primitive or known type`
        });
      }
    }
  };

  // Validate entity types
  for (const entityType of schema.entityTypes) {
    const etPath = `EntityType(${entityType.name})`;

    // Abstract types and derived types (with baseType) may omit keys
    if (entityType.key.length === 0 && !entityType.abstract && !entityType.baseType) {
      errors.push({
        path: etPath,
        message: 'Entity type has no key properties defined'
      });
    }

    const propertyNames = new Set(entityType.properties.map(p => p.name));

    // Check key properties exist
    for (const keyProp of entityType.key) {
      if (!propertyNames.has(keyProp)) {
        errors.push({
          path: `${etPath}/Key`,
          message: `Key property '${keyProp}' does not exist in entity type properties`
        });
      }
    }

    // Check property types are valid
    for (const prop of entityType.properties) {
      validatePropertyType(prop.type, `${etPath}/Property(${prop.name})`);
    }

    // Validate navigation property targets
    for (const navProp of entityType.navigationProperties) {
      const targetType = isCollectionType(navProp.type) ? unwrapCollection(navProp.type) : navProp.type;
      if (!knownTypeNames.has(targetType) && !targetType.includes('.')) {
        errors.push({
          path: `${etPath}/NavigationProperty(${navProp.name})`,
          message: `Navigation property references unknown entity type '${targetType}'`
        });
      }
    }

    // Validate baseType reference (warning-level: only flag if not namespace-qualified)
    if (entityType.baseType && !knownTypeNames.has(entityType.baseType)) {
      if (!entityType.baseType.includes('.')) {
        errors.push({
          path: etPath,
          message: `BaseType '${entityType.baseType}' is not a known entity type`
        });
      }
    }
  }

  // Validate complex types
  for (const complexType of schema.complexTypes) {
    const ctPath = `ComplexType(${complexType.name})`;

    for (const prop of complexType.properties) {
      validatePropertyType(prop.type, `${ctPath}/Property(${prop.name})`);
    }

    for (const navProp of complexType.navigationProperties) {
      const targetType = isCollectionType(navProp.type) ? unwrapCollection(navProp.type) : navProp.type;
      if (!knownTypeNames.has(targetType) && !targetType.includes('.')) {
        errors.push({
          path: `${ctPath}/NavigationProperty(${navProp.name})`,
          message: `Navigation property references unknown entity type '${targetType}'`
        });
      }
    }
  }

  // Validate entity container references
  if (schema.entityContainer) {
    for (const entitySet of schema.entityContainer.entitySets) {
      if (!knownTypeNames.has(entitySet.entityType)) {
        // Only warn if the type doesn't look namespace-qualified
        if (!entitySet.entityType.includes('.')) {
          errors.push({
            path: `EntityContainer/EntitySet(${entitySet.name})`,
            message: `Entity set references unknown entity type '${entitySet.entityType}'`
          });
        }
      }
    }
  }

  return { valid: errors.length === 0, errors };
};
