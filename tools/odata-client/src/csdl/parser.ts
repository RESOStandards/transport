/**
 * CSDL/EDMX XML parser — converts OData metadata XML into structured types.
 *
 * Moved from certification/add-edit/src/lib/metadata.ts and enhanced with
 * enum type, entity container, and NavigationProperty extraction. Uses
 * fast-xml-parser with the same options for compatibility with existing
 * RESO tooling.
 *
 * @see https://docs.oasis-open.org/odata/odata-csdl-xml/v4.01/odata-csdl-xml-v4.01.html
 */

import { XMLParser } from 'fast-xml-parser';
import type {
  CsdlAction,
  CsdlActionImport,
  CsdlComplexType,
  CsdlEntityContainer,
  CsdlEntitySet,
  CsdlEntityType,
  CsdlEnumMember,
  CsdlEnumType,
  CsdlFunction,
  CsdlFunctionImport,
  CsdlNavigationProperty,
  CsdlNavigationPropertyBinding,
  CsdlParameter,
  CsdlProperty,
  CsdlReferentialConstraint,
  CsdlReturnType,
  CsdlSchema,
  CsdlSingleton
} from './types.js';

/**
 * Parser options for fast-xml-parser.
 * `isArray` forces certain elements to always be arrays even when only
 * one child exists, preventing inconsistent shapes.
 */
const xmlParserOptions = {
  ignoreAttributes: false,
  attributeNamePrefix: '@_',
  isArray: (name: string) =>
    [
      'EntityType',
      'EnumType',
      'ComplexType',
      'Member',
      'Property',
      'NavigationProperty',
      'PropertyRef',
      'Annotation',
      'EntitySet',
      'NavigationPropertyBinding',
      'Action',
      'Function',
      'Parameter',
      'Singleton',
      'ActionImport',
      'FunctionImport',
      'ReferentialConstraint'
    ].includes(name)
};

/**
 * Check if a type string is a Collection wrapper: Collection(Namespace.TypeName)
 */
const isCollectionType = (type: string): boolean => type.startsWith('Collection(') && type.endsWith(')');

/**
 * Unwrap Collection(X) → X
 */
const unwrapCollectionType = (type: string): string => type.slice('Collection('.length, -1);

/**
 * Extract the unqualified type name from a potentially namespace-qualified
 * and/or Collection-wrapped type string.
 * e.g. "Collection(org.reso.metadata.Media)" → "Media"
 * e.g. "org.reso.metadata.Property" → "Property"
 */
const extractTypeName = (type: string): string => {
  const unwrapped = isCollectionType(type) ? unwrapCollectionType(type) : type;
  const dotIndex = unwrapped.lastIndexOf('.');
  return dotIndex >= 0 ? unwrapped.slice(dotIndex + 1) : unwrapped;
};

const parseProperties = (rawProperties: ReadonlyArray<Record<string, unknown>>): ReadonlyArray<CsdlProperty> =>
  rawProperties.map(rawProp => {
    const annotations: Record<string, string> = {};
    const rawAnnotations = (rawProp.Annotation as ReadonlyArray<Record<string, string>> | undefined) ?? [];
    for (const ann of rawAnnotations) {
      const term = ann['@_Term'];
      const value = ann['@_String'] ?? ann['@_Bool'] ?? '';
      if (term) {
        annotations[term] = value;
      }
    }

    return {
      name: rawProp['@_Name'] as string,
      type: rawProp['@_Type'] as string,
      ...(rawProp['@_Nullable'] !== undefined && {
        nullable: rawProp['@_Nullable'] === 'true'
      }),
      ...(rawProp['@_MaxLength'] !== undefined && {
        maxLength: Number(rawProp['@_MaxLength'])
      }),
      ...(rawProp['@_Precision'] !== undefined && {
        precision: Number(rawProp['@_Precision'])
      }),
      ...(rawProp['@_Scale'] !== undefined && {
        scale: Number(rawProp['@_Scale'])
      }),
      ...(Object.keys(annotations).length > 0 && { annotations })
    };
  });

/**
 * Parse ReferentialConstraint child elements from a NavigationProperty.
 */
const parseReferentialConstraints = (rawConstraints: ReadonlyArray<Record<string, unknown>>): ReadonlyArray<CsdlReferentialConstraint> =>
  rawConstraints.map(rc => ({
    property: rc['@_Property'] as string,
    referencedProperty: rc['@_ReferencedProperty'] as string
  }));

/**
 * Parse NavigationProperty elements from an entity type or complex type.
 *
 * NavigationProperty defines relationships between entity types. The type
 * attribute may be a simple qualified name (single entity) or wrapped in
 * Collection() for to-many relationships.
 *
 * @see https://docs.oasis-open.org/odata/odata-csdl-xml/v4.01/odata-csdl-xml-v4.01.html#sec_NavigationProperty
 */
const parseNavigationProperties = (rawNavProps: ReadonlyArray<Record<string, unknown>>): ReadonlyArray<CsdlNavigationProperty> =>
  rawNavProps.map(rawNav => {
    const name = rawNav['@_Name'] as string;
    const type = rawNav['@_Type'] as string;
    const collection = isCollectionType(type);
    const entityTypeName = extractTypeName(type);

    const rawConstraints = (rawNav.ReferentialConstraint as ReadonlyArray<Record<string, unknown>> | undefined) ?? [];

    return {
      name,
      type,
      isCollection: collection,
      entityTypeName,
      ...(rawNav['@_Nullable'] !== undefined && {
        nullable: rawNav['@_Nullable'] === 'true'
      }),
      ...(rawNav['@_Partner'] !== undefined && {
        partner: rawNav['@_Partner'] as string
      }),
      ...(rawNav['@_ContainsTarget'] !== undefined && {
        containsTarget: rawNav['@_ContainsTarget'] === 'true'
      }),
      ...(rawConstraints.length > 0 && {
        referentialConstraints: parseReferentialConstraints(rawConstraints)
      })
    };
  });

const parseEntityTypes = (rawEntityTypes: ReadonlyArray<Record<string, unknown>>): ReadonlyArray<CsdlEntityType> =>
  rawEntityTypes.map(rawEntity => {
    const name = rawEntity['@_Name'] as string;

    const keyRefs = ((rawEntity.Key as Record<string, unknown>)?.PropertyRef as ReadonlyArray<Record<string, string>> | undefined) ?? [];
    const key = keyRefs.map(ref => ref['@_Name']);

    const rawProperties = (rawEntity.Property as ReadonlyArray<Record<string, unknown>>) ?? [];
    const rawNavProperties = (rawEntity.NavigationProperty as ReadonlyArray<Record<string, unknown>>) ?? [];

    return {
      name,
      key,
      properties: parseProperties(rawProperties),
      navigationProperties: parseNavigationProperties(rawNavProperties),
      ...(rawEntity['@_BaseType'] !== undefined && {
        baseType: rawEntity['@_BaseType'] as string
      }),
      ...(rawEntity['@_Abstract'] !== undefined && {
        abstract: rawEntity['@_Abstract'] === 'true'
      }),
      ...(rawEntity['@_OpenType'] !== undefined && {
        openType: rawEntity['@_OpenType'] === 'true'
      }),
      ...(rawEntity['@_HasStream'] !== undefined && {
        hasStream: rawEntity['@_HasStream'] === 'true'
      })
    };
  });

const parseComplexTypes = (rawComplexTypes: ReadonlyArray<Record<string, unknown>>): ReadonlyArray<CsdlComplexType> =>
  rawComplexTypes.map(rawComplex => {
    const name = rawComplex['@_Name'] as string;

    const rawProperties = (rawComplex.Property as ReadonlyArray<Record<string, unknown>>) ?? [];
    const rawNavProperties = (rawComplex.NavigationProperty as ReadonlyArray<Record<string, unknown>>) ?? [];

    return {
      name,
      properties: parseProperties(rawProperties),
      navigationProperties: parseNavigationProperties(rawNavProperties),
      ...(rawComplex['@_BaseType'] !== undefined && {
        baseType: rawComplex['@_BaseType'] as string
      }),
      ...(rawComplex['@_Abstract'] !== undefined && {
        abstract: rawComplex['@_Abstract'] === 'true'
      }),
      ...(rawComplex['@_OpenType'] !== undefined && {
        openType: rawComplex['@_OpenType'] === 'true'
      })
    };
  });

const parseEnumTypes = (rawEnumTypes: ReadonlyArray<Record<string, unknown>>): ReadonlyArray<CsdlEnumType> =>
  rawEnumTypes.map(rawEnum => {
    const name = rawEnum['@_Name'] as string;
    const rawMembers = (rawEnum.Member as ReadonlyArray<Record<string, string>> | undefined) ?? [];
    const members: ReadonlyArray<CsdlEnumMember> = rawMembers.map(m => ({
      name: m['@_Name'],
      ...(m['@_Value'] !== undefined && { value: m['@_Value'] })
    }));
    return {
      name,
      members,
      ...(rawEnum['@_UnderlyingType'] !== undefined && {
        underlyingType: rawEnum['@_UnderlyingType'] as string
      }),
      ...(rawEnum['@_IsFlags'] !== undefined && {
        isFlags: rawEnum['@_IsFlags'] === 'true'
      })
    };
  });

/**
 * Parse NavigationPropertyBinding elements from an EntitySet or Singleton.
 */
const parseNavigationPropertyBindings = (
  rawBindings: ReadonlyArray<Record<string, unknown>>
): ReadonlyArray<CsdlNavigationPropertyBinding> =>
  rawBindings.map(b => ({
    path: b['@_Path'] as string,
    target: b['@_Target'] as string
  }));

/**
 * Parse Parameter elements from an Action or Function.
 */
const parseParameters = (rawParams: ReadonlyArray<Record<string, unknown>>): ReadonlyArray<CsdlParameter> =>
  rawParams.map(p => ({
    name: p['@_Name'] as string,
    type: p['@_Type'] as string,
    ...(p['@_Nullable'] !== undefined && {
      nullable: p['@_Nullable'] === 'true'
    })
  }));

/**
 * Parse a ReturnType element from an Action or Function.
 */
const parseReturnType = (rawReturn: Record<string, unknown> | undefined): CsdlReturnType | undefined => {
  if (!rawReturn) return undefined;
  return {
    type: rawReturn['@_Type'] as string,
    ...(rawReturn['@_Nullable'] !== undefined && {
      nullable: rawReturn['@_Nullable'] === 'true'
    })
  };
};

const parseActions = (rawActions: ReadonlyArray<Record<string, unknown>>): ReadonlyArray<CsdlAction> =>
  rawActions.map(rawAction => {
    const rawParams = (rawAction.Parameter as ReadonlyArray<Record<string, unknown>> | undefined) ?? [];
    const rawReturn = rawAction.ReturnType as Record<string, unknown> | undefined;

    return {
      name: rawAction['@_Name'] as string,
      ...(rawAction['@_IsBound'] !== undefined && {
        isBound: rawAction['@_IsBound'] === 'true'
      }),
      ...(rawAction['@_EntitySetPath'] !== undefined && {
        entitySetPath: rawAction['@_EntitySetPath'] as string
      }),
      parameters: parseParameters(rawParams),
      ...(rawReturn !== undefined && {
        returnType: parseReturnType(rawReturn) as CsdlReturnType
      })
    };
  });

const parseFunctions = (rawFunctions: ReadonlyArray<Record<string, unknown>>): ReadonlyArray<CsdlFunction> =>
  rawFunctions.map(rawFunc => {
    const rawParams = (rawFunc.Parameter as ReadonlyArray<Record<string, unknown>> | undefined) ?? [];
    const rawReturn = rawFunc.ReturnType as Record<string, unknown> | undefined;

    return {
      name: rawFunc['@_Name'] as string,
      ...(rawFunc['@_IsBound'] !== undefined && {
        isBound: rawFunc['@_IsBound'] === 'true'
      }),
      ...(rawFunc['@_IsComposable'] !== undefined && {
        isComposable: rawFunc['@_IsComposable'] === 'true'
      }),
      ...(rawFunc['@_EntitySetPath'] !== undefined && {
        entitySetPath: rawFunc['@_EntitySetPath'] as string
      }),
      parameters: parseParameters(rawParams),
      returnType: parseReturnType(rawReturn) as CsdlReturnType
    };
  });

const parseEntityContainer = (rawContainer: Record<string, unknown> | undefined): CsdlEntityContainer | undefined => {
  if (!rawContainer) return undefined;

  const name = (rawContainer['@_Name'] as string) ?? 'Default';

  // Entity sets
  const rawEntitySets = (rawContainer.EntitySet as ReadonlyArray<Record<string, unknown>> | undefined) ?? [];
  const entitySets: ReadonlyArray<CsdlEntitySet> = rawEntitySets.map(es => {
    const rawBindings = (es.NavigationPropertyBinding as ReadonlyArray<Record<string, unknown>> | undefined) ?? [];
    return {
      name: es['@_Name'] as string,
      entityType: es['@_EntityType'] as string,
      ...(rawBindings.length > 0 && {
        navigationPropertyBindings: parseNavigationPropertyBindings(rawBindings)
      })
    };
  });

  // Singletons
  const rawSingletons = (rawContainer.Singleton as ReadonlyArray<Record<string, unknown>> | undefined) ?? [];
  const singletons: ReadonlyArray<CsdlSingleton> = rawSingletons.map(s => {
    const rawBindings = (s.NavigationPropertyBinding as ReadonlyArray<Record<string, unknown>> | undefined) ?? [];
    return {
      name: s['@_Name'] as string,
      type: s['@_Type'] as string,
      ...(rawBindings.length > 0 && {
        navigationPropertyBindings: parseNavigationPropertyBindings(rawBindings)
      })
    };
  });

  // Action imports
  const rawActionImports = (rawContainer.ActionImport as ReadonlyArray<Record<string, unknown>> | undefined) ?? [];
  const actionImports: ReadonlyArray<CsdlActionImport> = rawActionImports.map(ai => ({
    name: ai['@_Name'] as string,
    action: ai['@_Action'] as string,
    ...(ai['@_EntitySet'] !== undefined && {
      entitySet: ai['@_EntitySet'] as string
    })
  }));

  // Function imports
  const rawFunctionImports = (rawContainer.FunctionImport as ReadonlyArray<Record<string, unknown>> | undefined) ?? [];
  const functionImports: ReadonlyArray<CsdlFunctionImport> = rawFunctionImports.map(fi => ({
    name: fi['@_Name'] as string,
    function: fi['@_Function'] as string,
    ...(fi['@_EntitySet'] !== undefined && {
      entitySet: fi['@_EntitySet'] as string
    })
  }));

  return { name, entitySets, singletons, actionImports, functionImports };
};

/**
 * Parse an OData EDMX/CSDL XML string into a structured CsdlSchema.
 *
 * Expects the standard EDMX 4.0 format:
 * `edmx:Edmx > edmx:DataServices > Schema > EntityType | EnumType | ComplexType | Action | Function | EntityContainer`
 *
 * @throws {Error} if the Schema element cannot be found
 */
export const parseCsdlXml = (xml: string): CsdlSchema => {
  const parser = new XMLParser(xmlParserOptions);
  const parsed = parser.parse(xml);

  const schema = parsed?.['edmx:Edmx']?.['edmx:DataServices']?.Schema ?? parsed?.['edmx:Edmx']?.['edmx:DataServices']?.schema;

  if (!schema) {
    throw new Error('Could not find Schema element in metadata XML');
  }

  const namespace: string = schema['@_Namespace'] ?? '';

  const rawEntityTypes: ReadonlyArray<Record<string, unknown>> = schema.EntityType ?? [];
  const rawEnumTypes: ReadonlyArray<Record<string, unknown>> = schema.EnumType ?? [];
  const rawComplexTypes: ReadonlyArray<Record<string, unknown>> = schema.ComplexType ?? [];
  const rawActions: ReadonlyArray<Record<string, unknown>> = schema.Action ?? [];
  const rawFunctions: ReadonlyArray<Record<string, unknown>> = schema.Function ?? [];
  const rawContainer = schema.EntityContainer as Record<string, unknown> | undefined;

  return {
    namespace,
    entityTypes: parseEntityTypes(rawEntityTypes),
    enumTypes: parseEnumTypes(rawEnumTypes),
    complexTypes: parseComplexTypes(rawComplexTypes),
    actions: parseActions(rawActions),
    functions: parseFunctions(rawFunctions),
    entityContainer: parseEntityContainer(rawContainer)
  };
};

/** Find an entity type by name. */
export const getEntityType = (schema: CsdlSchema, name: string): CsdlEntityType | undefined =>
  schema.entityTypes.find(et => et.name === name);

/** Find an enum type by name. */
export const getEnumType = (schema: CsdlSchema, name: string): CsdlEnumType | undefined => schema.enumTypes.find(et => et.name === name);
