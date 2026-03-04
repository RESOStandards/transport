import { getFieldsForResource, getKeyFieldForResource, isEnumType } from './loader.js';
import type { ResoField, ResoMetadata } from './types.js';

/** Escapes special XML characters in attribute values. */
const escapeXml = (str: string): string => str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');

/** Maps a RESO field type to an EDMX Property type string. */
const toEdmxType = (field: ResoField): string => {
  if (field.isCollection) {
    // Collection of enum → Collection(Edm.String)
    return 'Collection(Edm.String)';
  }
  if (isEnumType(field.type)) {
    return 'Edm.String';
  }
  return field.type;
};

/** Generates an EDMX Property element string for a field. */
const generateProperty = (field: ResoField): string => {
  const type = toEdmxType(field);
  const attrs: string[] = [`Name="${escapeXml(field.fieldName)}"`, `Type="${escapeXml(type)}"`];

  if (field.isCollection) {
    // Collections return [] not null — always Nullable="false"
    attrs.push('Nullable="false"');
  } else if (field.nullable === false) {
    // Nullable="true" is the OData default for non-collection properties — only emit when false
    attrs.push('Nullable="false"');
  }
  if (field.maxLength !== undefined) {
    attrs.push(`MaxLength="${field.maxLength}"`);
  }
  if (field.precision !== undefined) {
    attrs.push(`Precision="${field.precision}"`);
  }
  if (field.scale !== undefined) {
    attrs.push(`Scale="${field.scale}"`);
  }

  // Check for lookup annotation
  const lookupAnnotation = field.typeName
    ? `\n          <Annotation Term="RESO.OData.Metadata.LookupName" String="${escapeXml(field.typeName)}"/>`
    : '';

  if (lookupAnnotation) {
    return `        <Property ${attrs.join(' ')}>${lookupAnnotation}\n        </Property>`;
  }

  return `        <Property ${attrs.join(' ')}/>`;
};

/** Generates an EDMX NavigationProperty element for an expansion field. */
const generateNavigationProperty = (field: ResoField): string => {
  const type = field.isCollection ? `Collection(${field.type})` : field.type;
  return `        <NavigationProperty Name="${escapeXml(field.fieldName)}" Type="${escapeXml(type)}"/>`;
};

/** Generates an EDMX EntityType element for a resource. */
const generateEntityType = (
  resourceName: string,
  keyField: string,
  fields: ReadonlyArray<ResoField>,
  targetResourceSet: ReadonlySet<string>
): string => {
  const regularFields = fields.filter(f => !f.isExpansion);
  // Only emit NavigationProperty for types that exist in the schema
  const expansionFields = fields.filter(f => f.isExpansion && f.typeName && targetResourceSet.has(f.typeName));

  const properties = regularFields.map(generateProperty).join('\n');
  const navProperties = expansionFields.map(generateNavigationProperty).join('\n');

  return `      <EntityType Name="${escapeXml(resourceName)}">
        <Key>
          <PropertyRef Name="${escapeXml(keyField)}"/>
        </Key>
${properties}
${navProperties ? `${navProperties}\n` : ''}      </EntityType>`;
};

/** Generates an EntitySet element with NavigationPropertyBinding entries. */
const generateEntitySet = (resourceName: string, fields: ReadonlyArray<ResoField>, targetResourceSet: ReadonlySet<string>): string => {
  const namespace = 'org.reso.metadata';
  const expansionFields = fields.filter(f => f.isExpansion && f.typeName && targetResourceSet.has(f.typeName));

  if (expansionFields.length === 0) {
    return `        <EntitySet Name="${escapeXml(resourceName)}" EntityType="${namespace}.${escapeXml(resourceName)}"/>`;
  }

  const bindings = expansionFields
    .map(f => `          <NavigationPropertyBinding Path="${escapeXml(f.fieldName)}" Target="${escapeXml(f.typeName!)}"/>`)
    .join('\n');

  return `        <EntitySet Name="${escapeXml(resourceName)}" EntityType="${namespace}.${escapeXml(resourceName)}">\n${bindings}\n        </EntitySet>`;
};

/**
 * Generates a complete EDMX 4.0 XML metadata document from RESO JSON metadata.
 *
 * The output is compatible with fast-xml-parser using the same options as
 * certification/add-edit/src/lib/metadata.ts (attributeNamePrefix: "@_",
 * isArray for EntityType/Property/PropertyRef/Annotation).
 */
export const generateEdmx = (metadata: ResoMetadata, targetResources: ReadonlyArray<string>): string => {
  const targetResourceSet: ReadonlySet<string> = new Set(targetResources);
  const resourceData = targetResources
    .map(resource => {
      const fields = getFieldsForResource(metadata, resource);
      const keyField = getKeyFieldForResource(resource);
      if (!keyField || fields.length === 0) return null;
      return { resource, keyField, fields };
    })
    .filter((d): d is NonNullable<typeof d> => d !== null);

  const entityTypes = resourceData.map(d => generateEntityType(d.resource, d.keyField, d.fields, targetResourceSet)).join('\n');

  const entitySets = resourceData.map(d => generateEntitySet(d.resource, d.fields, targetResourceSet)).join('\n');

  return `<?xml version="1.0" encoding="UTF-8"?>
<edmx:Edmx Version="4.0" xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx">
  <edmx:DataServices>
    <Schema Namespace="org.reso.metadata" xmlns="http://docs.oasis-open.org/odata/ns/edm">
${entityTypes}
      <EntityContainer Name="Default">
${entitySets}
      </EntityContainer>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>`;
};
