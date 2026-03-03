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

  if (field.nullable !== undefined) {
    attrs.push(`Nullable="${field.nullable}"`);
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

/** Generates an EDMX EntityType element for a resource. */
const generateEntityType = (resourceName: string, keyField: string, fields: ReadonlyArray<ResoField>): string => {
  const properties = fields.map(generateProperty).join('\n');

  return `      <EntityType Name="${escapeXml(resourceName)}">
        <Key>
          <PropertyRef Name="${escapeXml(keyField)}"/>
        </Key>
${properties}
      </EntityType>`;
};

/**
 * Generates a complete EDMX 4.0 XML metadata document from RESO JSON metadata.
 *
 * The output is compatible with fast-xml-parser using the same options as
 * certification/add-edit/src/lib/metadata.ts (attributeNamePrefix: "@_",
 * isArray for EntityType/Property/PropertyRef/Annotation).
 */
export const generateEdmx = (metadata: ResoMetadata, targetResources: ReadonlyArray<string>): string => {
  const entityTypes = targetResources
    .map(resource => {
      const fields = getFieldsForResource(metadata, resource);
      const keyField = getKeyFieldForResource(resource);
      if (!keyField || fields.length === 0) return '';
      return generateEntityType(resource, keyField, fields);
    })
    .filter(Boolean)
    .join('\n');

  return `<?xml version="1.0" encoding="UTF-8"?>
<edmx:Edmx Version="4.0" xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx">
  <edmx:DataServices>
    <Schema Namespace="org.reso.metadata" xmlns="http://docs.oasis-open.org/odata/ns/edm">
${entityTypes}
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>`;
};
