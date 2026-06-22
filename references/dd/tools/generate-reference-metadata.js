#!/usr/bin/env node
/**
 * Generate reference metadata JSON from DD XLSX sheets.
 *
 * Produces the same JSON structure as the Commander's MetadataReport
 * serializer, extended with:
 *   - models[] — resource/complex type definitions (ready for DD 2.2)
 *   - isEnumeration — computed from SimpleDataType/LookupStatus
 *   - actions[] / functions[] — placeholders for future OData support
 *   - sourceResource on navigation properties
 *
 * Usage: node generate-reference-metadata.js <xlsx-path> <version> [output-path]
 */

import { writeFileSync } from 'node:fs';
import { resolve } from 'node:path';

const XLSX = (await import('xlsx')).default ?? (await import('xlsx'));

const [,, xlsxPath, version, outputPath] = process.argv;

if (!xlsxPath || !version) {
  console.log('Usage: node generate-reference-metadata.js <xlsx-path> <version> [output-path]');
  process.exit(0);
}

const wb = XLSX.readFile(resolve(xlsxPath));

// ── Type mapping ──

const SIMPLE_TYPE_TO_EDM = {
  'Boolean': 'Edm.Boolean',
  'Date': 'Edm.Date',
  'Decimal': 'Edm.Decimal',
  'Integer': 'Edm.Int64',
  'Number': 'Edm.Decimal',
  'String': 'Edm.String',
  'String List, Single': 'Edm.String',
  'String List, Multi': 'Edm.String',
  'Timestamp': 'Edm.DateTimeOffset',
  'Resource': 'Edm.NavigationProperty',
};

const ENUM_TYPES = new Set(['String List, Single', 'String List, Multi']);

// ── Fields ──

const fieldsSheet = wb.Sheets['Fields'];
if (!fieldsSheet) { console.error('No "Fields" sheet found.'); process.exit(1); }

const fieldsRaw = XLSX.utils.sheet_to_json(fieldsSheet);
const resourceSet = new Set();
const fields = [];

for (const row of fieldsRaw) {
  const resourceName = row['ResourceName'];
  const fieldName = row['StandardName'];
  if (!resourceName || !fieldName) continue;

  resourceSet.add(resourceName);

  const simpleType = String(row['SimpleDataType'] ?? '');
  const sourceResource = row['SourceResource'] ? String(row['SourceResource']) : undefined;
  const lookupName = row['LookupName'] ? String(row['LookupName']) : undefined;
  const sugMaxPrecision = row['SugMaxPrecision'];
  const isExpansion = simpleType === 'Resource' || simpleType === 'Collection' || !!sourceResource;
  const isCollection = simpleType === 'String List, Multi' || simpleType === 'Collection';
  const isEnumeration = ENUM_TYPES.has(simpleType) || (!!row['LookupStatus'] && String(row['LookupStatus']).trim() !== '');
  const targetResource = isExpansion ? (sourceResource ?? fieldName) : undefined;
  // Only EntityEventSequence needs Int64; other Number fields stay Decimal
  const resolvedSimpleType = simpleType === 'Number' && fieldName === 'EntityEventSequence' ? 'Integer' : simpleType;
  const edmType = isExpansion
    ? (isCollection ? `Collection(org.reso.metadata.${targetResource})` : `org.reso.metadata.${targetResource}`)
    : isEnumeration && lookupName
      ? `org.reso.metadata.enums.${lookupName}`
      : (SIMPLE_TYPE_TO_EDM[resolvedSimpleType] ?? resolvedSimpleType);

  // Annotations
  const annotations = [];
  const displayName = row['DisplayName'];
  if (displayName) annotations.push({ term: 'RESO.OData.Metadata.StandardName', value: String(displayName) });
  const wikiUrl = row['WikiPageUrl'];
  if (wikiUrl) annotations.push({ term: 'RESO.DDWikiUrl', value: String(wikiUrl) });
  const definition = row['Definition'];
  if (definition) annotations.push({ term: 'Core.Description', value: String(definition) });

  const field = {
    resourceName: String(resourceName),
    fieldName: String(fieldName),
    type: edmType,
    nullable: true,
    isEnumeration,
    annotations,
  };

  // Optional structural properties
  const maxLength = row['SugMaxLength'];
  if (maxLength && !isNaN(Number(maxLength))) field.maxLength = Number(maxLength);

  // For decimals: DD SugMaxLength = OData precision (total digits),
  // DD SugMaxPrecision = OData scale (decimal places)
  if (resolvedSimpleType === 'Number' && maxLength && !isNaN(Number(maxLength))) {
    field.precision = Number(maxLength);
    field.scale = sugMaxPrecision && !isNaN(Number(sugMaxPrecision)) ? Number(sugMaxPrecision) : 0;
  } else if (resolvedSimpleType === 'Decimal' && maxLength && !isNaN(Number(maxLength))) {
    field.precision = Number(maxLength);
    field.scale = sugMaxPrecision && !isNaN(Number(sugMaxPrecision)) ? Number(sugMaxPrecision) : 2;
  }

  if (isCollection) field.isCollection = true;
  if (isExpansion) {
    field.isExpansion = true;
    field.typeName = targetResource;
    if (sourceResource) field.sourceResource = sourceResource;
  }

  // DD metadata (useful for cert testing, not in Commander output)
  const lookupStatus = row['LookupStatus'];
  if (lookupStatus) field.lookupStatus = String(lookupStatus);
  const elementStatus = row['ElementStatus'];
  if (elementStatus) field.elementStatus = String(elementStatus);
  const payloads = row['Payloads'];
  if (payloads) field.payloads = String(payloads);
  const synonyms = row['Synonyms'];
  if (synonyms) field.synonyms = String(synonyms);

  fields.push(field);
}

// ── Lookups ──

const lookupsSheet = wb.Sheets['Lookups'];
if (!lookupsSheet) { console.error('No "Lookups" sheet found.'); process.exit(1); }

const lookupsRaw = XLSX.utils.sheet_to_json(lookupsSheet);
const lookups = [];

for (const row of lookupsRaw) {
  const lookupName = row['LookupName'];
  const standardLookupValue = row['StandardLookupValue'] ?? row['LookupDisplayName'];
  const lookupValue = row['LegacyODataValue'] ?? standardLookupValue;
  if (!lookupName || !lookupValue) continue;

  const annotations = [];
  annotations.push({ term: 'RESO.OData.Metadata.StandardName', value: String(standardLookupValue) });
  const wikiUrl = row['WikiPageUrl'];
  if (wikiUrl) annotations.push({ term: 'RESO.DDWikiUrl', value: String(wikiUrl) });
  const definition = row['Definition'];
  if (definition) annotations.push({ term: 'Core.Description', value: String(definition) });
  const legacyValue = row['LegacyODataValue'];
  if (legacyValue) annotations.push({ term: 'RESO.OData.Metadata.LegacyODataValue', value: String(legacyValue) });

  // Synonyms for lookups
  const synonyms = row['Synonyms'];
  if (synonyms) annotations.push({ term: 'RESO.OData.Metadata.Synonyms', value: String(synonyms) });

  lookups.push({
    lookupName: `org.reso.metadata.enums.${lookupName}`,
    lookupValue: String(lookupValue),
    type: 'Edm.Int32',
    annotations,
  });
}

// ── Models (resource/complex type definitions — ready for DD 2.2) ──

const models = [...resourceSet].sort().map(name => ({
  modelName: name,
  modelType: 'EntityType',
  // Complex type properties will go here in DD 2.2
  // properties: [...] for walking internal structure
}));

// ── Output ──

const output = {
  description: `RESO Data Dictionary ${version} Reference Metadata`,
  version,
  generatedOn: new Date().toISOString(),
  resources: [...resourceSet].sort(),
  models,
  fields,
  lookups,
  // Placeholders for future OData structural elements
  actions: [],
  functions: [],
};

const outFile = outputPath
  ? resolve(outputPath)
  : resolve(`reso-certification/etl/reference-metadata/dd-${version}.json`);

writeFileSync(outFile, JSON.stringify(output, null, 2));

console.log(`Generated: ${outFile}`);
console.log(`  Version: ${version}`);
console.log(`  Resources: ${resourceSet.size}`);
console.log(`  Models: ${models.length} (all EntityType for now)`);
console.log(`  Fields: ${fields.length} (${fields.filter(f => f.isEnumeration).length} enumerations, ${fields.filter(f => f.isExpansion).length} expansions)`);
console.log(`  Lookups: ${lookups.length}`);
console.log(`  Actions: 0 (placeholder)`);
console.log(`  Functions: 0 (placeholder)`);
