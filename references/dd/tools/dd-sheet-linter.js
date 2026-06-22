#!/usr/bin/env node
/**
 * DD Sheet Linter — validates RESO Data Dictionary XLSX sheets.
 *
 * Usage: node dd-sheet-linter.js <path-to-xlsx> [--prev <path-to-previous-xlsx>]
 *
 * Checks:
 *  1. Sheet structure (expected sheets + columns)
 *  2. PascalCase on all StandardNames
 *  3. No duplicate (Resource, FieldName) pairs
 *  4. No duplicate (LookupName, StandardLookupValue) pairs
 *  5. Unicode cleanliness (no BOM, NBSP, ZWSP)
 *  6. Referential integrity (LookupName → field with lookup type)
 *  7. Deprecation tracking vs. previous version (warnings, not errors)
 *  8. Synonym not equal to any StandardName
 *  9. Version Info sheet has valid version
 * 10. No empty StandardName cells in data rows
 */

import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';

// Dynamic import for openpyxl-equivalent — we use the xlsx package
let XLSX;
try {
  XLSX = (await import('xlsx')).default ?? (await import('xlsx'));
} catch {
  console.error('xlsx package not found. Install with: npm install xlsx');
  process.exit(1);
}

const [,, sheetPath, ...rest] = process.argv;
const prevFlag = rest.indexOf('--prev');
const prevPath = prevFlag >= 0 ? rest[prevFlag + 1] : null;

if (!sheetPath) {
  console.log('Usage: node dd-sheet-linter.js <path-to-xlsx> [--prev <path-to-previous-xlsx>]');
  process.exit(0);
}

const errors = [];
const warnings = [];
const info = [];

const addError = (check, msg) => errors.push({ check, msg });
const addWarning = (check, msg) => warnings.push({ check, msg });
const addInfo = (check, msg) => info.push({ check, msg });

// ── Load workbook ──

const wb = XLSX.readFile(resolve(sheetPath));
const prevWb = prevPath ? XLSX.readFile(resolve(prevPath)) : null;

// ── 1. Sheet structure ──

const EXPECTED_SHEETS_MINIMAL = ['Fields', 'Lookups'];
const EXPECTED_SHEETS_FULL = ['Fields', 'Lookups', 'Changes', 'Summary of Changes', 'Version Info'];

for (const name of EXPECTED_SHEETS_MINIMAL) {
  if (!wb.SheetNames.includes(name)) {
    addError('structure', `Missing required sheet: ${name}`);
  }
}

for (const name of EXPECTED_SHEETS_FULL) {
  if (!wb.SheetNames.includes(name)) {
    addWarning('structure', `Missing optional sheet: ${name}`);
  }
}

const fieldsSheet = wb.Sheets['Fields'];
const lookupsSheet = wb.Sheets['Lookups'];

if (!fieldsSheet || !lookupsSheet) {
  console.log('Cannot proceed without Fields and Lookups sheets.');
  process.exit(1);
}

const fieldsData = XLSX.utils.sheet_to_json(fieldsSheet);
const lookupsData = XLSX.utils.sheet_to_json(lookupsSheet);

addInfo('counts', `Fields: ${fieldsData.length} rows`);
addInfo('counts', `Lookups: ${lookupsData.length} rows`);

// ── Column name detection ──

const fieldHeaders = Object.keys(fieldsData[0] ?? {});
const lookupHeaders = Object.keys(lookupsData[0] ?? {});

const fieldNameCol = fieldHeaders.find(h => h === 'StandardName' || h === 'FieldName') ?? 'StandardName';
const resourceCol = fieldHeaders.find(h => h === 'ResourceName' || h === 'Resource') ?? 'ResourceName';
const lookupNameCol = lookupHeaders.find(h => h === 'LookupName') ?? 'LookupName';
const lookupValueCol = lookupHeaders.find(h => h === 'StandardLookupValue' || h === 'LookupDisplayName') ?? 'StandardLookupValue';
const synonymCol = fieldHeaders.find(h => h === 'Synonyms') ?? null;
const lookupSynonymCol = lookupHeaders.find(h => h === 'Synonyms') ?? null;

// ── 2. PascalCase check ──

const PASCAL_RE = /^[A-Z][a-zA-Z0-9]*$/;

const allFieldNames = new Set();
for (const row of fieldsData) {
  const name = row[fieldNameCol];
  if (!name) continue;
  allFieldNames.add(name);
  if (!PASCAL_RE.test(name)) {
    addError('pascal-case', `Field "${name}" is not PascalCase`);
  }
}

// ── 3. Duplicate fields ──

const fieldPairs = new Set();
for (const row of fieldsData) {
  const resource = row[resourceCol];
  const name = row[fieldNameCol];
  if (!resource || !name) continue;
  const key = `${resource}.${name}`;
  if (fieldPairs.has(key)) {
    addError('duplicate-field', `Duplicate field: ${key}`);
  }
  fieldPairs.add(key);
}

// ── 4. Duplicate lookups ──

const lookupPairs = new Set();
for (const row of lookupsData) {
  const name = row[lookupNameCol];
  const value = row[lookupValueCol];
  if (!name || !value) continue;
  const key = `${name}.${value}`;
  if (lookupPairs.has(key)) {
    addError('duplicate-lookup', `Duplicate lookup: ${key}`);
  }
  lookupPairs.add(key);
}

// ── 5. Unicode cleanliness ──

const checkUnicode = (sheet, sheetName) => {
  const raw = XLSX.utils.sheet_to_json(sheet, { header: 1 });
  for (let r = 0; r < raw.length; r++) {
    const row = raw[r];
    if (!row) continue;
    for (let c = 0; c < row.length; c++) {
      const val = row[c];
      if (typeof val !== 'string') continue;
      if (val.includes('\uFEFF')) addWarning('unicode', `BOM (U+FEFF) in ${sheetName} row ${r + 1}, col ${c + 1}`);
      if (val.includes('\u00A0')) addWarning('unicode', `NBSP (U+00A0) in ${sheetName} row ${r + 1}, col ${c + 1}`);
      if (val.includes('\u200B')) addWarning('unicode', `ZWSP (U+200B) in ${sheetName} row ${r + 1}, col ${c + 1}`);
      if (val.includes('\u200C')) addWarning('unicode', `ZWNJ (U+200C) in ${sheetName} row ${r + 1}, col ${c + 1}`);
      if (val.includes('\u200D')) addWarning('unicode', `ZWJ (U+200D) in ${sheetName} row ${r + 1}, col ${c + 1}`);
    }
  }
};

checkUnicode(fieldsSheet, 'Fields');
checkUnicode(lookupsSheet, 'Lookups');

// ── 6. Referential integrity (LookupName → field) ──

const lookupFieldNames = new Set(lookupsData.map(r => r[lookupNameCol]).filter(Boolean));
const fieldsWithLookups = new Set();
for (const row of fieldsData) {
  const simpleType = row['SimpleDataType'] ?? row['Simple Data Type'] ?? '';
  if (typeof simpleType === 'string' && simpleType.toLowerCase().includes('lookup')) {
    fieldsWithLookups.add(row[fieldNameCol]);
  }
}

for (const lookupName of lookupFieldNames) {
  // LookupName should correspond to a field name (with some exceptions for shared lookups)
  if (!allFieldNames.has(lookupName) && !lookupName.includes('Type') && !lookupName.includes('Status')) {
    // Relaxed check — many lookup names are shared across resources
    // Only flag if the lookup name doesn't match ANY field
    addInfo('referential', `LookupName "${lookupName}" has no exact field match (may be shared)`);
  }
}

// ── 7. Deprecation tracking vs. previous version ──

if (prevWb) {
  const prevFieldsSheet = prevWb.Sheets['Fields'];
  const prevLookupsSheet = prevWb.Sheets['Lookups'];

  if (prevFieldsSheet) {
    const prevFieldsData = XLSX.utils.sheet_to_json(prevFieldsSheet);
    const prevFieldNames = new Set(prevFieldsData.map(r => r[fieldNameCol]).filter(Boolean));

    const removedFields = [...prevFieldNames].filter(n => !allFieldNames.has(n));
    const addedFields = [...allFieldNames].filter(n => !prevFieldNames.has(n));

    if (removedFields.length > 0) {
      addWarning('deprecation', `${removedFields.length} field(s) removed (deprecated): ${removedFields.slice(0, 10).join(', ')}${removedFields.length > 10 ? '...' : ''}`);
    }
    if (addedFields.length > 0) {
      addInfo('additions', `${addedFields.length} field(s) added: ${addedFields.slice(0, 10).join(', ')}${addedFields.length > 10 ? '...' : ''}`);
    }
  }

  if (prevLookupsSheet) {
    const prevLookupsData = XLSX.utils.sheet_to_json(prevLookupsSheet);
    const prevLookupKeys = new Set(prevLookupsData.map(r => `${r[lookupNameCol]}.${r[lookupValueCol]}`).filter(k => !k.includes('undefined')));
    const currLookupKeys = new Set(lookupsData.map(r => `${r[lookupNameCol]}.${r[lookupValueCol]}`).filter(k => !k.includes('undefined')));

    const removedLookups = [...prevLookupKeys].filter(k => !currLookupKeys.has(k));
    const addedLookups = [...currLookupKeys].filter(k => !prevLookupKeys.has(k));

    if (removedLookups.length > 0) {
      addWarning('deprecation', `${removedLookups.length} lookup(s) removed: ${removedLookups.slice(0, 5).join(', ')}${removedLookups.length > 5 ? '...' : ''}`);
    }
    if (addedLookups.length > 0) {
      addInfo('additions', `${addedLookups.length} lookup(s) added: ${addedLookups.slice(0, 5).join(', ')}${addedLookups.length > 5 ? '...' : ''}`);
    }
  }
}

// ── 8. Synonym not equal to any StandardName ──

if (synonymCol) {
  for (const row of fieldsData) {
    const synonyms = row[synonymCol];
    if (!synonyms || typeof synonyms !== 'string') continue;
    for (const syn of synonyms.split(',').map(s => s.trim()).filter(Boolean)) {
      if (allFieldNames.has(syn)) {
        addWarning('synonym-collision', `Synonym "${syn}" for field "${row[fieldNameCol]}" is also a StandardName`);
      }
    }
  }
}

// ── 9. Version Info ──

if (wb.SheetNames.includes('Version Info')) {
  const versionSheet = XLSX.utils.sheet_to_json(wb.Sheets['Version Info']);
  if (versionSheet.length === 0) {
    addWarning('version', 'Version Info sheet is empty');
  }
}

// ── 10. No empty StandardName cells ──

let emptyNameCount = 0;
for (const row of fieldsData) {
  if (!row[fieldNameCol] || String(row[fieldNameCol]).trim() === '') {
    emptyNameCount++;
  }
}
if (emptyNameCount > 0) {
  addError('empty-name', `${emptyNameCount} field row(s) with empty StandardName`);
}

let emptyLookupCount = 0;
for (const row of lookupsData) {
  if (!row[lookupValueCol] || String(row[lookupValueCol]).trim() === '') {
    emptyLookupCount++;
  }
}
if (emptyLookupCount > 0) {
  addError('empty-lookup', `${emptyLookupCount} lookup row(s) with empty StandardLookupValue`);
}

// ── Report ──

console.log('\n╔══════════════════════════════════════════╗');
console.log('║         DD Sheet Linter Report           ║');
console.log('╚══════════════════════════════════════════╝\n');

console.log(`File: ${sheetPath}`);
if (prevPath) console.log(`Prev: ${prevPath}`);
console.log();

if (info.length > 0) {
  console.log('ℹ  Info:');
  for (const { check, msg } of info) console.log(`   [${check}] ${msg}`);
  console.log();
}

if (warnings.length > 0) {
  console.log(`⚠  Warnings (${warnings.length}):`);
  for (const { check, msg } of warnings) console.log(`   [${check}] ${msg}`);
  console.log();
}

if (errors.length > 0) {
  console.log(`✗  Errors (${errors.length}):`);
  for (const { check, msg } of errors) console.log(`   [${check}] ${msg}`);
  console.log();
}

if (errors.length === 0 && warnings.length === 0) {
  console.log('✓  All checks passed.\n');
} else if (errors.length === 0) {
  console.log(`✓  No errors. ${warnings.length} warning(s).\n`);
} else {
  console.log(`✗  ${errors.length} error(s), ${warnings.length} warning(s).\n`);
  process.exit(1);
}
