import { readFile } from "node:fs/promises";
import type { ResoMetadata, ResoField, ResoLookup } from "./types.js";
import { KEY_FIELD_MAP } from "./types.js";

/** Reads and parses a RESO metadata JSON file from disk. */
export const loadMetadata = async (filePath: string): Promise<ResoMetadata> => {
  const content = await readFile(filePath, "utf-8");
  return JSON.parse(content) as ResoMetadata;
};

/** Returns all fields belonging to a specific resource. */
export const getFieldsForResource = (
  metadata: ResoMetadata,
  resourceName: string,
): ReadonlyArray<ResoField> =>
  metadata.fields.filter((f) => f.resourceName === resourceName);

/** Returns all lookup values for a given enum type (fully qualified name). */
export const getLookupsForType = (
  metadata: ResoMetadata,
  lookupName: string,
): ReadonlyArray<ResoLookup> =>
  metadata.lookups.filter((l) => l.lookupName === lookupName);

/** Returns the primary key field name for a resource, or undefined if unknown. */
export const getKeyFieldForResource = (resourceName: string): string | undefined =>
  KEY_FIELD_MAP[resourceName];

/** Checks whether a field type is an enum reference (not a primitive Edm type). */
export const isEnumType = (type: string): boolean =>
  !type.startsWith("Edm.");

/** Extracts the lookup name from a field type string. For enums, returns the type itself. */
export const getLookupNameFromType = (type: string): string => type;

/** Returns the standard name annotation value for a field, if present. */
export const getStandardName = (field: ResoField): string | undefined =>
  field.annotations.find((a) => a.term === "RESO.OData.Metadata.StandardName")?.value;

/** Returns the description annotation value for a field, if present. */
export const getDescription = (field: ResoField): string | undefined =>
  field.annotations.find((a) => a.term === "Core.Description")?.value;
