import { XMLParser } from "fast-xml-parser";
import { readFile } from "node:fs/promises";
import type { ParsedMetadata, EntityType, EntityProperty } from "./types.js";

/**
 * Parser options for fast-xml-parser.
 * `isArray` forces certain elements to always be arrays even when only one child exists,
 * which prevents inconsistent shapes when the metadata has a single EntityType or Property.
 */
const xmlParserOptions = {
  ignoreAttributes: false,
  attributeNamePrefix: "@_",
  isArray: (name: string) =>
    ["EntityType", "Property", "PropertyRef", "Annotation"].includes(name),
};

/**
 * Fetches OData XML metadata from a server's `/$metadata` endpoint.
 * Requires a bearer token for authorization.
 * Returns the raw XML string.
 */
export async function fetchMetadata(
  serverUrl: string,
  authToken: string,
): Promise<string> {
  const metadataUrl = `${serverUrl.replace(/\/$/, "")}/$metadata`;
  const response = await fetch(metadataUrl, {
    headers: {
      Authorization: `Bearer ${authToken}`,
      Accept: "application/xml",
    },
  });

  if (!response.ok) {
    throw new Error(
      `Failed to fetch metadata from ${metadataUrl}: ${response.status} ${response.statusText}`,
    );
  }

  return response.text();
}

/** Reads OData XML metadata from a local file. */
export async function loadMetadataFromFile(filePath: string): Promise<string> {
  return readFile(filePath, "utf-8");
}

/**
 * Parses an OData EDMX XML metadata document into a structured representation.
 *
 * Extracts the schema namespace, entity types, their key properties, property definitions
 * (including type, nullable, maxLength, precision, scale), and any annotations.
 *
 * Expects the standard EDMX 4.0 format:
 * `edmx:Edmx > edmx:DataServices > Schema > EntityType > Property`
 */
export function parseMetadataXml(xml: string): ParsedMetadata {
  const parser = new XMLParser(xmlParserOptions);
  const parsed = parser.parse(xml);

  const schema =
    parsed?.["edmx:Edmx"]?.["edmx:DataServices"]?.["Schema"] ??
    parsed?.["edmx:Edmx"]?.["edmx:DataServices"]?.["schema"];

  if (!schema) {
    throw new Error("Could not find Schema element in metadata XML");
  }

  const namespace: string = schema["@_Namespace"] ?? "";

  const rawEntityTypes: ReadonlyArray<Record<string, unknown>> =
    schema["EntityType"] ?? [];

  const entityTypes: ReadonlyArray<EntityType> = rawEntityTypes.map(
    (rawEntity) => {
      const name = rawEntity["@_Name"] as string;

      const keyRefs = (
        (rawEntity["Key"] as Record<string, unknown>)?.["PropertyRef"] as
          | ReadonlyArray<Record<string, string>>
          | undefined
      ) ?? [];
      const keyProperties = keyRefs.map((ref) => ref["@_Name"]);

      const rawProperties =
        (rawEntity["Property"] as ReadonlyArray<Record<string, unknown>>) ?? [];

      const properties: ReadonlyArray<EntityProperty> = rawProperties.map(
        (rawProp) => {
          const annotations: Record<string, string> = {};
          const rawAnnotations =
            (rawProp["Annotation"] as
              | ReadonlyArray<Record<string, string>>
              | undefined) ?? [];
          for (const ann of rawAnnotations) {
            const term = ann["@_Term"];
            const value = ann["@_String"] ?? ann["@_Bool"] ?? "";
            if (term) {
              annotations[term] = value;
            }
          }

          const prop: EntityProperty = {
            name: rawProp["@_Name"] as string,
            type: rawProp["@_Type"] as string,
            ...(rawProp["@_Nullable"] !== undefined && {
              nullable: rawProp["@_Nullable"] === "true",
            }),
            ...(rawProp["@_MaxLength"] !== undefined && {
              maxLength: Number(rawProp["@_MaxLength"]),
            }),
            ...(rawProp["@_Precision"] !== undefined && {
              precision: Number(rawProp["@_Precision"]),
            }),
            ...(rawProp["@_Scale"] !== undefined && {
              scale: Number(rawProp["@_Scale"]),
            }),
            ...(Object.keys(annotations).length > 0 && { annotations }),
          };

          return prop;
        },
      );

      return { name, keyProperties, properties };
    },
  );

  return { namespace, entityTypes };
}

/** Finds an entity type by name in parsed metadata. Returns undefined if not found. */
export function getEntityType(
  metadata: ParsedMetadata,
  resourceName: string,
): EntityType | undefined {
  return metadata.entityTypes.find((et) => et.name === resourceName);
}

/**
 * Validates that all fields in a payload exist in the entity type's property definitions.
 * Keys prefixed with `@` (OData annotations) are ignored.
 * Returns the list of unknown fields, if any.
 */
export function validatePayloadAgainstMetadata(
  payload: Record<string, unknown>,
  entityType: EntityType,
): { readonly valid: boolean; readonly unknownFields: ReadonlyArray<string> } {
  const propertyNames = new Set(entityType.properties.map((p) => p.name));
  const unknownFields = Object.keys(payload)
    .filter((key) => !key.startsWith("@"))
    .filter((key) => !propertyNames.has(key));

  return {
    valid: unknownFields.length === 0,
    unknownFields,
  };
}
