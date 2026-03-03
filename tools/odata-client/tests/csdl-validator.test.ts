import { describe, it, expect } from "vitest";
import { validateCsdl } from "../src/csdl/validator.js";
import type { CsdlSchema } from "../src/csdl/types.js";

const validSchema: CsdlSchema = {
  namespace: "org.reso.metadata",
  entityTypes: [
    {
      name: "Property",
      key: ["ListingKey"],
      properties: [
        { name: "ListingKey", type: "Edm.String" },
        { name: "ListPrice", type: "Edm.Decimal" },
        { name: "City", type: "Edm.String" },
      ],
      navigationProperties: [],
    },
  ],
  enumTypes: [
    {
      name: "StandardStatus",
      members: [
        { name: "Active", value: "0" },
        { name: "Pending", value: "1" },
      ],
    },
  ],
  complexTypes: [],
  actions: [],
  functions: [],
  entityContainer: {
    name: "Default",
    entitySets: [
      { name: "Property", entityType: "org.reso.metadata.Property" },
    ],
    singletons: [],
    actionImports: [],
    functionImports: [],
  },
};

describe("validateCsdl", () => {
  it("accepts a valid schema", () => {
    const result = validateCsdl(validSchema);
    expect(result.valid).toBe(true);
    expect(result.errors).toHaveLength(0);
  });

  it("detects missing namespace", () => {
    const schema: CsdlSchema = { ...validSchema, namespace: "" };
    const result = validateCsdl(schema);
    expect(result.valid).toBe(false);
    expect(result.errors[0].message).toContain("namespace");
  });

  it("detects missing key properties", () => {
    const schema: CsdlSchema = {
      ...validSchema,
      entityTypes: [{ name: "NoKey", key: [], properties: [], navigationProperties: [] }],
    };
    const result = validateCsdl(schema);
    expect(result.valid).toBe(false);
    expect(result.errors[0].message).toContain("no key");
  });

  it("detects key property not in properties list", () => {
    const schema: CsdlSchema = {
      ...validSchema,
      entityTypes: [
        {
          name: "BadKey",
          key: ["MissingProp"],
          properties: [{ name: "SomeField", type: "Edm.String" }],
          navigationProperties: [],
        },
      ],
    };
    const result = validateCsdl(schema);
    expect(result.valid).toBe(false);
    expect(result.errors[0].message).toContain("MissingProp");
  });

  it("detects invalid property types", () => {
    const schema: CsdlSchema = {
      ...validSchema,
      entityTypes: [
        {
          name: "BadType",
          key: ["Id"],
          properties: [
            { name: "Id", type: "Edm.String" },
            { name: "Broken", type: "InvalidType" },
          ],
          navigationProperties: [],
        },
      ],
    };
    const result = validateCsdl(schema);
    expect(result.valid).toBe(false);
    expect(result.errors[0].message).toContain("InvalidType");
  });

  it("allows namespace-qualified types (enum references)", () => {
    const schema: CsdlSchema = {
      ...validSchema,
      entityTypes: [
        {
          name: "WithEnum",
          key: ["Id"],
          properties: [
            { name: "Id", type: "Edm.String" },
            { name: "Status", type: "org.reso.metadata.StandardStatus" },
          ],
          navigationProperties: [],
        },
      ],
    };
    const result = validateCsdl(schema);
    expect(result.valid).toBe(true);
  });

  it("allows Collection types", () => {
    const schema: CsdlSchema = {
      ...validSchema,
      entityTypes: [
        {
          name: "WithCollection",
          key: ["Id"],
          properties: [
            { name: "Id", type: "Edm.String" },
            { name: "Tags", type: "Collection(Edm.String)" },
          ],
          navigationProperties: [],
        },
      ],
    };
    const result = validateCsdl(schema);
    expect(result.valid).toBe(true);
  });

  it("accepts Edm.Stream as a valid type", () => {
    const schema: CsdlSchema = {
      ...validSchema,
      entityTypes: [
        {
          name: "Document",
          key: ["Id"],
          properties: [
            { name: "Id", type: "Edm.String" },
            { name: "Content", type: "Edm.Stream" },
          ],
          navigationProperties: [],
        },
      ],
    };
    const result = validateCsdl(schema);
    expect(result.valid).toBe(true);
  });

  it("validates schema with complex types correctly", () => {
    const schema: CsdlSchema = {
      ...validSchema,
      complexTypes: [
        {
          name: "Address",
          properties: [
            { name: "Street", type: "Edm.String" },
            { name: "City", type: "Edm.String" },
          ],
          navigationProperties: [],
        },
      ],
      entityTypes: [
        {
          name: "Customer",
          key: ["Id"],
          properties: [
            { name: "Id", type: "Edm.String" },
            { name: "HomeAddress", type: "org.reso.metadata.Address" },
          ],
          navigationProperties: [],
        },
      ],
    };
    const result = validateCsdl(schema);
    expect(result.valid).toBe(true);
  });

  it("allows abstract entity types without keys", () => {
    const schema: CsdlSchema = {
      ...validSchema,
      entityTypes: [
        {
          name: "BaseEntity",
          key: [],
          properties: [{ name: "CreatedAt", type: "Edm.DateTimeOffset" }],
          navigationProperties: [],
          abstract: true,
        },
      ],
    };
    const result = validateCsdl(schema);
    expect(result.valid).toBe(true);
  });

  it("allows derived entity types without keys", () => {
    const schema: CsdlSchema = {
      ...validSchema,
      entityTypes: [
        {
          name: "BaseEntity",
          key: ["Id"],
          properties: [{ name: "Id", type: "Edm.Guid" }],
          navigationProperties: [],
        },
        {
          name: "DerivedEntity",
          key: [],
          properties: [{ name: "Extra", type: "Edm.String" }],
          navigationProperties: [],
          baseType: "org.reso.metadata.BaseEntity",
        },
      ],
    };
    const result = validateCsdl(schema);
    expect(result.valid).toBe(true);
  });

  it("validates navigation property targets reference known entity types", () => {
    const schema: CsdlSchema = {
      ...validSchema,
      entityTypes: [
        {
          name: "Order",
          key: ["Id"],
          properties: [{ name: "Id", type: "Edm.String" }],
          navigationProperties: [
            {
              name: "Customer",
              type: "UnknownType",
              isCollection: false,
              entityTypeName: "UnknownType",
            },
          ],
        },
      ],
    };
    const result = validateCsdl(schema);
    expect(result.valid).toBe(false);
    expect(result.errors.some((e) => e.message.includes("UnknownType"))).toBe(true);
  });
});
