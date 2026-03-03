/**
 * @reso/odata-client
 *
 * OData 4.01 client SDK for TypeScript. Provides URI building, CRUD helpers,
 * CSDL metadata parsing/validation, query option validation, and response
 * parsing.
 *
 * Inspired by Apache Olingo's Java Client SDK.
 *
 * @see https://olingo.apache.org/doc/odata4/index.html
 *
 * @example
 * ```ts
 * import { createClient, createEntity, readEntity, buildUri } from "@reso/odata-client";
 *
 * const client = await createClient({
 *   baseUrl: "http://localhost:8080",
 *   auth: { mode: "token", authToken: "test" },
 * });
 *
 * const created = await createEntity(client, "Property", {
 *   ListPrice: 250000,
 *   City: "Austin",
 * }, { prefer: "representation" });
 * ```
 */

// Types
export type {
  ODataQueryOptions,
  ODataResponse,
  ODataEntity,
  ODataCollection,
  ODataAnnotations,
  ODataErrorBody,
  ODataErrorDetail,
  AuthConfig,
  TokenAuth,
  ClientCredentialsAuth,
  ClientConfig,
  PreferReturn,
  WriteOptions,
  ODataClient
} from './types.js';

// URI builder
export { buildUri } from './uri/builder.js';
export type { UriBuilder } from './uri/builder.js';
export { parseQueryString } from './uri/parser.js';

// CSDL parser & validator
export { parseCsdlXml, getEntityType, getEnumType } from './csdl/parser.js';
export { validateCsdl } from './csdl/validator.js';
export type {
  CsdlSchema,
  CsdlEntityType,
  CsdlProperty,
  CsdlNavigationProperty,
  CsdlReferentialConstraint,
  CsdlComplexType,
  CsdlEnumType,
  CsdlEnumMember,
  CsdlEntityContainer,
  CsdlEntitySet,
  CsdlNavigationPropertyBinding,
  CsdlSingleton,
  CsdlActionImport,
  CsdlFunctionImport,
  CsdlParameter,
  CsdlReturnType,
  CsdlAction,
  CsdlFunction,
  CsdlValidationError,
  CsdlValidationResult
} from './csdl/types.js';

// Query validator
export { validateQueryOptions } from './query/validator.js';
export type {
  QueryValidationError,
  QueryValidationResult
} from './query/validator.js';

// HTTP client & auth
export { createClient } from './http/client.js';
export { resolveToken, fetchAccessToken } from './http/auth.js';

// CRUD helpers
export { createEntity } from './crud/create.js';
export { readEntity } from './crud/read.js';
export { updateEntity } from './crud/update.js';
export { replaceEntity } from './crud/replace.js';
export { deleteEntity } from './crud/delete.js';
export type { DeleteOptions } from './crud/delete.js';
export { queryEntities } from './crud/query.js';

// Response parsing
export { extractAnnotations, isODataCollection, extractEntityData, getNextLink, followAllPages } from './response/parser.js';
export { isODataError, parseODataError, getErrorTargets } from './response/error.js';

// Metadata fetcher
export { fetchRawMetadata, fetchAndParseMetadata } from './metadata/fetcher.js';

// Re-export filter parser types for convenience
export type {
  FilterExpression,
  ComparisonExpr,
  LogicalExpr,
  NotExpr,
  ArithmeticExpr,
  FunctionCallExpr,
  LambdaExpr,
  CollectionExpr,
  LiteralExpr,
  PropertyExpr
} from '@reso/odata-filter-parser';
export { parseFilter } from '@reso/odata-filter-parser';
