/**
 * Data Access Layer (DAL) interface — abstracts the underlying persistence
 * layer (PostgreSQL, MongoDB, etc.) from the OData request handling code.
 *
 * Handlers pass structured query options and receive hydrated entities with
 * expanded navigation properties already resolved. The DAL implementation
 * decides *how* to fetch and group the data:
 *
 *  - RDBMS (Postgres): LEFT JOIN + app-side grouping for $expand
 *  - Document store (MongoDB): batch lookups per navigation property
 *
 * @see https://www.odata.org/documentation/ (OData 4.01)
 */

import type { ResoField } from '../metadata/types.js';

// ---------------------------------------------------------------------------
// Query options — the handler-level representation of OData system query opts
// ---------------------------------------------------------------------------

/** Parsed OData system query options passed to the DAL. */
export interface CollectionQueryOptions {
  /** Parsed $filter string (raw OData expression, DAL implementation parses to AST). */
  readonly $filter?: string;
  /** Comma-separated list of fields to return. */
  readonly $select?: string;
  /** Comma-separated ordering expressions, e.g. "ListPrice desc,City asc". */
  readonly $orderby?: string;
  /** Maximum number of records to return. */
  readonly $top?: number;
  /** Number of records to skip (offset). */
  readonly $skip?: number;
  /** Request inline count of total matching records. */
  readonly $count?: boolean;
  /** Comma-separated navigation properties to expand (single-level). */
  readonly $expand?: string;
}

// ---------------------------------------------------------------------------
// Result types
// ---------------------------------------------------------------------------

/** A single entity record with optional expanded navigation properties. */
export type EntityRecord = Readonly<Record<string, unknown>>;

/** Result of a collection query. */
export interface CollectionResult {
  /** The matching entity records (with expanded navprops nested inline). */
  readonly value: ReadonlyArray<EntityRecord>;
  /** Total count of matching records (only present when $count=true). */
  readonly count?: number;
}

/** Result of a single-entity read. `undefined` means not found. */
export type SingleResult = EntityRecord | undefined;

// ---------------------------------------------------------------------------
// Navigation property metadata (used by DAL for $expand resolution)
// ---------------------------------------------------------------------------

/** Describes a navigation property relationship between two resources. */
export interface NavigationPropertyBinding {
  /** The navigation property name as it appears in $expand (e.g. "Media"). */
  readonly name: string;
  /** The target resource/table name (e.g. "Media"). */
  readonly targetResource: string;
  /** The target resource's key field (e.g. "MediaKey"). */
  readonly targetKeyField: string;
  /** The target resource's fields (for deserialization). */
  readonly targetFields: ReadonlyArray<ResoField>;
  /**
   * FK relationship: the field on the *target* resource that references the
   * parent. Follows RESO convention of ResourceName + ResourceRecordKey.
   * e.g. For Property→Media, the target (Media) has "ResourceName" and
   * "ResourceRecordKey" columns pointing back to the parent.
   */
  readonly foreignKey: NavigationForeignKey;
  /** Whether this is a to-many (collection) or to-one relationship. */
  readonly isCollection: boolean;
}

/** Foreign key descriptor for a navigation property relationship. */
export interface NavigationForeignKey {
  /**
   * Strategy for resolving the FK relationship.
   *
   * - "resource-record-key": RESO convention — target has ResourceName +
   *   ResourceRecordKey columns. Filter target WHERE ResourceName = parent
   *   resource AND ResourceRecordKey = parent key value.
   *
   * - "direct": Direct FK column on the target pointing to the parent key.
   *   e.g. target.ParentKey = parent.Key
   */
  readonly strategy: 'resource-record-key' | 'direct';
  /**
   * For "direct" strategy: the column on the target resource that holds the
   * parent's key value.
   */
  readonly targetColumn?: string;
}

// ---------------------------------------------------------------------------
// Data Access Layer interface
// ---------------------------------------------------------------------------

/** Resource context passed to DAL methods. */
export interface ResourceContext {
  /** Resource/table name (e.g. "Property"). */
  readonly resource: string;
  /** Primary key field name (e.g. "ListingKey"). */
  readonly keyField: string;
  /** All field definitions for the resource. */
  readonly fields: ReadonlyArray<ResoField>;
  /** Navigation property bindings for $expand resolution. */
  readonly navigationBindings: ReadonlyArray<NavigationPropertyBinding>;
}

/**
 * Data Access Layer interface.
 *
 * Implementations translate these high-level operations into database-specific
 * queries. Each method receives a `ResourceContext` describing the target
 * resource's schema, so the DAL can validate fields and build queries.
 */
export interface DataAccessLayer {
  /**
   * Query a collection of entities with optional filtering, sorting,
   * pagination, field selection, and navigation property expansion.
   */
  readonly queryCollection: (ctx: ResourceContext, options?: CollectionQueryOptions) => Promise<CollectionResult>;

  /**
   * Read a single entity by its primary key, optionally with $select
   * and $expand.
   */
  readonly readByKey: (
    ctx: ResourceContext,
    keyValue: string,
    options?: {
      readonly $select?: string;
      readonly $expand?: string;
    }
  ) => Promise<SingleResult>;

  /**
   * Insert a new entity record. Returns the full inserted row.
   */
  readonly insert: (ctx: ResourceContext, record: Readonly<Record<string, unknown>>) => Promise<EntityRecord>;

  /**
   * Update an existing entity (PATCH merge semantics). Returns the updated
   * row, or `undefined` if the record was not found.
   */
  readonly update: (ctx: ResourceContext, keyValue: string, updates: Readonly<Record<string, unknown>>) => Promise<SingleResult>;

  /**
   * Delete an entity by key. Returns `true` if deleted, `false` if not found.
   */
  readonly deleteByKey: (ctx: ResourceContext, keyValue: string) => Promise<boolean>;
}
