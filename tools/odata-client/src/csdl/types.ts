/**
 * CSDL (Common Schema Definition Language) types for OData metadata.
 *
 * These types represent the structure of an OData EDMX/CSDL XML metadata
 * document after parsing. They are OData-standard types, not RESO-specific.
 */

/** A property (field) in an entity type or complex type. */
export interface CsdlProperty {
  readonly name: string;
  readonly type: string;
  readonly nullable?: boolean;
  readonly maxLength?: number;
  readonly precision?: number;
  readonly scale?: number;
  readonly annotations?: Readonly<Record<string, string>>;
}

/** A referential constraint on a navigation property (foreign key relationship). */
export interface CsdlReferentialConstraint {
  readonly property: string;
  readonly referencedProperty: string;
}

/**
 * A navigation property linking one entity type to another.
 * Represents a relationship that can be expanded via $expand.
 *
 * @see https://docs.oasis-open.org/odata/odata-csdl-xml/v4.01/odata-csdl-xml-v4.01.html#sec_NavigationProperty
 */
export interface CsdlNavigationProperty {
  readonly name: string;
  /** Fully-qualified entity type or Collection(EntityType). */
  readonly type: string;
  /** Whether this is a collection navigation (derived from Collection() wrapper). */
  readonly isCollection: boolean;
  /** The entity type name (without namespace prefix or Collection wrapper). */
  readonly entityTypeName: string;
  readonly nullable?: boolean;
  /** Partner navigation property name on the target type. */
  readonly partner?: string;
  /** Whether the navigation property contains its target (containment navigation). */
  readonly containsTarget?: boolean;
  /** Referential constraints defining foreign key relationships. */
  readonly referentialConstraints?: ReadonlyArray<CsdlReferentialConstraint>;
}

/** An entity type definition. */
export interface CsdlEntityType {
  readonly name: string;
  readonly key: ReadonlyArray<string>;
  readonly properties: ReadonlyArray<CsdlProperty>;
  readonly navigationProperties: ReadonlyArray<CsdlNavigationProperty>;
  /** Fully-qualified base type for inheritance. */
  readonly baseType?: string;
  /** Whether this entity type is abstract. */
  readonly abstract?: boolean;
  /** Whether this entity type is open (allows dynamic properties). */
  readonly openType?: boolean;
  /** Whether this entity type has an associated media stream. */
  readonly hasStream?: boolean;
}

/** A complex type definition (structured type without a key). */
export interface CsdlComplexType {
  readonly name: string;
  readonly baseType?: string;
  readonly abstract?: boolean;
  readonly openType?: boolean;
  readonly properties: ReadonlyArray<CsdlProperty>;
  readonly navigationProperties: ReadonlyArray<CsdlNavigationProperty>;
}

/** A member of an enumeration type. */
export interface CsdlEnumMember {
  readonly name: string;
  readonly value?: string;
}

/** An enumeration type definition. */
export interface CsdlEnumType {
  readonly name: string;
  readonly members: ReadonlyArray<CsdlEnumMember>;
  /** The underlying integer type (e.g. Edm.Int32). */
  readonly underlyingType?: string;
  /** Whether this enum supports bitwise combination of members. */
  readonly isFlags?: boolean;
}

/** A parameter for an Action or Function. */
export interface CsdlParameter {
  readonly name: string;
  readonly type: string;
  readonly nullable?: boolean;
}

/** A return type for an Action or Function. */
export interface CsdlReturnType {
  readonly type: string;
  readonly nullable?: boolean;
}

/** An OData Action definition. */
export interface CsdlAction {
  readonly name: string;
  readonly isBound?: boolean;
  readonly entitySetPath?: string;
  readonly parameters: ReadonlyArray<CsdlParameter>;
  readonly returnType?: CsdlReturnType;
}

/** An OData Function definition. */
export interface CsdlFunction {
  readonly name: string;
  readonly isBound?: boolean;
  readonly isComposable?: boolean;
  readonly entitySetPath?: string;
  readonly parameters: ReadonlyArray<CsdlParameter>;
  readonly returnType: CsdlReturnType;
}

/** A navigation property binding in an entity set or singleton. */
export interface CsdlNavigationPropertyBinding {
  readonly path: string;
  readonly target: string;
}

/** An entity set in the entity container. */
export interface CsdlEntitySet {
  readonly name: string;
  readonly entityType: string;
  readonly navigationPropertyBindings?: ReadonlyArray<CsdlNavigationPropertyBinding>;
}

/** A singleton in the entity container. */
export interface CsdlSingleton {
  readonly name: string;
  readonly type: string;
  readonly navigationPropertyBindings?: ReadonlyArray<CsdlNavigationPropertyBinding>;
}

/** An action import in the entity container. */
export interface CsdlActionImport {
  readonly name: string;
  readonly action: string;
  readonly entitySet?: string;
}

/** A function import in the entity container. */
export interface CsdlFunctionImport {
  readonly name: string;
  readonly function: string;
  readonly entitySet?: string;
}

/** The entity container. */
export interface CsdlEntityContainer {
  readonly name: string;
  readonly entitySets: ReadonlyArray<CsdlEntitySet>;
  readonly singletons: ReadonlyArray<CsdlSingleton>;
  readonly actionImports: ReadonlyArray<CsdlActionImport>;
  readonly functionImports: ReadonlyArray<CsdlFunctionImport>;
}

/** A parsed CSDL schema. */
export interface CsdlSchema {
  readonly namespace: string;
  readonly entityTypes: ReadonlyArray<CsdlEntityType>;
  readonly enumTypes: ReadonlyArray<CsdlEnumType>;
  readonly complexTypes: ReadonlyArray<CsdlComplexType>;
  readonly actions: ReadonlyArray<CsdlAction>;
  readonly functions: ReadonlyArray<CsdlFunction>;
  readonly entityContainer?: CsdlEntityContainer;
}

/** Validation error for a CSDL document. */
export interface CsdlValidationError {
  readonly path: string;
  readonly message: string;
}

/** Result of CSDL validation. */
export interface CsdlValidationResult {
  readonly valid: boolean;
  readonly errors: ReadonlyArray<CsdlValidationError>;
}
