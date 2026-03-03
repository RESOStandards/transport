/**
 * OData $filter expression AST types.
 *
 * Inspired by Apache Olingo's UriParser expression tree, adapted for
 * TypeScript with discriminated unions instead of class hierarchies.
 *
 * @see https://olingo.apache.org/doc/odata4/index.html
 */

// ---------------------------------------------------------------------------
// Comparison operators (OData 4.01 §5.1.1.1)
// ---------------------------------------------------------------------------

export type ComparisonOperator = 'eq' | 'ne' | 'gt' | 'ge' | 'lt' | 'le' | 'has' | 'in';

export interface ComparisonExpr {
  readonly type: 'comparison';
  readonly operator: ComparisonOperator;
  readonly left: FilterExpression;
  readonly right: FilterExpression;
}

// ---------------------------------------------------------------------------
// Logical operators (OData 4.01 §5.1.1.2)
// ---------------------------------------------------------------------------

export type LogicalOperator = 'and' | 'or';

export interface LogicalExpr {
  readonly type: 'logical';
  readonly operator: LogicalOperator;
  readonly left: FilterExpression;
  readonly right: FilterExpression;
}

export interface NotExpr {
  readonly type: 'not';
  readonly operand: FilterExpression;
}

// ---------------------------------------------------------------------------
// Arithmetic operators (OData 4.01 §5.1.1.3)
// ---------------------------------------------------------------------------

export type ArithmeticOperator = 'add' | 'sub' | 'mul' | 'div' | 'mod' | 'divby';

export interface ArithmeticExpr {
  readonly type: 'arithmetic';
  readonly operator: ArithmeticOperator;
  readonly left: FilterExpression;
  readonly right: FilterExpression;
}

// ---------------------------------------------------------------------------
// Function calls (OData 4.01 §5.1.1.4)
// ---------------------------------------------------------------------------

export type FilterFunctionName =
  // String functions
  | 'contains'
  | 'startswith'
  | 'endswith'
  | 'length'
  | 'indexof'
  | 'substring'
  | 'tolower'
  | 'toupper'
  | 'trim'
  | 'concat'
  | 'matchesPattern'
  // Date/Time functions
  | 'year'
  | 'month'
  | 'day'
  | 'hour'
  | 'minute'
  | 'second'
  | 'fractionalseconds'
  | 'totalseconds'
  | 'date'
  | 'time'
  | 'totaloffsetminutes'
  | 'now'
  | 'maxdatetime'
  | 'mindatetime'
  // Math functions
  | 'round'
  | 'floor'
  | 'ceiling'
  // Type functions
  | 'cast'
  | 'isof';

export interface FunctionCallExpr {
  readonly type: 'function';
  readonly name: FilterFunctionName;
  readonly args: ReadonlyArray<FilterExpression>;
}

// ---------------------------------------------------------------------------
// Lambda expressions (OData 4.01 §5.1.1.10)
// ---------------------------------------------------------------------------

export interface LambdaExpr {
  readonly type: 'lambda';
  readonly operator: 'any' | 'all';
  readonly variable: string;
  readonly source: FilterExpression;
  readonly predicate: FilterExpression;
}

// ---------------------------------------------------------------------------
// Literals
// ---------------------------------------------------------------------------

export type LiteralDataType =
  | 'string'
  | 'number'
  | 'boolean'
  | 'null'
  | 'date'
  | 'datetimeoffset'
  | 'timeofday'
  | 'duration'
  | 'guid'
  | 'enum';

export interface LiteralExpr {
  readonly type: 'literal';
  readonly value: string | number | boolean | null;
  readonly dataType: LiteralDataType;
}

// ---------------------------------------------------------------------------
// Collection literal (used for 'in' operator)
// ---------------------------------------------------------------------------

export interface CollectionExpr {
  readonly type: 'collection';
  readonly items: ReadonlyArray<FilterExpression>;
}

// ---------------------------------------------------------------------------
// Property access
// ---------------------------------------------------------------------------

export interface PropertyExpr {
  readonly type: 'property';
  readonly name: string;
}

// ---------------------------------------------------------------------------
// Union
// ---------------------------------------------------------------------------

export type FilterExpression =
  | ComparisonExpr
  | LogicalExpr
  | NotExpr
  | ArithmeticExpr
  | FunctionCallExpr
  | LiteralExpr
  | PropertyExpr
  | LambdaExpr
  | CollectionExpr;

// ---------------------------------------------------------------------------
// Token types (used by the lexer, exported for testing)
// ---------------------------------------------------------------------------

export type TokenType =
  | 'property'
  | 'literal_string'
  | 'literal_number'
  | 'literal_boolean'
  | 'literal_null'
  | 'literal_date'
  | 'literal_datetimeoffset'
  | 'literal_timeofday'
  | 'literal_duration'
  | 'literal_guid'
  | 'literal_enum'
  | 'comparison_op'
  | 'logical_op'
  | 'not_op'
  | 'arithmetic_op'
  | 'function'
  | 'lambda_op'
  | 'lparen'
  | 'rparen'
  | 'comma'
  | 'colon'
  | 'slash';

export interface Token {
  readonly type: TokenType;
  readonly value: string;
  readonly position: number;
}
