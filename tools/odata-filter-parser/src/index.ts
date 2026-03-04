/**
 * @reso/odata-filter-parser
 *
 * Standalone OData $filter expression parser. Tokenizes and parses filter
 * strings into a typed AST (discriminated union). Zero runtime dependencies.
 *
 * Used by both the OData client (query validation) and the reference server
 * ($filter -> SQL WHERE translation).
 *
 * Inspired by Apache Olingo's UriParser / ExpressionParser.
 *
 * @example
 * ```ts
 * import { parseFilter } from "@reso/odata-filter-parser";
 *
 * const ast = parseFilter("ListPrice gt 200000 and City eq 'Austin'");
 * ```
 */

export { parseFilter, ParseError, LexerError } from './parser.js';
export { astToFilterString } from './serializer.js';
export { tokenize } from './lexer.js';
export type {
  FilterExpression,
  ComparisonExpr,
  LogicalExpr,
  NotExpr,
  ArithmeticExpr,
  FunctionCallExpr,
  LiteralExpr,
  PropertyExpr,
  LambdaExpr,
  CollectionExpr,
  ComparisonOperator,
  LogicalOperator,
  ArithmeticOperator,
  FilterFunctionName,
  LiteralDataType,
  Token,
  TokenType
} from './types.js';
