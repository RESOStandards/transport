/**
 * OData $filter recursive descent parser.
 *
 * Parses a token stream into a FilterExpression AST. Operator precedence
 * (lowest -> highest): or, and, not, comparison (eq/ne/gt/ge/lt/le/has/in),
 * arithmetic (add/sub, mul/div/mod/divby), primary.
 *
 * Inspired by Apache Olingo's ExpressionParser, adapted for a functional
 * TypeScript style with discriminated union AST nodes.
 *
 * @see https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_SystemQueryOptionfilter
 */

import { tokenize, LexerError } from "./lexer.js";
import type {
  FilterExpression,
  Token,
  ComparisonOperator,
  ArithmeticOperator,
  FilterFunctionName,
} from "./types.js";

export class ParseError extends Error {
  constructor(
    message: string,
    public readonly position: number,
  ) {
    super(`${message} at position ${position}`);
    this.name = "ParseError";
  }
}

/** Parser state — a simple index into the token array. */
interface ParserState {
  readonly tokens: ReadonlyArray<Token>;
  pos: number;
}

const peek = (state: ParserState): Token | undefined =>
  state.pos < state.tokens.length ? state.tokens[state.pos] : undefined;

const advance = (state: ParserState): Token => {
  const token = state.tokens[state.pos];
  if (!token) {
    const lastPos =
      state.tokens.length > 0
        ? state.tokens[state.tokens.length - 1].position
        : 0;
    throw new ParseError("Unexpected end of expression", lastPos);
  }
  state.pos++;
  return token;
};

const expect = (state: ParserState, type: string, value?: string): Token => {
  const token = advance(state);
  if (token.type !== type || (value !== undefined && token.value !== value)) {
    throw new ParseError(
      `Expected ${value ? `'${value}'` : type} but got '${token.value}'`,
      token.position,
    );
  }
  return token;
};

// Set of all recognized filter function names
const FILTER_FUNCTIONS = new Set<string>([
  "contains", "startswith", "endswith", "length", "indexof",
  "substring", "tolower", "toupper", "trim", "concat", "matchesPattern",
  "year", "month", "day", "hour", "minute", "second",
  "fractionalseconds", "totalseconds", "date", "time",
  "totaloffsetminutes", "now", "maxdatetime", "mindatetime",
  "round", "floor", "ceiling",
  "cast", "isof",
]);

// ---------------------------------------------------------------------------
// Grammar rules (lowest precedence first)
// ---------------------------------------------------------------------------

/**
 * orExpr -> andExpr ( 'or' andExpr )*
 */
const parseOrExpr = (state: ParserState): FilterExpression => {
  let left = parseAndExpr(state);
  while (peek(state)?.type === "logical_op" && peek(state)?.value === "or") {
    advance(state); // consume 'or'
    const right = parseAndExpr(state);
    left = { type: "logical", operator: "or", left, right };
  }
  return left;
};

/**
 * andExpr -> notExpr ( 'and' notExpr )*
 */
const parseAndExpr = (state: ParserState): FilterExpression => {
  let left = parseNotExpr(state);
  while (peek(state)?.type === "logical_op" && peek(state)?.value === "and") {
    advance(state); // consume 'and'
    const right = parseNotExpr(state);
    left = { type: "logical", operator: "and", left, right };
  }
  return left;
};

/**
 * notExpr -> 'not' notExpr | comparisonExpr
 */
const parseNotExpr = (state: ParserState): FilterExpression => {
  if (peek(state)?.type === "not_op") {
    advance(state); // consume 'not'
    const operand = parseNotExpr(state);
    return { type: "not", operand };
  }
  return parseComparisonExpr(state);
};

/**
 * comparisonExpr -> arithmeticExpr ( comparisonOp arithmeticExpr )?
 *
 * Special handling for 'in' operator: right side is a parenthesized list.
 */
const parseComparisonExpr = (state: ParserState): FilterExpression => {
  const left = parseArithmeticExpr(state);
  if (peek(state)?.type === "comparison_op") {
    const opToken = advance(state);
    const op = opToken.value as ComparisonOperator;

    if (op === "in") {
      // 'in' expects a parenthesized collection: (value, value, ...)
      expect(state, "lparen");
      const items: FilterExpression[] = [];
      if (peek(state)?.type !== "rparen") {
        items.push(parseOrExpr(state));
        while (peek(state)?.type === "comma") {
          advance(state); // consume ','
          items.push(parseOrExpr(state));
        }
      }
      expect(state, "rparen");
      return {
        type: "comparison",
        operator: "in",
        left,
        right: { type: "collection", items },
      };
    }

    const right = parseArithmeticExpr(state);
    return { type: "comparison", operator: op, left, right };
  }
  return left;
};

/**
 * arithmeticExpr -> primaryExpr ( arithmeticOp primaryExpr )*
 */
const parseArithmeticExpr = (state: ParserState): FilterExpression => {
  let left = parsePrimaryExpr(state);
  while (peek(state)?.type === "arithmetic_op") {
    const op = advance(state).value as ArithmeticOperator;
    const right = parsePrimaryExpr(state);
    left = { type: "arithmetic", operator: op, left, right };
  }
  return left;
};

/**
 * primaryExpr -> functionCall | grouping | literal | property (with optional lambda)
 */
const parsePrimaryExpr = (state: ParserState): FilterExpression => {
  const token = peek(state);
  if (!token) {
    throw new ParseError("Unexpected end of expression", 0);
  }

  switch (token.type) {
    case "function":
      return parseFunctionCall(state);

    case "lparen":
      return parseGrouping(state);

    case "literal_string": {
      advance(state);
      return { type: "literal", value: token.value, dataType: "string" };
    }

    case "literal_number": {
      advance(state);
      const numValue = token.value.includes(".")
        ? parseFloat(token.value)
        : parseInt(token.value, 10);
      return { type: "literal", value: numValue, dataType: "number" };
    }

    case "literal_boolean": {
      advance(state);
      return {
        type: "literal",
        value: token.value === "true",
        dataType: "boolean",
      };
    }

    case "literal_null": {
      advance(state);
      return { type: "literal", value: null, dataType: "null" };
    }

    case "literal_date": {
      advance(state);
      return { type: "literal", value: token.value, dataType: "date" };
    }

    case "literal_datetimeoffset": {
      advance(state);
      return { type: "literal", value: token.value, dataType: "datetimeoffset" };
    }

    case "literal_timeofday": {
      advance(state);
      return { type: "literal", value: token.value, dataType: "timeofday" };
    }

    case "literal_duration": {
      advance(state);
      return { type: "literal", value: token.value, dataType: "duration" };
    }

    case "literal_guid": {
      advance(state);
      return { type: "literal", value: token.value, dataType: "guid" };
    }

    case "literal_enum": {
      advance(state);
      return { type: "literal", value: token.value, dataType: "enum" };
    }

    case "property": {
      advance(state);
      let expr: FilterExpression = { type: "property", name: token.value };

      // Check for lambda: property/any(...) or property/all(...)
      // Also handles nested paths like property/subprop/any(...)
      while (peek(state)?.type === "slash") {
        advance(state); // consume '/'

        const next = peek(state);
        if (next?.type === "lambda_op") {
          // This is a lambda expression
          expr = parseLambda(state, expr);
        } else if (next?.type === "property") {
          // Navigation path segment inside a lambda predicate: o/Amount
          advance(state);
          expr = { type: "property", name: `${getPropertyName(expr)}/${next.value}` };
        } else {
          throw new ParseError(
            `Expected property name or lambda operator after '/'`,
            next?.position ?? 0,
          );
        }
      }

      return expr;
    }

    default:
      throw new ParseError(
        `Unexpected token '${token.value}'`,
        token.position,
      );
  }
};

/**
 * Extract the property name from an expression (for building navigation paths).
 */
const getPropertyName = (expr: FilterExpression): string => {
  if (expr.type === "property") return expr.name;
  throw new ParseError("Expected property expression for navigation path", 0);
};

/**
 * parseLambda -> lambdaOp '(' [ variable ':' predicate ] ')'
 *
 * The source expression (the collection property) is passed in.
 */
const parseLambda = (state: ParserState, source: FilterExpression): FilterExpression => {
  const opToken = advance(state); // consume 'any' or 'all'
  const operator = opToken.value as "any" | "all";

  expect(state, "lparen");

  // Check for empty lambda: any() — means "collection is non-empty"
  if (peek(state)?.type === "rparen") {
    advance(state); // consume ')'
    return {
      type: "lambda",
      operator,
      variable: "",
      source,
      predicate: { type: "literal", value: true, dataType: "boolean" },
    };
  }

  // Read variable name
  const varToken = advance(state);
  if (varToken.type !== "property") {
    throw new ParseError(
      `Expected lambda variable name but got '${varToken.value}'`,
      varToken.position,
    );
  }
  const variable = varToken.value;

  // Expect colon
  expect(state, "colon");

  // Parse predicate expression
  const predicate = parseOrExpr(state);

  expect(state, "rparen");

  return { type: "lambda", operator, variable, source, predicate };
};

/**
 * functionCall -> functionName '(' argList ')'
 * argList -> expression ( ',' expression )*
 *
 * For cast/isof, type names (like Edm.Int32) are parsed as property expressions.
 */
const parseFunctionCall = (state: ParserState): FilterExpression => {
  const nameToken = advance(state);
  const rawName = nameToken.value;
  const name = (FILTER_FUNCTIONS.has(rawName) ? rawName : rawName) as FilterFunctionName;
  expect(state, "lparen");

  const args: FilterExpression[] = [];
  if (peek(state)?.type !== "rparen") {
    args.push(parseOrExpr(state));
    while (peek(state)?.type === "comma") {
      advance(state); // consume ','
      args.push(parseOrExpr(state));
    }
  }

  expect(state, "rparen");
  return { type: "function", name, args };
};

/**
 * grouping -> '(' expression ')'
 */
const parseGrouping = (state: ParserState): FilterExpression => {
  advance(state); // consume '('
  const expr = parseOrExpr(state);
  expect(state, "rparen");
  return expr;
};

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/**
 * Parse an OData $filter expression string into a FilterExpression AST.
 *
 * @throws {LexerError} if the input contains invalid characters
 * @throws {ParseError} if the token stream is syntactically invalid
 *
 * @example
 * ```ts
 * const ast = parseFilter("ListPrice gt 200000 and City eq 'Austin'");
 * // -> { type: "logical", operator: "and", left: {...}, right: {...} }
 * ```
 */
export const parseFilter = (filterString: string): FilterExpression => {
  const tokens = tokenize(filterString);
  if (tokens.length === 0) {
    throw new ParseError("Empty filter expression", 0);
  }

  const state: ParserState = { tokens, pos: 0 };
  const ast = parseOrExpr(state);

  // Ensure all tokens were consumed
  if (state.pos < state.tokens.length) {
    const remaining = state.tokens[state.pos];
    throw new ParseError(
      `Unexpected token '${remaining.value}' after end of expression`,
      remaining.position,
    );
  }

  return ast;
};

export { LexerError };
