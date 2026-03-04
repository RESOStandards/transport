/**
 * OData $filter expression lexer (tokenizer).
 *
 * Converts a raw $filter string into a flat array of tokens for the
 * recursive-descent parser. Handles OData literal formats: single-quoted
 * strings (with '' escape), integers, decimals, booleans, null, dates,
 * datetimeoffsets, timeofday, durations, guids, and enum values.
 */

import type { Token } from './types.js';

const COMPARISON_OPS = new Set(['eq', 'ne', 'gt', 'ge', 'lt', 'le', 'has', 'in']);
const LOGICAL_OPS = new Set(['and', 'or']);
const ARITHMETIC_OPS = new Set(['add', 'sub', 'mul', 'div', 'mod', 'divby']);
const FUNCTIONS = new Set([
  // String
  'contains',
  'startswith',
  'endswith',
  'length',
  'indexof',
  'substring',
  'tolower',
  'toupper',
  'trim',
  'concat',
  'matchesPattern',
  // Date/Time
  'year',
  'month',
  'day',
  'hour',
  'minute',
  'second',
  'fractionalseconds',
  'totalseconds',
  'date',
  'time',
  'totaloffsetminutes',
  'now',
  'maxdatetime',
  'mindatetime',
  // Math
  'round',
  'floor',
  'ceiling',
  // Type
  'cast',
  'isof'
]);
const LAMBDA_OPS = new Set(['any', 'all']);
const BOOLEANS = new Set(['true', 'false']);

// Regex patterns for date/time/guid literals
const DATE_PATTERN = /^\d{4}-\d{2}-\d{2}/;
const DATETIMEOFFSET_PATTERN = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?(Z|[+-]\d{2}:\d{2})/;
const GUID_PATTERN = /^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}/;
const DURATION_PATTERN = /^duration'[^']*'/;

export class LexerError extends Error {
  constructor(
    message: string,
    public readonly position: number
  ) {
    super(`${message} at position ${position}`);
    this.name = 'LexerError';
  }
}

/**
 * Tokenize an OData $filter expression string.
 */
export const tokenize = (input: string): ReadonlyArray<Token> => {
  const tokens: Token[] = [];
  let pos = 0;

  const skipWhitespace = (): void => {
    while (pos < input.length && /\s/.test(input[pos])) {
      pos++;
    }
  };

  const readString = (): Token => {
    const start = pos;
    pos++; // skip opening quote
    let value = '';
    while (pos < input.length) {
      if (input[pos] === "'") {
        if (pos + 1 < input.length && input[pos + 1] === "'") {
          // escaped single quote
          value += "'";
          pos += 2;
        } else {
          // end of string
          pos++; // skip closing quote
          return { type: 'literal_string', value, position: start };
        }
      } else {
        value += input[pos];
        pos++;
      }
    }
    throw new LexerError('Unterminated string literal', start);
  };

  const readNumber = (): Token => {
    const start = pos;
    let numStr = '';
    if (input[pos] === '-') {
      numStr += '-';
      pos++;
    }
    while (pos < input.length && /[0-9]/.test(input[pos])) {
      numStr += input[pos];
      pos++;
    }
    if (pos < input.length && input[pos] === '.') {
      numStr += '.';
      pos++;
      while (pos < input.length && /[0-9]/.test(input[pos])) {
        numStr += input[pos];
        pos++;
      }
    }
    return { type: 'literal_number', value: numStr, position: start };
  };

  /**
   * Check if the remaining input at current pos matches a date/datetime/guid literal.
   * Returns a token if matched, undefined otherwise.
   */
  const tryReadDateTimeGuid = (): Token | undefined => {
    const remaining = input.slice(pos);

    // Try DateTimeOffset first (longer match)
    const dtMatch = remaining.match(DATETIMEOFFSET_PATTERN);
    if (dtMatch) {
      const start = pos;
      const value = dtMatch[0];
      pos += value.length;
      return { type: 'literal_datetimeoffset', value, position: start };
    }

    // Try Date
    const dateMatch = remaining.match(DATE_PATTERN);
    if (dateMatch) {
      // Make sure it's not followed by more alphanumeric chars (would be a property name)
      const value = dateMatch[0];
      const afterMatch = pos + value.length;
      if (afterMatch >= input.length || !/[a-zA-Z0-9_]/.test(input[afterMatch])) {
        const start = pos;
        pos += value.length;
        return { type: 'literal_date', value, position: start };
      }
    }

    return undefined;
  };

  const tryReadGuid = (): Token | undefined => {
    const remaining = input.slice(pos);
    const guidMatch = remaining.match(GUID_PATTERN);
    if (guidMatch) {
      const value = guidMatch[0];
      const afterMatch = pos + value.length;
      // Make sure it's not followed by more alphanumeric chars
      if (afterMatch >= input.length || !/[a-zA-Z0-9_]/.test(input[afterMatch])) {
        const start = pos;
        pos += value.length;
        return { type: 'literal_guid', value, position: start };
      }
    }
    return undefined;
  };

  const tryReadTimeOfDay = (): Token | undefined => {
    const remaining = input.slice(pos);
    const todMatch = remaining.match(/^\d{2}:\d{2}:\d{2}/);
    if (todMatch) {
      const value = todMatch[0];
      const afterMatch = pos + value.length;
      // Make sure it's not followed by more alphanumeric chars (but allow fractional seconds)
      if (afterMatch >= input.length || !/[a-zA-Z0-9_]/.test(input[afterMatch])) {
        const start = pos;
        pos += value.length;
        return { type: 'literal_timeofday', value, position: start };
      }
    }
    return undefined;
  };

  const tryReadDuration = (): Token | undefined => {
    const remaining = input.slice(pos);
    const durMatch = remaining.match(DURATION_PATTERN);
    if (durMatch) {
      const start = pos;
      const full = durMatch[0];
      pos += full.length;
      // Extract value between duration'...'
      const value = full.slice("duration'".length, full.length - 1);
      return { type: 'literal_duration', value, position: start };
    }
    return undefined;
  };

  /**
   * Try to read an enum literal: Namespace.Type'Member' pattern.
   * Only called when we've read a word and the next char is a single quote,
   * AND the word contains a dot (indicating a namespace-qualified type).
   */
  const tryReadEnum = (word: string, start: number): Token | undefined => {
    // The word must contain a dot to be an enum (namespace qualified)
    if (!word.includes('.')) return undefined;
    // The current pos should be at a single quote
    if (pos >= input.length || input[pos] !== "'") return undefined;

    pos++; // skip opening quote
    let member = '';
    while (pos < input.length && input[pos] !== "'") {
      member += input[pos];
      pos++;
    }
    if (pos < input.length && input[pos] === "'") {
      pos++; // skip closing quote
      return { type: 'literal_enum', value: `${word}'${member}'`, position: start };
    }
    throw new LexerError('Unterminated enum literal', start);
  };

  const readWord = (): Token => {
    const start = pos;
    let word = '';
    while (pos < input.length && /[a-zA-Z0-9_.]/.test(input[pos])) {
      word += input[pos];
      pos++;
    }

    // Classify the word
    const lower = word.toLowerCase();

    // Check for duration literal: duration'...'
    if (lower === 'duration' && pos < input.length && input[pos] === "'") {
      // Back up and use tryReadDuration from the start
      pos = start;
      const durToken = tryReadDuration();
      if (durToken) return durToken;
      // If it didn't match, re-read the word
      pos = start;
      word = '';
      while (pos < input.length && /[a-zA-Z0-9_.]/.test(input[pos])) {
        word += input[pos];
        pos++;
      }
    }

    // Check for enum literal: Namespace.Type'Member'
    if (word.includes('.') && pos < input.length && input[pos] === "'") {
      const enumToken = tryReadEnum(word, start);
      if (enumToken) return enumToken;
    }

    if (lower === 'null') {
      return { type: 'literal_null', value: word, position: start };
    }
    if (BOOLEANS.has(lower)) {
      return { type: 'literal_boolean', value: lower, position: start };
    }
    if (lower === 'not') {
      return { type: 'not_op', value: lower, position: start };
    }

    // For 'in', only treat as operator when between two expressions
    // (i.e., preceded by a value-producing token)
    if (lower === 'in') {
      const lastToken = tokens.length > 0 ? tokens[tokens.length - 1] : undefined;
      const isAfterValue =
        lastToken &&
        (lastToken.type === 'property' ||
          lastToken.type === 'literal_string' ||
          lastToken.type === 'literal_number' ||
          lastToken.type === 'literal_boolean' ||
          lastToken.type === 'literal_null' ||
          lastToken.type === 'literal_date' ||
          lastToken.type === 'literal_datetimeoffset' ||
          lastToken.type === 'literal_timeofday' ||
          lastToken.type === 'literal_duration' ||
          lastToken.type === 'literal_guid' ||
          lastToken.type === 'literal_enum' ||
          lastToken.type === 'rparen');
      if (isAfterValue) {
        return { type: 'comparison_op', value: lower, position: start };
      }
      // Otherwise treat as property name
      // But first check if followed by '(' — then it could be a function
      skipWhitespace();
      if (pos < input.length && input[pos] === '(') {
        return { type: 'function', value: word, position: start };
      }
      return { type: 'property', value: word, position: start };
    }

    if (COMPARISON_OPS.has(lower)) {
      return { type: 'comparison_op', value: lower, position: start };
    }
    if (LOGICAL_OPS.has(lower)) {
      return { type: 'logical_op', value: lower, position: start };
    }
    if (ARITHMETIC_OPS.has(lower)) {
      return { type: 'arithmetic_op', value: lower, position: start };
    }

    // Check if this is a lambda operator (any/all) — they follow a '/'
    if (LAMBDA_OPS.has(lower)) {
      const lastToken = tokens.length > 0 ? tokens[tokens.length - 1] : undefined;
      if (lastToken && lastToken.type === 'slash') {
        return { type: 'lambda_op', value: lower, position: start };
      }
    }

    // Check if this is a function call (word immediately followed by '(')
    skipWhitespace();
    if (pos < input.length && input[pos] === '(') {
      if (FUNCTIONS.has(lower) || FUNCTIONS.has(word)) {
        return { type: 'function', value: FUNCTIONS.has(word) ? word : lower, position: start };
      }
      // Check for lambda ops used as function-like syntax: any(...) / all(...)
      if (LAMBDA_OPS.has(lower)) {
        return { type: 'lambda_op', value: lower, position: start };
      }
      // Unknown function — treat as function, let parser handle
      return { type: 'function', value: word, position: start };
    }

    // It's a property name
    return { type: 'property', value: word, position: start };
  };

  while (pos < input.length) {
    skipWhitespace();
    if (pos >= input.length) break;

    const ch = input[pos];

    // Try to match date/datetime/guid literals when starting with a digit
    if (/[0-9]/.test(ch)) {
      // Try GUID first (starts with hex digit)
      const guidToken = tryReadGuid();
      if (guidToken) {
        tokens.push(guidToken);
        continue;
      }

      // Try datetime/date patterns
      const dtToken = tryReadDateTimeGuid();
      if (dtToken) {
        tokens.push(dtToken);
        continue;
      }

      // Try TimeOfDay (HH:MM:SS)
      const todToken = tryReadTimeOfDay();
      if (todToken) {
        tokens.push(todToken);
        continue;
      }

      // Fall through to regular number
      tokens.push(readNumber());
      continue;
    }

    if (ch === "'") {
      tokens.push(readString());
    } else if (ch === '(') {
      tokens.push({ type: 'lparen', value: '(', position: pos });
      pos++;
    } else if (ch === ')') {
      tokens.push({ type: 'rparen', value: ')', position: pos });
      pos++;
    } else if (ch === ',') {
      tokens.push({ type: 'comma', value: ',', position: pos });
      pos++;
    } else if (ch === ':') {
      tokens.push({ type: 'colon', value: ':', position: pos });
      pos++;
    } else if (ch === '/') {
      tokens.push({ type: 'slash', value: '/', position: pos });
      pos++;
    } else if (ch === '-' && pos + 1 < input.length && /[0-9]/.test(input[pos + 1])) {
      // Negative number — only if preceded by an operator or start of input
      const lastToken = tokens.length > 0 ? tokens[tokens.length - 1] : undefined;
      const isAfterOperator =
        !lastToken ||
        lastToken.type === 'comparison_op' ||
        lastToken.type === 'logical_op' ||
        lastToken.type === 'arithmetic_op' ||
        lastToken.type === 'not_op' ||
        lastToken.type === 'lparen' ||
        lastToken.type === 'comma';
      if (isAfterOperator) {
        tokens.push(readNumber());
      } else {
        // Could be 'sub' shorthand — treat as error, let parser handle
        throw new LexerError(`Unexpected character '${ch}'`, pos);
      }
    } else if (/[a-zA-Z_]/.test(ch)) {
      tokens.push(readWord());
    } else {
      throw new LexerError(`Unexpected character '${ch}'`, pos);
    }
  }

  return tokens;
};
