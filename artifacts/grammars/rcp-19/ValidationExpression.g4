/*
 RESO RCP-19 Validation Expression Grammar

 By downloading this resource, you agree to the RESO EULA: https://www.reso.org/eula/ Contact
 dev@reso.org if you have questions

 Taken from: https://github.com/darnjo/rcp019/blob/master/LICENSE Copyright (c) 2018 Joshua Darnell
 (josh@kurotek.com)

 NOTE: This file is written with the intent of preserving the structure of the original RETS 1.9 VE
 grammar so that anyone who has implemented their systems using it shouldn't have to make any
 changes unless they are wanting to support the new features. In other words, all changes made to
 this grammar so far have been additive. There are more optimal representations of this file that
 will accept the RESO RCP-019 grammar, and as long as your parser supports an equivalent set of
 symbols and expressions, then the internals of your system are for you to decide. This example
 grammar should already perform reasonably well in most situations, but the parse tree will be a bit
 deep (and therefore not be as performant) due to the current structure.
 */

grammar ValidationExpression;

exp
  : orExp
  | collectionExp
  | funcExp
  ;

orExp
  : andExp (OR andExp)*
  ;

andExp
  : notExp (AND notExp)*
  ;

notExp
  : NOT notExp
  | eqExp
  ;

eqExp
  : cmpExp
  | cmpExp (NE | EQ) cmpExp
  ;

cmpExp
  : cntExp
  | cntExp (LTE | GTE | LT | GT) cntExp
  ;

cntExp
  : sumExp
  | sumExp (CONTAINS | IN) sumExp
  ;

sumExp
  : prodExp ((PLUS | MINUS | CONCAT) prodExp)*
  ;

prodExp
  : atomExp ((ASTERISK | SLASH | MOD) atomExp)*
  ;

// NOTE:  original VE writing had that all lists of size 1 were atomExp, not list.
// LIST() and SET() were created instead as a top-level item called 'collection' and should be used instead.
atomExp
  : LPAREN exp RPAREN
  | list
  | value
  ;

// this was left in for backwards compatibility with the first production rule, LPAREN exp RPAREN
list
  : LPAREN (exp (COMMA exp)*)? RPAREN
  ;

// this was previously an atomExp, but was moved to the top-level for faster parsing (10x speedup)
funcExp
  : func LPAREN (param (COMMA param)*)? RPAREN
  ;

collectionExp
  : (LIST | SET) LPAREN (exp (COMMA exp)*)? RPAREN
  | (UNION | INTERSECTION | DIFFERENCE) LPAREN (collectionExp COMMA collectionExp (COMMA collectionExp)*)? RPAREN
  ;

// SPECFUNC was added. LOCALFUNC could be added as well, with corresponding known local functions
func
  : SPECFUNC
  | ALPHANUM
  ;

param
  : exp
  ;

value
  : fieldName
  | specValue
  | charValue
  | intValue
  | floatValue
  | timeValue
  ;

fieldName
  : (LAST)? FIELD_NAME
  | LBRACKET (LAST)? FIELD_NAME RBRACKET
  ;

specValue : DOT FIELD_NAME DOT;
charValue : QUOTED_TERM;
timeValue : HASH ISO_TIMESTAMP HASH;
intValue : (PLUS | MINUS)? DIGIT+ ;
floatValue : intValue DOT DIGIT+;

// Tokens - may be moved to lexer file
CONCAT: PIPE;
LPAREN: '(';
RPAREN: ')';
SQUOTE: '\'';
QUOTE: '"';
DOT: '.';
ASTERISK: '*';
SLASH: '/';
EXCLAMATION: '!';
DOLLAR: '$';
CARET: '^';
BACKSLASH: '\\';
SINGLE_SPACE: ' ';
COLON: ':';
PIPE: '|';
LBRACKET: '[';
RBRACKET: ']';
HASH: '#';

OR: '.OR.';
AND: '.AND.';
NOT: '.NOT.';

EQ: '=';
NE: EXCLAMATION EQ;
LT: '<';
LTE: LT EQ;
GT: '>';
GTE: GT EQ;

CONTAINS: '.CONTAINS.';
IN: '.IN.';
COMMA: ',';
PLUS: '+';
MINUS: '-';
MOD: '.MOD.';

IIF: 'IIF';
MATCH: 'MATCH';
LAST: 'LAST';
LIST: 'LIST';
SET: 'SET';
DIFFERENCE: 'DIFFERENCE';
INTERSECTION: 'INTERSECTION';
UNION: 'UNION';
TRUE: 'TRUE';
FALSE: 'FALSE';
EMPTY: 'EMPTY';
NULL: 'NULL';
TODAY: 'TODAY';
NOW: 'NOW';
ENTRY: 'ENTRY';
OLDVALUE: 'OLDVALUE';
UPDATEACTION: 'UPDATEACTION';
ANY: 'any';

// See: Data Dictionary Member and Office Resources
MEMBER_LOGIN_ID: 'MEMBER_LOGIN_ID';
MEMBER_MLS_SECURITY_CLASS: 'MEMBER_MLS_SECURITY_CLASS';
MEMBER_TYPE: 'MEMBER_TYPE';
MEMBER_MLS_ID: 'MEMBER_MLS_ID';
OFFICE_BROKER_MLS_ID: 'OFFICE_BROKER_MLS_ID';
OFFICE_MLS_ID: 'OFFICE_MLS_ID';

ALPHA: ('a' ..'z' | 'A' ..'Z');
DIGIT: ('0' ..'9');

// special tokens
RESO_SPECIAL_TOKENS: FIELD_NAME | SPECOP;

// TODO: Dynamically fill in your FIELD_NAMEs here
FIELD_NAME:
	'ListPrice'
	| 'Status'
	| 'CloseDate'
	| 'Bedrooms'
	| 'Bathrooms';

SPECFUNC: IIF | MATCH;

SPECOP:
	EMPTY
	| TRUE
	| FALSE
	| TODAY
	| NOW
	| ENTRY
	| OLDVALUE
	| MEMBER_LOGIN_ID
	| MEMBER_MLS_SECURITY_CLASS
	| MEMBER_TYPE
	| MEMBER_MLS_ID
	| OFFICE_BROKER_MLS_ID
	| OFFICE_MLS_ID
	| UPDATEACTION
	| ANY;

ALPHANUM: ALPHA (ALPHA | DIGIT)*;

QUOTED_TERM: QUOTE (~[\\"])*? QUOTE | SQUOTE (~[\\'])*? SQUOTE;

// #2023-12-04T06:12:24.00Z# #2023-12-04T06:12:24.00-7:00# #2023-12-04T06:12:24.00+7:00#
ISO_TIMESTAMP:
	HASH YEAR '-' MONTH '-' DAY 'T' [0-23] COLON [0-59] COLON [0-59] DOT (
		DIGIT
	)* ('Z' | (PLUS | MINUS) [0-12] COLON [0-59]) HASH;

// #2023-12-04#
ISO_DATE: HASH YEAR '-' MONTH '-' DAY HASH;

YEAR: DIGIT DIGIT DIGIT DIGIT;
MONTH: [0-12];
DAY: [0-3] DIGIT;

//added support for c++ style comments
SLASH_STAR_COMMENT: '/*' .+? '*/' -> skip;
SLASH_SLASH_COMMENT: '//' .+? ('\n' | EOF) -> skip;

WS: [ \t\n\r]+ -> skip;
