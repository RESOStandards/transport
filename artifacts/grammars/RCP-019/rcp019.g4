/*
 RESO RCP-019 Validation Expression Grammar
 
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

grammar rcp019;

// TODO: import https://github.com/antlr/grammars-v4/blob/master/pcre/PCRE.g4

options {
	tokenVocab = rcp019Lexer;
}

// Parse an entire expression without leaving any extra input leftover
topExp: exp EOF;

exp: orExp | collectionExp | funcExp;

orExp: andExp (OR andExp)*;

andExp: notExp (AND notExp)*;

notExp: NOT notExp | eqExp;

eqExp: cmpExp | cmpExp (NE | EQ) cmpExp;

cmpExp: cntExp | cntExp (LTE | GTE | LT | GT) cntExp;

cntExp: sumExp | sumExp (CONTAINS | IN) sumExp;

sumExp: prodExp ((PLUS | MINUS | CONCAT) prodExp)*;

prodExp: atomExp ((ASTERISK | SLASH | MOD) atomExp)*;

// NOTE: original VE writing had that all lists of size 1 were atomExp, not list. LIST() and SET()
// were created as a top-level item called 'collection' and should be used instead.
atomExp: LPAREN exp RPAREN | listExp | value;

// this was left in for backwards compatibility with the first production rule, LPAREN exp RPAREN
listExp: LPAREN (exp (COMMA exp)*)? RPAREN;

// this was previously an atomExp, but was moved to the top-level for faster parsing (10x speedup)
funcExp: func LPAREN (param (COMMA param)*)? RPAREN;

collectionExp: (LIST | SET) LPAREN (exp (COMMA exp)*)? RPAREN
	| (UNION | INTERSECTION | DIFFERENCE) LPAREN (
		collectionExp COMMA collectionExp (COMMA collectionExp)*
	)? RPAREN;

param: exp;

value:
	fieldName
	| specValue
	| charValue
	| intValue
	| floatValue
	| timeValue;

fieldName: (LAST)? FIELD_NAME
	| LBRACKET (LAST)? FIELD_NAME RBRACKET;

specValue: DOT SPECOP DOT;
charValue: QUOTED_TERM;
timeValue: HASH ISO_TIMESTAMP HASH;
intValue: (PLUS | MINUS)? DIGIT+;
floatValue: intValue DOT DIGIT+;

// SPECFUNC was added. LOCALFUNC could be added as well, with corresponding known local functions
func: SPECFUNC | ALPHANUM;