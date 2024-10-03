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
  : (LAST)? RETSNAME
  | LBRACKET (LAST)? RETSNAME RBRACKET
  ;

specValue : DOT RETSNAME DOT;
charValue : QUOTED_TERM;
timeValue : HASH RETSDATETIME HASH;
intValue : (PLUS | MINUS)? DIGIT+ ;
floatValue : intValue DOT DIGIT+;

// Tokens - may be moved to lexer file
CONCAT : PIPE;
LPAREN : '(' ;
RPAREN : ')' ;
SQUOTE : '\'' ;
QUOTE : '"' ;
DOT : '.' ;
ASTERISK : '*';
SLASH : '/';
EXCLAMATION : '!';

OR  : '.OR.';
AND : '.AND.';
NOT : '.NOT.';

EQ  : '=';
NE  : EXCLAMATION EQ;
LT  : '<';
LTE : LT EQ;
GT  : '>';
GTE : GT EQ;

CONTAINS : '.CONTAINS.';
IN : '.IN.';
COMMA: ',';
PLUS: '+';
MINUS: '-';
MOD: '.MOD.';
PIPE: '|';
LBRACKET: '[';
RBRACKET: ']';
HASH: '#';

IIF: 'IIF';
LAST: 'LAST';
LIST: 'LIST';
SET: 'SET';
DIFFERENCE: 'DIFFERENCE';
INTERSECTION: 'INTERSECTION';
UNION: 'UNION';
TRUE: 'TRUE';
FALSE: 'FALSE';
EMPTY: 'EMPTY';
TODAY: 'TODAY';
NOW: 'NOW';
ENTRY: 'ENTRY';
OLDVALUE: 'OLDVALUE';
USERID: 'USERID';
USERCLASS: 'USERCLASS';
USERLEVEL: 'USERLEVEL';
AGENTCODE: 'AGENTCODE';
BROKERCODE: 'BROKERCODE';
BROKERBRANCH: 'BROKERBRANCH';
UPDATEACTION: 'UPDATEACTION';
ANY: 'any';

// special tokens
RETSNAME
  : DICTNAME
  | SPECOP
  ;

// TODO: dynamically fill in your dictnames here
DICTNAME
  : 'ListPrice'
  | 'Status'
  | 'CloseDate'
  | 'Bedrooms'
  | 'Bathrooms'
  ;

SPECFUNC
  : IIF
  ;

SPECOP
  : EMPTY
  | TRUE
  | FALSE
  | TODAY
  | NOW
  | ENTRY
  | OLDVALUE
  | USERID
  | USERCLASS
  | USERLEVEL
  | AGENTCODE
  | BROKERCODE
  | BROKERBRANCH
  | UPDATEACTION
  | ANY
  ;

RETSDATETIME: '##TODO##';
ALPHA: ('a'..'z' | 'A'..'Z');
DIGIT: ('0'..'9');

ALPHANUM: ALPHA (ALPHA|DIGIT)*;

QUOTED_TERM
    :   QUOTE (~[\\"])*? QUOTE
    |   SQUOTE (~[\\'])*? SQUOTE
    ;

//added support for c++ style comments
SLASH_STAR_COMMENT  : '/*' .+? '*/' -> skip ;
SLASH_SLASH_COMMENT : '//' .+? ('\n'|EOF) -> skip ;

WS : [ \t\n\r]+ -> skip ;
