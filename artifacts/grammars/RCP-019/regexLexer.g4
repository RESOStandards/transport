/*
 * Copyright (c) 2014-2023 by Bart Kiers
 *
 * The MIT license.
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * Project      : PCRE Parser, an ANTLR 4 grammar for PCRE
 * Developed by : Bart Kiers, bart@big-o.nl
 * Also see     : https://github.com/bkiers/pcre-parser
 *
 * Based on http://www.pcre.org/pcre.txt
 * (REVISION Last updated: 14 June 2021)
 */
lexer grammar regexLexer;

/// \      general escape character with several uses
BSlash : '\\';

/// $      assert end of string (or line, in multiline mode)
Dollar : '$';

/// .      match any character except newline (by default)
Dot : '.';

/// [      start character class definition
OBrack : '[';

/// ^      assert start of string (or line, in multiline mode)
Caret : '^';

/// |      start of alternative branch
Pipe : '|';

/// ?      extends the meaning of (, also 0 or 1 quantifier.txt, also quantifier.txt minimizer
QMark : '?';

/// *      0 or more quantifier.txt
Star : '*';

/// +      1 or more quantifier.txt, also "possessive quantifier.txt"
Plus : '+';

/// {      start min/max quantifier.txt
OBrace : '{';

CBrace : '}';

/// (      start subpattern
OPar : '(';

/// )      end subpattern
CPar : ')';

/// ]      terminates the character class
CBrack : ']';

OPosixBrack : '[:';
CPosixBrack : ':]';

Comma : ',';
Dash : '-';
UScore : '_';
Eq : '=';
Amp : '&';
Lt : '<';
Gt : '>';
Quote : '\'';
Col : ':';
Hash : '#';
Excl : '!';
