package com.en_circle.slt.plugin.lisp.number;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.TokenType;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;

%%

%class LispNumberLexer
%implements FlexLexer
%unicode
%function advance
%type IElementType
%eof{  return;
%eof}

%{

%}

DIGIT=[0-9]
SIGN=[+\-]
SLASH="/"
DOT="."
EXPONENT_MARKER=[dDeEfFlLsS]

%%

<YYINITIAL> {DIGIT} {  yybegin(YYINITIAL); return LispNumberType.DIGIT; }
<YYINITIAL> {SIGN} {  yybegin(YYINITIAL); return LispNumberType.SIGN; }
<YYINITIAL> {SLASH} {  yybegin(YYINITIAL); return LispNumberType.SLASH; }
<YYINITIAL> {DOT} {  yybegin(YYINITIAL); return LispTypes.DOT; }
<YYINITIAL> {EXPONENT_MARKER} {  yybegin(YYINITIAL); return LispNumberType.EXPONENT_MARKER; }

[^] { return TokenType.BAD_CHARACTER; }
