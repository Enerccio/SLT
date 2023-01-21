package com.en_circle.slt.plugin.lisp;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.TokenType;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.en_circle.slt.plugin.lisp.LispLexerUtils;
import com.en_circle.slt.plugin.lisp.number.LispNumberLexerAdapter;

%%

%class LispLexer
%implements FlexLexer
%unicode
%function advance
%type IElementType
%eof{  return;
%eof}

%{

LispNumberLexerAdapter numberLexer = new LispNumberLexerAdapter();
int escapeCount = 0;
StringBuffer tokenBuffer = new StringBuffer();

IElementType processBuffer(boolean unget) {
      if (unget)
          yypushback(1);
      return LispLexerUtils.processToken(numberLexer, tokenBuffer.toString());
}

%}

%state LINE_COMMENT
%state STRING
%state STRING_ESCAPE
%state SHARPSIGN
%state BIT_ARRAY
%state CHARACTER
%state BLOCK_COMMENT
%state BLOCK_COMMENT_TEST
%state BINARY_NUM
%state OCTAL_NUM
%state RADIX_NUM
%state HEX_NUM
%state STEP8
%state STEP8ESCAPE
%state STEP9
%state STEP9ESCAPE

WHITESPACE_CHARACTER=[\r\n\t\ \x0c\x0a]
CONSTITUENT_CHARACTER=[!$%&*+\-\./0-9:<=>?@A-Za-z\[\]\^_{}~]
TERMINATING_MACRO_CHAR=[\"'\(\),;`]

%%

<YYINITIAL> [\(] { yybegin(YYINITIAL); return LispTypes.LPAREN;  }
<YYINITIAL> [\)] { yybegin(YYINITIAL); return LispTypes.RPAREN;  }
<YYINITIAL> [\'] { yybegin(YYINITIAL); return LispTypes.QUOTE;  }
<YYINITIAL> [;] { yybegin(LINE_COMMENT); }
<YYINITIAL> [\"] { yybegin(STRING); }
<YYINITIAL> [`] { yybegin(YYINITIAL); return LispTypes.BACKQUOTE;  }
<YYINITIAL> [,] { yybegin(YYINITIAL); return LispTypes.COMMA;  }
<YYINITIAL> [#] { yybegin(SHARPSIGN); }

<YYINITIAL> {WHITESPACE_CHARACTER}+ { yybegin(YYINITIAL); return TokenType.WHITE_SPACE; }
<YYINITIAL> ["|"] { yybegin(STEP9); escapeCount=1; tokenBuffer.setLength(0); }
<YYINITIAL> {CONSTITUENT_CHARACTER} { yybegin(STEP8); escapeCount=0; tokenBuffer.setLength(0); tokenBuffer.append(yytext()); }

<YYINITIAL> <<EOF>> { return null; }
<YYINITIAL> [^] { yybegin(YYINITIAL); return TokenType.ERROR_ELEMENT; }

<LINE_COMMENT> {
    [\r\n] { yybegin(YYINITIAL); return LispTypes.LINE_COMMENT; }
    <<EOF>> { yybegin(YYINITIAL); return LispTypes.LINE_COMMENT; }
    [^] { }
}

<STRING> {
    [\"] { yybegin(YYINITIAL); return LispTypes.STRING_TOKEN; }
    [\\] { yybegin(STRING_ESCAPE); }

    <<EOF>> { yybegin(YYINITIAL); return TokenType.ERROR_ELEMENT; }
    [^] { }
}

<STRING_ESCAPE> {
    [^] { yybegin(STRING); }
    <<EOF>> { yybegin(YYINITIAL); return TokenType.ERROR_ELEMENT; }
}

<SHARPSIGN> {
    [0-9] { }

    [\b\t\n\r\ ] { yybegin(YYINITIAL); return TokenType.ERROR_ELEMENT; }
    [!\"$%&] { yybegin(YYINITIAL); return LispTypes.UNDEFINED_SEQUENCE;  }
    ["#"] { yybegin(YYINITIAL); return LispTypes.REFERENCE_LABEL; }
    ["'"] { yybegin(YYINITIAL); return LispTypes.FUNCTION; }
    ["("] { yybegin(YYINITIAL); return LispTypes.HASH_LPAREN; }
    ["*"] { yybegin(BIT_ARRAY); }
    [","] { yybegin(YYINITIAL); return LispTypes.UNDEFINED_SEQUENCE; }
    [":"] { yybegin(YYINITIAL); return LispTypes.UNINTERN; }
    [";"] { yybegin(YYINITIAL); return LispTypes.UNDEFINED_SEQUENCE; }
    ["="] { yybegin(YYINITIAL); return LispTypes.REFERENCE_SET; }
    [>?@\[] { yybegin(YYINITIAL); return LispTypes.UNDEFINED_SEQUENCE; }
    ["\\"] { yybegin(CHARACTER); }
    [\]\^_`] { yybegin(YYINITIAL); return LispTypes.UNDEFINED_SEQUENCE; }
    ["|"] { yybegin(BLOCK_COMMENT); }
    [~{}] { yybegin(YYINITIAL); return LispTypes.UNDEFINED_SEQUENCE; }
    ["+"] { yybegin(YYINITIAL); return LispTypes.TEST_SUCCESS; }
    ["-"] { yybegin(YYINITIAL); return LispTypes.TEST_FALURE; }
    ["."] { yybegin(YYINITIAL); return LispTypes.EVAL_VALUE; }
    [/] { yybegin(YYINITIAL); return LispTypes.UNDEFINED_SEQUENCE; }
    [aA] { yybegin(YYINITIAL); return LispTypes.ARRAY_START; }
    [bB] { yybegin(BINARY_NUM); }
    [cC] { yybegin(YYINITIAL); return LispTypes.REAL_PAIR_START; }
    [d-n] { yybegin(YYINITIAL); return LispTypes.UNDEFINED_SEQUENCE; }
    [D-N] { yybegin(YYINITIAL); return LispTypes.UNDEFINED_SEQUENCE; }
    [oO] { yybegin(OCTAL_NUM); }
    [pP] { yybegin(YYINITIAL); return LispTypes.PATHNAME_INDICATOR; }
    [qQ] { yybegin(YYINITIAL); return LispTypes.UNDEFINED_SEQUENCE; }
    [rR] { yybegin(RADIX_NUM); }
    [sS] { yybegin(YYINITIAL); return LispTypes.STRUCTURE_TOKEN; }
    [t-w] { yybegin(YYINITIAL); return LispTypes.UNDEFINED_SEQUENCE; }
    [T-W] { yybegin(YYINITIAL); return LispTypes.UNDEFINED_SEQUENCE; }
    [xX] { yybegin(HEX_NUM); }
    [y-z] { yybegin(YYINITIAL); return LispTypes.UNDEFINED_SEQUENCE; }
    [Y-Z] { yybegin(YYINITIAL); return LispTypes.UNDEFINED_SEQUENCE; }

    <<EOF>> { yybegin(YYINITIAL); return TokenType.ERROR_ELEMENT; }
    [^] { yybegin(YYINITIAL); return TokenType.ERROR_ELEMENT; }
}

<BIT_ARRAY> {
    [0|1] { }

    <<EOF>> { yybegin(YYINITIAL); yypushback(1); return LispTypes.BIT_ARRAY; }
    [^] { yybegin(YYINITIAL); yypushback(1); return LispTypes.BIT_ARRAY; }
}

<CHARACTER> {
    <<EOF>> { yybegin(YYINITIAL); return TokenType.ERROR_ELEMENT; }
    [^] { yybegin(YYINITIAL); return LispTypes.CHARACTER; }
}

<BLOCK_COMMENT> {
    ["|"] { yybegin(BLOCK_COMMENT_TEST); }

    <<EOF>> { yybegin(YYINITIAL); return TokenType.ERROR_ELEMENT; }
    [^] { }
}

<BLOCK_COMMENT_TEST> {
    ["#"] { yybegin(YYINITIAL); return LispTypes.BLOCK_COMMENT; }

    <<EOF>> { yybegin(YYINITIAL); return TokenType.ERROR_ELEMENT; }
    [^] { yybegin(BLOCK_COMMENT); }
}

<BINARY_NUM> {
    [01/] { }

    <<EOF>> { yybegin(YYINITIAL); return TokenType.ERROR_ELEMENT; }
    [^] { yybegin(YYINITIAL); yypushback(1); return LispTypes.BINARY_NUMBER_TOKEN; }
}

<OCTAL_NUM> {
    [0-7/] { }

    <<EOF>> { yybegin(YYINITIAL); return TokenType.ERROR_ELEMENT; }
    [^] { yybegin(YYINITIAL); yypushback(1); return LispTypes.OCTAL_NUMBER_TOKEN; }
}

<RADIX_NUM> {
    [0-9a-zA-Z/] { }

    <<EOF>> { yybegin(YYINITIAL); return TokenType.ERROR_ELEMENT; }
    [^] { yybegin(YYINITIAL); yypushback(1); return LispTypes.RADIX_NUMBER_TOKEN; }
}


<HEX_NUM> {
    [0-9a-fA-F/] { }

    <<EOF>> { yybegin(YYINITIAL); return TokenType.ERROR_ELEMENT; }
    [^] { yybegin(YYINITIAL); yypushback(1); return LispTypes.HEX_NUMBER_TOKEN; }
}

<STEP8> {
    {CONSTITUENT_CHARACTER} | "#" { tokenBuffer.append(yytext()); }
    \\ { yybegin(STEP8ESCAPE); }
    "|" { yybegin(STEP9); escapeCount++; }
    {TERMINATING_MACRO_CHAR} { yybegin(YYINITIAL); return processBuffer(true); }
    {WHITESPACE_CHARACTER} { yybegin(YYINITIAL); return processBuffer(true); }
    <<EOF>> { yybegin(YYINITIAL); return processBuffer(false); }
}

<STEP8ESCAPE> {
     <<EOF>> { yybegin(YYINITIAL); return TokenType.ERROR_ELEMENT; }
     [^] { yybegin(STEP8); tokenBuffer.append(yytext()); }
}

<STEP9> {
    {CONSTITUENT_CHARACTER} | {TERMINATING_MACRO_CHAR} | {WHITESPACE_CHARACTER} | "#"  { tokenBuffer.append(yytext()); }
    \\ { yybegin(STEP9ESCAPE); }
     "|" { yybegin(STEP8); escapeCount++; }
     <<EOF>> { yybegin(YYINITIAL); return TokenType.ERROR_ELEMENT; }
}

<STEP9ESCAPE> {
     <<EOF>> { yybegin(YYINITIAL); return TokenType.ERROR_ELEMENT; }
     [^] { yybegin(STEP9); tokenBuffer.append(yytext()); }
}