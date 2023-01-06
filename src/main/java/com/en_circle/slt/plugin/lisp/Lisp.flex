package com.en_circle.slt.plugin.lisp;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.TokenType;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;

%%

%class LispLexer
%implements FlexLexer
%unicode
%function advance
%type IElementType
%eof{  return;
%eof}

%{
  StringBuffer string = new StringBuffer();
  StringBuffer identifier = new StringBuffer();

  IElementType processIdentifier(boolean unread) {
        yybegin(YYINITIAL);
        if (unread)
          yypushback(1);
        String value = identifier.toString();
        return LispTypes.IDENTIFIER_TOKEN;
    }
%}

%state STRING
%state IDENTIFIER

WHITE_SPACE=[\ \n\t\f\r]
END_OF_LINE_COMMENT=(";")[^\r\n]*
LPARAM=["("]
RPARAM=[")"]
COMMA=[","]
AMPERSAND=["@"]
BACKQUOTE=["`"]
QUOTE=["'"]
DOT=["."]
INTEGER_LITERAL = 0 | [1-9][0-9]*
BIN_LITERAL = (0b)[0-1]*
OCT_LITERAL = (0o)[0-7]*
HEX_LITERAL = (0x)[(1-9)|(a-f)|(A-F)]*
FLOAT_LITERAL = {INTEGER_LITERAL}\.{INTEGER_LITERAL}
IDENTIFIER_TOKEN_START=[^\ \r\n\f\r\t\(\)\,\@`\'\"0123456789\.]

%%

<YYINITIAL> {END_OF_LINE_COMMENT}
    { yybegin(YYINITIAL); return LispTypes.COMMENT; }
<YYINITIAL> {WHITE_SPACE}+
    { yybegin(YYINITIAL); return TokenType.WHITE_SPACE; }

<YYINITIAL> {LPARAM}
    { yybegin(YYINITIAL); return LispTypes.LPARAM; }
<YYINITIAL> {RPARAM}
    { yybegin(YYINITIAL); return LispTypes.RPARAM; }
<YYINITIAL> {AMPERSAND}
    { yybegin(YYINITIAL); return LispTypes.AMPERSAND; }
<YYINITIAL> {COMMA}
    { yybegin(YYINITIAL); return LispTypes.COMMA; }
<YYINITIAL> {BACKQUOTE}
    { yybegin(YYINITIAL); return LispTypes.BACKQUOTE; }
<YYINITIAL> {DOT}
    { yybegin(YYINITIAL); return LispTypes.DOT; }
<YYINITIAL> {QUOTE}
    { yybegin(YYINITIAL); return LispTypes.QUOTE; }
<YYINITIAL> {INTEGER_LITERAL}|{BIN_LITERAL}|{OCT_LITERAL}|{HEX_LITERAL}|{FLOAT_LITERAL}
    { yybegin(YYINITIAL); return LispTypes.NUMBER_TOKEN; }

<YYINITIAL> \"
    { string.setLength(0); yybegin(STRING); }
<YYINITIAL> {IDENTIFIER_TOKEN_START}
    { identifier.setLength(0); identifier.append(yytext()); yybegin(IDENTIFIER); }

<STRING> {
    \"                             { yybegin(YYINITIAL);
                                     return LispTypes.STRING_TOKEN; }
    [^\n\r\"\\]+                   { string.append( yytext() ); }
    \\t                            { string.append('\t'); }
    \\n                            { string.append('\n'); }

    \\r                            { string.append('\r'); }
    \\\"                           { string.append('\"'); }
    \\                             { string.append('\\'); }
  }

<IDENTIFIER> {
    [\ \r\n\f\r\t\(\)\,\@`\'\"\;]
      {
            return processIdentifier(true);
      }

    <<EOF>> {
          return processIdentifier(false);
      }

    [^\ \r\n\f\t\r\(\)\,\@`\'\"\;]+
      { identifier.append( yytext() ); }
  }

[^]                              { throw new Error("Illegal character <"+
                                                    yytext()+">"); }