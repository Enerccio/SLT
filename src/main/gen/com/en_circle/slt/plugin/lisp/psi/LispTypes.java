// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.psi;

import com.en_circle.slt.plugin.lisp.impl.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;

public interface LispTypes {

  IElementType ATOM = new LispElementType("ATOM");
  IElementType LIST = new LispElementType("LIST");
  IElementType NUMBER = new LispElementType("NUMBER");
  IElementType PAIR = new LispElementType("PAIR");
  IElementType SEXPR = new LispElementType("SEXPR");
  IElementType STRING = new LispElementType("STRING");
  IElementType SUGAR = new LispElementType("SUGAR");
  IElementType SYMBOL = new LispElementType("SYMBOL");

  IElementType AMPERSAND = new LispTokenType("AMPERSAND");
  IElementType BACKQUOTE = new LispTokenType("BACKQUOTE");
  IElementType COMMA = new LispTokenType("COMMA");
  IElementType COMMENT = new LispTokenType("COMMENT");
  IElementType DOT = new LispTokenType("DOT");
  IElementType HASHTAG = new LispTokenType("HASHTAG");
  IElementType IDENTIFIER_TOKEN = new LispTokenType("IDENTIFIER_TOKEN");
  IElementType LPAREN = new LispTokenType("LPAREN");
  IElementType NUMBER_TOKEN = new LispTokenType("NUMBER_TOKEN");
  IElementType QUOTE = new LispTokenType("QUOTE");
  IElementType RPAREN = new LispTokenType("RPAREN");
  IElementType STRING_TOKEN = new LispTokenType("STRING_TOKEN");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
      if (type == ATOM) {
        return new LispAtomImpl(node);
      }
      else if (type == LIST) {
        return new LispListImpl(node);
      }
      else if (type == NUMBER) {
        return new LispNumberImpl(node);
      }
      else if (type == PAIR) {
        return new LispPairImpl(node);
      }
      else if (type == SEXPR) {
        return new LispSexprImpl(node);
      }
      else if (type == STRING) {
        return new LispStringImpl(node);
      }
      else if (type == SUGAR) {
        return new LispSugarImpl(node);
      }
      else if (type == SYMBOL) {
        return new LispSymbolImpl(node);
      }
      throw new AssertionError("Unknown element type: " + type);
    }
  }
}
