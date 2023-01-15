// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.psi;

import com.en_circle.slt.plugin.lisp.impl.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;

public interface LispTypes {

  IElementType ARRAY = new LispElementType("ARRAY");
  IElementType BINARY_NUMBER = new LispElementType("BINARY_NUMBER");
  IElementType COMMENT = new LispElementType("COMMENT");
  IElementType COMPOUND_SYMBOL = new LispElementType("COMPOUND_SYMBOL");
  IElementType DATUM = new LispElementType("DATUM");
  IElementType ENHANCEMENT = new LispElementType("ENHANCEMENT");
  IElementType EVALED = new LispElementType("EVALED");
  IElementType HEX_NUMBER = new LispElementType("HEX_NUMBER");
  IElementType INTEGER = new LispElementType("INTEGER");
  IElementType LIST = new LispElementType("LIST");
  IElementType NUMBER = new LispElementType("NUMBER");
  IElementType OCTAL_NUMBER = new LispElementType("OCTAL_NUMBER");
  IElementType PAIR = new LispElementType("PAIR");
  IElementType PATHNAME = new LispElementType("PATHNAME");
  IElementType RADIX_NUMBER = new LispElementType("RADIX_NUMBER");
  IElementType RATIO = new LispElementType("RATIO");
  IElementType REAL = new LispElementType("REAL");
  IElementType REAL_PAIR = new LispElementType("REAL_PAIR");
  IElementType SEXPR = new LispElementType("SEXPR");
  IElementType STRING = new LispElementType("STRING");
  IElementType STRUCTURE = new LispElementType("STRUCTURE");
  IElementType SYMBOL = new LispElementType("SYMBOL");
  IElementType TESTED = new LispElementType("TESTED");
  IElementType TOPLEVEL = new LispElementType("TOPLEVEL");
  IElementType VECTOR = new LispElementType("VECTOR");

  IElementType ARRAY_START = new LispTokenType("ARRAY_START");
  IElementType BACKQUOTE = new LispTokenType("BACKQUOTE");
  IElementType BINARY_NUMBER_TOKEN = new LispTokenType("BINARY_NUMBER_TOKEN");
  IElementType BIT_ARRAY = new LispTokenType("BIT_ARRAY");
  IElementType BLOCK_COMMENT = new LispTokenType("BLOCK_COMMENT");
  IElementType CHARACTER = new LispTokenType("CHARACTER");
  IElementType COMMA = new LispTokenType("COMMA");
  IElementType DOT = new LispTokenType("DOT");
  IElementType EVAL_VALUE = new LispTokenType("EVAL_VALUE");
  IElementType FUNCTION = new LispTokenType("FUNCTION");
  IElementType HASH_LPAREN = new LispTokenType("HASH_LPAREN");
  IElementType HEX_NUMBER_TOKEN = new LispTokenType("HEX_NUMBER_TOKEN");
  IElementType INTEGER_NUMBER = new LispTokenType("INTEGER_NUMBER");
  IElementType LINE_COMMENT = new LispTokenType("LINE_COMMENT");
  IElementType LPAREN = new LispTokenType("LPAREN");
  IElementType OCTAL_NUMBER_TOKEN = new LispTokenType("OCTAL_NUMBER_TOKEN");
  IElementType PATHNAME_INDICATOR = new LispTokenType("PATHNAME_INDICATOR");
  IElementType QUOTE = new LispTokenType("QUOTE");
  IElementType RADIX_NUMBER_TOKEN = new LispTokenType("RADIX_NUMBER_TOKEN");
  IElementType RATIO_NUMBER = new LispTokenType("RATIO_NUMBER");
  IElementType REAL_NUMBER = new LispTokenType("REAL_NUMBER");
  IElementType REAL_PAIR_START = new LispTokenType("REAL_PAIR_START");
  IElementType REFERENCE_LABEL = new LispTokenType("REFERENCE_LABEL");
  IElementType REFERENCE_SET = new LispTokenType("REFERENCE_SET");
  IElementType RPAREN = new LispTokenType("RPAREN");
  IElementType STRING_TOKEN = new LispTokenType("STRING_TOKEN");
  IElementType STRUCTURE_TOKEN = new LispTokenType("STRUCTURE_TOKEN");
  IElementType SYMBOL_TOKEN = new LispTokenType("SYMBOL_TOKEN");
  IElementType TEST_FALURE = new LispTokenType("TEST_FALURE");
  IElementType TEST_SUCCESS = new LispTokenType("TEST_SUCCESS");
  IElementType UNDEFINED_SEQUENCE = new LispTokenType("UNDEFINED_SEQUENCE");
  IElementType UNINTERN = new LispTokenType("UNINTERN");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
      if (type == ARRAY) {
        return new LispArrayImpl(node);
      }
      else if (type == BINARY_NUMBER) {
        return new LispBinaryNumberImpl(node);
      }
      else if (type == COMMENT) {
        return new LispCommentImpl(node);
      }
      else if (type == COMPOUND_SYMBOL) {
        return new LispCompoundSymbolImpl(node);
      }
      else if (type == DATUM) {
        return new LispDatumImpl(node);
      }
      else if (type == ENHANCEMENT) {
        return new LispEnhancementImpl(node);
      }
      else if (type == EVALED) {
        return new LispEvaledImpl(node);
      }
      else if (type == HEX_NUMBER) {
        return new LispHexNumberImpl(node);
      }
      else if (type == INTEGER) {
        return new LispIntegerImpl(node);
      }
      else if (type == LIST) {
        return new LispListImpl(node);
      }
      else if (type == NUMBER) {
        return new LispNumberImpl(node);
      }
      else if (type == OCTAL_NUMBER) {
        return new LispOctalNumberImpl(node);
      }
      else if (type == PAIR) {
        return new LispPairImpl(node);
      }
      else if (type == PATHNAME) {
        return new LispPathnameImpl(node);
      }
      else if (type == RADIX_NUMBER) {
        return new LispRadixNumberImpl(node);
      }
      else if (type == RATIO) {
        return new LispRatioImpl(node);
      }
      else if (type == REAL) {
        return new LispRealImpl(node);
      }
      else if (type == REAL_PAIR) {
        return new LispRealPairImpl(node);
      }
      else if (type == SEXPR) {
        return new LispSexprImpl(node);
      }
      else if (type == STRING) {
        return new LispStringImpl(node);
      }
      else if (type == STRUCTURE) {
        return new LispStructureImpl(node);
      }
      else if (type == SYMBOL) {
        return new LispSymbolImpl(node);
      }
      else if (type == TESTED) {
        return new LispTestedImpl(node);
      }
      else if (type == TOPLEVEL) {
        return new LispToplevelImpl(node);
      }
      else if (type == VECTOR) {
        return new LispVectorImpl(node);
      }
      throw new AssertionError("Unknown element type: " + type);
    }
  }
}
