// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp;

import com.intellij.lang.ASTNode;
import com.intellij.lang.LightPsiParser;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;
import com.intellij.lang.PsiParser;
import com.intellij.psi.tree.IElementType;

import static com.en_circle.slt.plugin.lisp.LispParserUtil.*;
import static com.en_circle.slt.plugin.lisp.psi.LispTypes.*;

@SuppressWarnings({"SimplifiableIfStatement", "UnusedAssignment"})
public class LispParser implements PsiParser, LightPsiParser {

  public ASTNode parse(IElementType t, PsiBuilder b) {
    parseLight(t, b);
    return b.getTreeBuilt();
  }

  public void parseLight(IElementType t, PsiBuilder b) {
    boolean r;
    b = adapt_builder_(t, b, this, null);
    Marker m = enter_section_(b, 0, _COLLAPSE_, null);
    r = parse_root_(t, b);
    exit_section_(b, 0, m, t, r, true, TRUE_CONDITION);
  }

  protected boolean parse_root_(IElementType t, PsiBuilder b) {
    return parse_root_(t, b, 0);
  }

  static boolean parse_root_(IElementType t, PsiBuilder b, int l) {
    return lispFile(b, l + 1);
  }

  /* ********************************************************** */
  // ARRAY_START list
  public static boolean array(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "array")) return false;
    if (!nextTokenIs(b, ARRAY_START)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ARRAY_START);
    r = r && list(b, l + 1);
    exit_section_(b, m, ARRAY, r);
    return r;
  }

  /* ********************************************************** */
  // BINARY_NUMBER_TOKEN
  public static boolean binary_number(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "binary_number")) return false;
    if (!nextTokenIs(b, BINARY_NUMBER_TOKEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, BINARY_NUMBER_TOKEN);
    exit_section_(b, m, BINARY_NUMBER, r);
    return r;
  }

  /* ********************************************************** */
  // LINE_COMMENT | BLOCK_COMMENT
  public static boolean comment(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "comment")) return false;
    if (!nextTokenIs(b, "<comment>", BLOCK_COMMENT, LINE_COMMENT)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, COMMENT, "<comment>");
    r = consumeToken(b, LINE_COMMENT);
    if (!r) r = consumeToken(b, BLOCK_COMMENT);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // tested | evaled | pathname | UNDEFINED_SEQUENCE | BIT_ARRAY | CHARACTER
  //             | number | real_pair
  //             | symbol
  //             | string | vector | array | structure | list | pair
  public static boolean datum(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "datum")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, DATUM, "<datum>");
    r = tested(b, l + 1);
    if (!r) r = evaled(b, l + 1);
    if (!r) r = pathname(b, l + 1);
    if (!r) r = consumeToken(b, UNDEFINED_SEQUENCE);
    if (!r) r = consumeToken(b, BIT_ARRAY);
    if (!r) r = consumeToken(b, CHARACTER);
    if (!r) r = number(b, l + 1);
    if (!r) r = real_pair(b, l + 1);
    if (!r) r = symbol(b, l + 1);
    if (!r) r = string(b, l + 1);
    if (!r) r = vector(b, l + 1);
    if (!r) r = array(b, l + 1);
    if (!r) r = structure(b, l + 1);
    if (!r) r = list(b, l + 1);
    if (!r) r = pair(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // REFERENCE_SET | REFERENCE_LABEL | TEST_SUCCESS | COMMA | BACKQUOTE | QUOTE | FUNCTION
  public static boolean enhancement(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "enhancement")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, ENHANCEMENT, "<enhancement>");
    r = consumeToken(b, REFERENCE_SET);
    if (!r) r = consumeToken(b, REFERENCE_LABEL);
    if (!r) r = consumeToken(b, TEST_SUCCESS);
    if (!r) r = consumeToken(b, COMMA);
    if (!r) r = consumeToken(b, BACKQUOTE);
    if (!r) r = consumeToken(b, QUOTE);
    if (!r) r = consumeToken(b, FUNCTION);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // EVAL_VALUE sexpr
  public static boolean evaled(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "evaled")) return false;
    if (!nextTokenIs(b, EVAL_VALUE)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, EVAL_VALUE);
    r = r && sexpr(b, l + 1);
    exit_section_(b, m, EVALED, r);
    return r;
  }

  /* ********************************************************** */
  // HEX_NUMBER_TOKEN
  public static boolean hex_number(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "hex_number")) return false;
    if (!nextTokenIs(b, HEX_NUMBER_TOKEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, HEX_NUMBER_TOKEN);
    exit_section_(b, m, HEX_NUMBER, r);
    return r;
  }

  /* ********************************************************** */
  // INTEGER_NUMBER
  public static boolean integer(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "integer")) return false;
    if (!nextTokenIs(b, INTEGER_NUMBER)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, INTEGER_NUMBER);
    exit_section_(b, m, INTEGER, r);
    return r;
  }

  /* ********************************************************** */
  // sexpr*
  static boolean lispFile(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lispFile")) return false;
    while (true) {
      int c = current_position_(b);
      if (!sexpr(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "lispFile", c)) break;
    }
    return true;
  }

  /* ********************************************************** */
  // LPAREN sexpr* RPAREN
  public static boolean list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "list")) return false;
    if (!nextTokenIs(b, LPAREN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && list_1(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, LIST, r);
    return r;
  }

  // sexpr*
  private static boolean list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "list_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!sexpr(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "list_1", c)) break;
    }
    return true;
  }

  /* ********************************************************** */
  // binary_number | octal_number | hex_number | radix_number | integer | ratio | real
  public static boolean number(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "number")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, NUMBER, "<number>");
    r = binary_number(b, l + 1);
    if (!r) r = octal_number(b, l + 1);
    if (!r) r = hex_number(b, l + 1);
    if (!r) r = radix_number(b, l + 1);
    if (!r) r = integer(b, l + 1);
    if (!r) r = ratio(b, l + 1);
    if (!r) r = real(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // OCTAL_NUMBER_TOKEN
  public static boolean octal_number(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "octal_number")) return false;
    if (!nextTokenIs(b, OCTAL_NUMBER_TOKEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, OCTAL_NUMBER_TOKEN);
    exit_section_(b, m, OCTAL_NUMBER, r);
    return r;
  }

  /* ********************************************************** */
  // LPAREN sexpr+ DOT sexpr RPAREN
  public static boolean pair(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pair")) return false;
    if (!nextTokenIs(b, LPAREN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && pair_1(b, l + 1);
    r = r && consumeToken(b, DOT);
    r = r && sexpr(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, PAIR, r);
    return r;
  }

  // sexpr+
  private static boolean pair_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pair_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = sexpr(b, l + 1);
    while (r) {
      int c = current_position_(b);
      if (!sexpr(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "pair_1", c)) break;
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // PATHNAME_INDICATOR sexpr
  public static boolean pathname(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pathname")) return false;
    if (!nextTokenIs(b, PATHNAME_INDICATOR)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PATHNAME_INDICATOR);
    r = r && sexpr(b, l + 1);
    exit_section_(b, m, PATHNAME, r);
    return r;
  }

  /* ********************************************************** */
  // RADIX_NUMBER_TOKEN
  public static boolean radix_number(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "radix_number")) return false;
    if (!nextTokenIs(b, RADIX_NUMBER_TOKEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, RADIX_NUMBER_TOKEN);
    exit_section_(b, m, RADIX_NUMBER, r);
    return r;
  }

  /* ********************************************************** */
  // RATIO_NUMBER
  public static boolean ratio(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ratio")) return false;
    if (!nextTokenIs(b, RATIO_NUMBER)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, RATIO_NUMBER);
    exit_section_(b, m, RATIO, r);
    return r;
  }

  /* ********************************************************** */
  // REAL_NUMBER
  public static boolean real(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "real")) return false;
    if (!nextTokenIs(b, REAL_NUMBER)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, REAL_NUMBER);
    exit_section_(b, m, REAL, r);
    return r;
  }

  /* ********************************************************** */
  // REAL_PAIR_START LPAREN real real RPAREN
  public static boolean real_pair(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "real_pair")) return false;
    if (!nextTokenIs(b, REAL_PAIR_START)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, REAL_PAIR_START, LPAREN);
    r = r && real(b, l + 1);
    r = r && real(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, REAL_PAIR, r);
    return r;
  }

  /* ********************************************************** */
  // (enhancement* datum) | comment
  public static boolean sexpr(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sexpr")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, SEXPR, "<sexpr>");
    r = sexpr_0(b, l + 1);
    if (!r) r = comment(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // enhancement* datum
  private static boolean sexpr_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sexpr_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = sexpr_0_0(b, l + 1);
    r = r && datum(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // enhancement*
  private static boolean sexpr_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sexpr_0_0")) return false;
    while (true) {
      int c = current_position_(b);
      if (!enhancement(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "sexpr_0_0", c)) break;
    }
    return true;
  }

  /* ********************************************************** */
  // STRING_TOKEN
  public static boolean string(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "string")) return false;
    if (!nextTokenIs(b, STRING_TOKEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, STRING_TOKEN);
    exit_section_(b, m, STRING, r);
    return r;
  }

  /* ********************************************************** */
  // STRUCTURE_TOKEN list
  public static boolean structure(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "structure")) return false;
    if (!nextTokenIs(b, STRUCTURE_TOKEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, STRUCTURE_TOKEN);
    r = r && list(b, l + 1);
    exit_section_(b, m, STRUCTURE, r);
    return r;
  }

  /* ********************************************************** */
  // UNINTERN? SYMBOL_TOKEN
  public static boolean symbol(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "symbol")) return false;
    if (!nextTokenIs(b, "<symbol>", SYMBOL_TOKEN, UNINTERN)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, SYMBOL, "<symbol>");
    r = symbol_0(b, l + 1);
    r = r && consumeToken(b, SYMBOL_TOKEN);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // UNINTERN?
  private static boolean symbol_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "symbol_0")) return false;
    consumeToken(b, UNINTERN);
    return true;
  }

  /* ********************************************************** */
  // (TEST_SUCCESS | TEST_FALURE) sexpr
  public static boolean tested(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tested")) return false;
    if (!nextTokenIs(b, "<tested>", TEST_FALURE, TEST_SUCCESS)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, TESTED, "<tested>");
    r = tested_0(b, l + 1);
    r = r && sexpr(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // TEST_SUCCESS | TEST_FALURE
  private static boolean tested_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tested_0")) return false;
    boolean r;
    r = consumeToken(b, TEST_SUCCESS);
    if (!r) r = consumeToken(b, TEST_FALURE);
    return r;
  }

  /* ********************************************************** */
  // HASH_LPAREN sexpr* RPAREN
  public static boolean vector(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "vector")) return false;
    if (!nextTokenIs(b, HASH_LPAREN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, HASH_LPAREN);
    r = r && vector_1(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, VECTOR, r);
    return r;
  }

  // sexpr*
  private static boolean vector_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "vector_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!sexpr(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "vector_1", c)) break;
    }
    return true;
  }

}
