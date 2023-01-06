// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;
import static com.en_circle.slt.plugin.lisp.psi.LispTypes.*;
import static com.en_circle.slt.plugin.lisp.LispParserUtil.*;
import com.intellij.psi.tree.IElementType;
import com.intellij.lang.ASTNode;
import com.intellij.psi.tree.TokenSet;
import com.intellij.lang.PsiParser;
import com.intellij.lang.LightPsiParser;

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
  // number | string | symbol
  public static boolean atom(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atom")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, ATOM, "<atom>");
    r = number(b, l + 1);
    if (!r) r = string(b, l + 1);
    if (!r) r = symbol(b, l + 1);
    exit_section_(b, l, m, r, false, null);
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
  // LPARAM sexpr* RPARAM
  public static boolean list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "list")) return false;
    if (!nextTokenIs(b, LPARAM)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPARAM);
    r = r && list_1(b, l + 1);
    r = r && consumeToken(b, RPARAM);
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
  // NUMBER_TOKEN
  public static boolean number(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "number")) return false;
    if (!nextTokenIs(b, NUMBER_TOKEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, NUMBER_TOKEN);
    exit_section_(b, m, NUMBER, r);
    return r;
  }

  /* ********************************************************** */
  // LPARAM sexpr+ DOT sexpr RPARAM
  public static boolean pair(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pair")) return false;
    if (!nextTokenIs(b, LPARAM)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPARAM);
    r = r && pair_1(b, l + 1);
    r = r && consumeToken(b, DOT);
    r = r && sexpr(b, l + 1);
    r = r && consumeToken(b, RPARAM);
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
  // sugar* (pair|list|atom|COMMENT)
  public static boolean sexpr(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sexpr")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, SEXPR, "<sexpr>");
    r = sexpr_0(b, l + 1);
    r = r && sexpr_1(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // sugar*
  private static boolean sexpr_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sexpr_0")) return false;
    while (true) {
      int c = current_position_(b);
      if (!sugar(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "sexpr_0", c)) break;
    }
    return true;
  }

  // pair|list|atom|COMMENT
  private static boolean sexpr_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sexpr_1")) return false;
    boolean r;
    r = pair(b, l + 1);
    if (!r) r = list(b, l + 1);
    if (!r) r = atom(b, l + 1);
    if (!r) r = consumeToken(b, COMMENT);
    return r;
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
  // QUOTE | BACKQUOTE | (COMMA AMPERSAND?)
  public static boolean sugar(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sugar")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, SUGAR, "<sugar>");
    r = consumeToken(b, QUOTE);
    if (!r) r = consumeToken(b, BACKQUOTE);
    if (!r) r = sugar_2(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // COMMA AMPERSAND?
  private static boolean sugar_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sugar_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && sugar_2_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // AMPERSAND?
  private static boolean sugar_2_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sugar_2_1")) return false;
    consumeToken(b, AMPERSAND);
    return true;
  }

  /* ********************************************************** */
  // IDENTIFIER_TOKEN
  public static boolean symbol(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "symbol")) return false;
    if (!nextTokenIs(b, IDENTIFIER_TOKEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, IDENTIFIER_TOKEN);
    exit_section_(b, m, SYMBOL, r);
    return r;
  }

}
