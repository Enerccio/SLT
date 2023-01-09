// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.psi;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import org.jetbrains.annotations.NotNull;

public class LispVisitor extends PsiElementVisitor {

  public void visitAtom(@NotNull LispAtom o) {
    visitPsiElement(o);
  }

  public void visitList(@NotNull LispList o) {
    visitPsiElement(o);
  }

  public void visitNumber(@NotNull LispNumber o) {
    visitPsiElement(o);
  }

  public void visitPair(@NotNull LispPair o) {
    visitPsiElement(o);
  }

  public void visitSexpr(@NotNull LispSexpr o) {
    visitPsiElement(o);
  }

  public void visitString(@NotNull LispString o) {
    visitPsiElement(o);
  }

  public void visitSugar(@NotNull LispSugar o) {
    visitPsiElement(o);
  }

  public void visitSymbol(@NotNull LispSymbol o) {
    visitNamedElement(o);
  }

  public void visitNamedElement(@NotNull LispNamedElement o) {
    visitPsiElement(o);
  }

  public void visitPsiElement(@NotNull PsiElement o) {
    visitElement(o);
  }

}
