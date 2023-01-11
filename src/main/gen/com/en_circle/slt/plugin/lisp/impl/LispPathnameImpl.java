// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.impl;

import com.en_circle.slt.plugin.lisp.psi.LispPathname;
import com.en_circle.slt.plugin.lisp.psi.LispSexpr;
import com.en_circle.slt.plugin.lisp.psi.LispVisitor;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import org.jetbrains.annotations.NotNull;

public class LispPathnameImpl extends ASTWrapperPsiElement implements LispPathname {

  public LispPathnameImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull LispVisitor visitor) {
    visitor.visitPathname(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof LispVisitor) accept((LispVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public LispSexpr getSexpr() {
    return findNotNullChildByClass(LispSexpr.class);
  }

}
