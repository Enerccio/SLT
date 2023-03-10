// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.impl;

import com.en_circle.slt.plugin.lisp.psi.*;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class LispSexprImpl extends ASTWrapperPsiElement implements LispSexpr {

  public LispSexprImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull LispVisitor visitor) {
    visitor.visitSexpr(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof LispVisitor) accept((LispVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public LispComment getComment() {
    return findChildByClass(LispComment.class);
  }

  @Override
  @Nullable
  public LispDatum getDatum() {
    return findChildByClass(LispDatum.class);
  }

  @Override
  @NotNull
  public List<LispEnhancement> getEnhancementList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, LispEnhancement.class);
  }

}
