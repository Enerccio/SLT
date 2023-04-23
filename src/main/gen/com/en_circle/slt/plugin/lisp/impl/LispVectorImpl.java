// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.impl;

import com.en_circle.slt.plugin.lisp.psi.*;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class LispVectorImpl extends ASTWrapperPsiElement implements LispVector {

  public LispVectorImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull LispVisitor visitor) {
    visitor.visitVector(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof LispVisitor) accept((LispVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public LispLhashparenthesis getLhashparenthesis() {
    return findNotNullChildByClass(LispLhashparenthesis.class);
  }

  @Override
  @NotNull
  public LispRparenthesis getRparenthesis() {
    return findNotNullChildByClass(LispRparenthesis.class);
  }

  @Override
  @NotNull
  public List<LispSexpr> getSexprList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, LispSexpr.class);
  }

}
