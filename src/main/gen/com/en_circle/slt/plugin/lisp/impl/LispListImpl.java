// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.impl;

import com.en_circle.slt.plugin.lisp.psi.*;
import com.en_circle.slt.plugin.lisp.psi.impl.LispPsiImplUtil;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.navigation.ItemPresentation;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class LispListImpl extends ASTWrapperPsiElement implements LispList {

  public LispListImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull LispVisitor visitor) {
    visitor.visitList(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof LispVisitor) accept((LispVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public LispLparenthesis getLparenthesis() {
    return findNotNullChildByClass(LispLparenthesis.class);
  }

  @Override
  @Nullable
  public LispRparenthesis getRparenthesis() {
    return findChildByClass(LispRparenthesis.class);
  }

  @Override
  @NotNull
  public List<LispSexpr> getSexprList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, LispSexpr.class);
  }

  @Override
  public String getName() {
    return LispPsiImplUtil.getName(this);
  }

  @Override
  public ItemPresentation getPresentation() {
    return LispPsiImplUtil.getPresentation(this);
  }

}
