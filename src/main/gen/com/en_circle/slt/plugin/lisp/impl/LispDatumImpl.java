// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.impl;

import com.en_circle.slt.plugin.lisp.psi.*;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LispDatumImpl extends ASTWrapperPsiElement implements LispDatum {

  public LispDatumImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull LispVisitor visitor) {
    visitor.visitDatum(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof LispVisitor) accept((LispVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public LispArray getArray() {
    return findChildByClass(LispArray.class);
  }

  @Override
  @Nullable
  public LispCompoundSymbol getCompoundSymbol() {
    return findChildByClass(LispCompoundSymbol.class);
  }

  @Override
  @Nullable
  public LispEvaled getEvaled() {
    return findChildByClass(LispEvaled.class);
  }

  @Override
  @Nullable
  public LispList getList() {
    return findChildByClass(LispList.class);
  }

  @Override
  @Nullable
  public LispNumber getNumber() {
    return findChildByClass(LispNumber.class);
  }

  @Override
  @Nullable
  public LispPathname getPathname() {
    return findChildByClass(LispPathname.class);
  }

  @Override
  @Nullable
  public LispRealPair getRealPair() {
    return findChildByClass(LispRealPair.class);
  }

  @Override
  @Nullable
  public LispString getString() {
    return findChildByClass(LispString.class);
  }

  @Override
  @Nullable
  public LispStructure getStructure() {
    return findChildByClass(LispStructure.class);
  }

  @Override
  @Nullable
  public LispTested getTested() {
    return findChildByClass(LispTested.class);
  }

  @Override
  @Nullable
  public LispVector getVector() {
    return findChildByClass(LispVector.class);
  }

}
