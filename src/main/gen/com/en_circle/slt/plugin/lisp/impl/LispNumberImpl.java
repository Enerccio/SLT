// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.impl;

import com.en_circle.slt.plugin.lisp.psi.*;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LispNumberImpl extends ASTWrapperPsiElement implements LispNumber {

  public LispNumberImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull LispVisitor visitor) {
    visitor.visitNumber(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof LispVisitor) accept((LispVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public LispBinaryNumber getBinaryNumber() {
    return findChildByClass(LispBinaryNumber.class);
  }

  @Override
  @Nullable
  public LispHexNumber getHexNumber() {
    return findChildByClass(LispHexNumber.class);
  }

  @Override
  @Nullable
  public LispInteger getInteger() {
    return findChildByClass(LispInteger.class);
  }

  @Override
  @Nullable
  public LispOctalNumber getOctalNumber() {
    return findChildByClass(LispOctalNumber.class);
  }

  @Override
  @Nullable
  public LispRadixNumber getRadixNumber() {
    return findChildByClass(LispRadixNumber.class);
  }

  @Override
  @Nullable
  public LispRatio getRatio() {
    return findChildByClass(LispRatio.class);
  }

  @Override
  @Nullable
  public LispReal getReal() {
    return findChildByClass(LispReal.class);
  }

}
