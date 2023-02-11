// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.impl;

import com.en_circle.slt.plugin.lisp.psi.LispString;
import com.en_circle.slt.plugin.lisp.psi.LispVisitor;
import com.en_circle.slt.plugin.lisp.psi.impl.LispPsiImplUtil;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import org.jetbrains.annotations.NotNull;

public class LispStringImpl extends ASTWrapperPsiElement implements LispString {

  public LispStringImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull LispVisitor visitor) {
    visitor.visitString(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof LispVisitor) accept((LispVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  public String getName() {
    return LispPsiImplUtil.getName(this);
  }

  @Override
  public PsiElement setName(String newName) {
    return LispPsiImplUtil.setName(this, newName);
  }

}
