// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.impl;

import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.en_circle.slt.plugin.lisp.psi.LispVisitor;
import com.en_circle.slt.plugin.lisp.psi.impl.LispNamedElementImpl;
import com.en_circle.slt.plugin.lisp.psi.impl.LispPsiImplUtil;
import com.intellij.lang.ASTNode;
import com.intellij.navigation.ItemPresentation;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiReference;
import org.jetbrains.annotations.NotNull;

public class LispSymbolImpl extends LispNamedElementImpl implements LispSymbol {

  public LispSymbolImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull LispVisitor visitor) {
    visitor.visitSymbol(this);
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

  @Override
  public PsiElement getNameIdentifier() {
    return LispPsiImplUtil.getNameIdentifier(this);
  }

  @Override
  public PsiReference[] getReferences() {
    return LispPsiImplUtil.getReferences(this);
  }

  @Override
  public ItemPresentation getPresentation() {
    return LispPsiImplUtil.getPresentation(this);
  }

}
