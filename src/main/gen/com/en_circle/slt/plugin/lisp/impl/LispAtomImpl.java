// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static com.en_circle.slt.plugin.lisp.psi.LispTypes.*;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.en_circle.slt.plugin.lisp.psi.*;
import com.en_circle.slt.plugin.lisp.psi.impl.LispPsiImplUtil;

public class LispAtomImpl extends ASTWrapperPsiElement implements LispAtom {

  public LispAtomImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull LispVisitor visitor) {
    visitor.visitAtom(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof LispVisitor) accept((LispVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public LispNumber getNumber() {
    return findChildByClass(LispNumber.class);
  }

  @Override
  @Nullable
  public LispString getString() {
    return findChildByClass(LispString.class);
  }

  @Override
  @Nullable
  public LispSymbol getSymbol() {
    return findChildByClass(LispSymbol.class);
  }

}
