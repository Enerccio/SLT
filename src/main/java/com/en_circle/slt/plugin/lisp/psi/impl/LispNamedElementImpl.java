package com.en_circle.slt.plugin.lisp.psi.impl;

import com.en_circle.slt.plugin.lisp.psi.LispNamedElement;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import org.jetbrains.annotations.NotNull;

public abstract class LispNamedElementImpl extends ASTWrapperPsiElement implements LispNamedElement {

  public LispNamedElementImpl(@NotNull ASTNode node) {
    super(node);
  }

}