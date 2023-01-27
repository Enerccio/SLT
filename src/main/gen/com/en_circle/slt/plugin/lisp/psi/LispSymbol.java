// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.psi;

import com.intellij.navigation.ItemPresentation;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;

public interface LispSymbol extends LispNamedElement {

  String getName();

  PsiElement setName(String newName);

  PsiElement getNameIdentifier();

  PsiReference[] getReferences();

  ItemPresentation getPresentation();

}
