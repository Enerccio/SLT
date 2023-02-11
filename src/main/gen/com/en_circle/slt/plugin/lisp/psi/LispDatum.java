// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.psi;

import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.Nullable;

public interface LispDatum extends PsiElement {

  @Nullable
  LispArray getArray();

  @Nullable
  LispCompoundSymbol getCompoundSymbol();

  @Nullable
  LispEvaled getEvaled();

  @Nullable
  LispList getList();

  @Nullable
  LispNumber getNumber();

  @Nullable
  LispPathname getPathname();

  @Nullable
  LispRealPair getRealPair();

  @Nullable
  LispString getString();

  @Nullable
  LispStructure getStructure();

  @Nullable
  LispTested getTested();

  @Nullable
  LispVector getVector();

}
