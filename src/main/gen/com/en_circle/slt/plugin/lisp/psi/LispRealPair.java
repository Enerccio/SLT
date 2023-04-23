// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.psi;

import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public interface LispRealPair extends PsiElement {

  @NotNull
  LispLparenthesis getLparenthesis();

  @NotNull
  List<LispReal> getRealList();

  @NotNull
  LispRparenthesis getRparenthesis();

}
