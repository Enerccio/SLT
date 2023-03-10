// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.psi;

import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public interface LispSexpr extends PsiElement {

  @Nullable
  LispComment getComment();

  @Nullable
  LispDatum getDatum();

  @NotNull
  List<LispEnhancement> getEnhancementList();

}
