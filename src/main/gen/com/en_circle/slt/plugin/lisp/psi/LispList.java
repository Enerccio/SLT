// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.psi;

import com.intellij.navigation.ItemPresentation;
import com.intellij.psi.NavigatablePsiElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public interface LispList extends NavigatablePsiElement {

  @NotNull
  LispLparenthesis getLparenthesis();

  @Nullable
  LispRparenthesis getRparenthesis();

  @NotNull
  List<LispSexpr> getSexprList();

  String getName();

  ItemPresentation getPresentation();

}
