// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface LispSexpr extends PsiElement {

  @Nullable
  LispAtom getAtom();

  @Nullable
  LispList getList();

  @Nullable
  LispPair getPair();

  @NotNull
  List<LispSugar> getSugarList();

}
