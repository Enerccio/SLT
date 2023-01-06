// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface LispAtom extends PsiElement {

  @Nullable
  LispNumber getNumber();

  @Nullable
  LispString getString();

  @Nullable
  LispSymbol getSymbol();

}
