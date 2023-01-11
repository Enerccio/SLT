// This is a generated file. Not intended for manual editing.
package com.en_circle.slt.plugin.lisp.psi;

import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.Nullable;

public interface LispNumber extends PsiElement {

  @Nullable
  LispBinaryNumber getBinaryNumber();

  @Nullable
  LispHexNumber getHexNumber();

  @Nullable
  LispInteger getInteger();

  @Nullable
  LispOctalNumber getOctalNumber();

  @Nullable
  LispRadixNumber getRadixNumber();

  @Nullable
  LispRatio getRatio();

  @Nullable
  LispReal getReal();

}
