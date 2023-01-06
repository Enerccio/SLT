package com.en_circle.slt.plugin.lisp.psi;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.SltCommonLispLanguage;
import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import org.jetbrains.annotations.NotNull;

public class LispFile extends PsiFileBase {

  public LispFile(@NotNull FileViewProvider viewProvider) {
    super(viewProvider, SltCommonLispLanguage.INSTANCE);
  }

  @NotNull
  @Override
  public FileType getFileType() {
    return SltCommonLispFileType.INSTANCE;
  }

  @Override
  public String toString() {
    return SltCommonLispLanguage.ID;
  }

}