package com.en_circle.slt.plugin.references;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.ResolveResult;
import org.jetbrains.annotations.Nullable;

public class SltLazyElementResolve implements ResolveResult {

    private final PsiFile file;
    private final int offset;

    public SltLazyElementResolve(PsiFile file, int offset) {
        this.file = file;
        this.offset = offset;
    }

    @Override
    public @Nullable PsiElement getElement() {
        return file.findElementAt(offset);
    }

    @Override
    public boolean isValidResult() {
        return false;
    }
}
