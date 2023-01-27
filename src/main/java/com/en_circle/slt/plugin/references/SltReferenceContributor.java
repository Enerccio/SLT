package com.en_circle.slt.plugin.references;

import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.psi.*;
import com.intellij.util.ProcessingContext;
import org.jetbrains.annotations.NotNull;

public class SltReferenceContributor extends PsiReferenceContributor {

    @Override
    public void registerReferenceProviders(@NotNull PsiReferenceRegistrar registrar) {
        registrar.registerReferenceProvider(PlatformPatterns.psiElement(LispSymbol.class), new PsiReferenceProvider() {
            @Override
            public PsiReference @NotNull [] getReferencesByElement(@NotNull PsiElement element, @NotNull ProcessingContext context) {
                if (element instanceof LispSymbol symbol) {
                    return new PsiReference[] { new SltReference(symbol) };
                }
                return PsiReference.EMPTY_ARRAY;
            }
        });
    }
}
