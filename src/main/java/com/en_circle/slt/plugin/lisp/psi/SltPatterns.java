package com.en_circle.slt.plugin.lisp.psi;

import com.intellij.patterns.ElementPattern;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.psi.PsiElement;

public class SltPatterns {

    public static ElementPattern<? extends PsiElement> getLParen() {
        return PlatformPatterns.psiElement(LispTypes.LPAREN);
    }

    public static ElementPattern<? extends PsiElement> getHeadPattern() {
        return PlatformPatterns.psiElement(LispTypes.SYMBOL_TOKEN)
                                .afterLeaf("(")
                                .andNot(PlatformPatterns.psiElement().withParent(LispString.class));
    }

}
