package com.en_circle.slt.plugin.highlights;

import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.intellij.lang.BracePair;
import com.intellij.lang.PairedBraceMatcher;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class SltBraceMatcher implements PairedBraceMatcher {

    private static final BracePair[] PAIRS = new BracePair[] {
        new BracePair(LispTypes.LPAREN, LispTypes.RPAREN, true),
    };

    @Override
    public BracePair @NotNull [] getPairs() {
        return PAIRS;
    }

    @Override
    public boolean isPairedBracesAllowedBeforeType(@NotNull IElementType lbraceType, @Nullable IElementType contextType) {
        return false;
    }

    @Override
    public int getCodeConstructStart(PsiFile file, int openingBraceOffset) {
        return openingBraceOffset;
    }
}
