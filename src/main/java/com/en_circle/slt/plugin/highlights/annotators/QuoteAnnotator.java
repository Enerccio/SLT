package com.en_circle.slt.plugin.highlights.annotators;

import com.en_circle.slt.plugin.highlights.SltHighlighterColors;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.LispParserUtil.QuoteState;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;

public class QuoteAnnotator implements Annotator {

    @Override
    public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
        if (element.getFirstChild() == null) {
            QuoteState state = LispParserUtil.getQuoteState(element);
            if (QuoteState.isQuoted(state)) {
                SltHighlighterColors.setHighlighting(element, holder, SltHighlighterColors.QUOTED);
            }
        }
    }
}
