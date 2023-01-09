package com.en_circle.slt.plugin.highlights.annotators;

import com.en_circle.slt.plugin.SltSBCL;
import com.en_circle.slt.plugin.SymbolState;
import com.en_circle.slt.plugin.highlights.CommonLispHighlighterColors;
import com.en_circle.slt.plugin.lisp.psi.impl.LispPsiImplUtil;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;

public class CallFormAnnotator implements Annotator {

    @Override
    public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
        String text = LispPsiImplUtil.getSExpressionHead(element);
        if (text != null) {
            SymbolState state = SltSBCL.getInstance().refreshSymbolFromServer(SltSBCL.getInstance().getGlobalPackage(), text);
            setHighlight(element, holder, state);
        }
    }

    private void setHighlight(PsiElement element, AnnotationHolder holder, SymbolState state) {
        switch (state.binding) {
            case NONE:
                break;
            case FUNCTION:
                CommonLispHighlighterColors.setHighlighting(element, holder,
                        CommonLispHighlighterColors.FUNCTION);
                break;
            case MACRO:
                CommonLispHighlighterColors.setHighlighting(element, holder,
                        CommonLispHighlighterColors.MACRO);
                break;
            case SPECIAL_FORM:
                CommonLispHighlighterColors.setHighlighting(element, holder,
                        CommonLispHighlighterColors.SPECIAL_FORM);
                break;
        }
    }
}
