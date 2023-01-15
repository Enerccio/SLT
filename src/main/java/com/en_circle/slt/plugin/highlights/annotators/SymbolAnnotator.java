package com.en_circle.slt.plugin.highlights.annotators;

import com.en_circle.slt.plugin.SltLispEnvironmentProvider;
import com.en_circle.slt.plugin.SymbolState;
import com.en_circle.slt.plugin.highlights.CommonLispHighlighterColors;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;

public class SymbolAnnotator implements Annotator {

    @Override
    public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
        if (element instanceof LispSymbol) {
            String text = element.getText();
            String packageName = LispParserUtil.getPackage(element);
            SymbolState state = SltLispEnvironmentProvider.getInstance().refreshSymbolFromServer(packageName, text, element);
            setHighlight(element, text, holder, state);
        }
    }

    private void setHighlight(PsiElement element, String name, AnnotationHolder holder, SymbolState state) {
        if (name.startsWith("&"))
            CommonLispHighlighterColors.setHighlighting(element, holder,
                    CommonLispHighlighterColors.DEFUN_FORM);

        switch (state.binding) {
            case NONE:
                break;
            case CLASS:
                CommonLispHighlighterColors.setHighlighting(element, holder,
                        CommonLispHighlighterColors.CLASS);
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
            case CONSTANT:
                CommonLispHighlighterColors.setHighlighting(element, holder,
                        CommonLispHighlighterColors.CONSTANT);
                break;
            case SPECIAL_VARIABLE:
                CommonLispHighlighterColors.setHighlighting(element, holder,
                        CommonLispHighlighterColors.SPECIAL_VARIABLE);
                break;
            case KEYWORD:
                CommonLispHighlighterColors.setHighlighting(element, holder,
                        CommonLispHighlighterColors.KEYWORD);
                break;
        }
    }
}
