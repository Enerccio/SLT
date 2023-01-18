package com.en_circle.slt.plugin.highlights.annotators;

import com.en_circle.slt.plugin.SltLispEnvironmentProvider;
import com.en_circle.slt.plugin.SymbolState;
import com.en_circle.slt.plugin.highlights.SltHighlighterColors;
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
            SltHighlighterColors.setHighlighting(element, holder,
                    SltHighlighterColors.DEFUN_FORM);

        switch (state.binding) {
            case NONE:
                break;
            case CLASS:
                SltHighlighterColors.setHighlighting(element, holder,
                        SltHighlighterColors.CLASS);
                break;
            case METHOD:
                SltHighlighterColors.setHighlighting(element, holder,
                        SltHighlighterColors.METHOD);
                break;
            case FUNCTION:
                SltHighlighterColors.setHighlighting(element, holder,
                        SltHighlighterColors.FUNCTION);
                break;
            case MACRO:
                SltHighlighterColors.setHighlighting(element, holder,
                        SltHighlighterColors.MACRO);
                break;
            case SPECIAL_FORM:
                SltHighlighterColors.setHighlighting(element, holder,
                        SltHighlighterColors.SPECIAL_FORM);
                break;
            case CONSTANT:
                SltHighlighterColors.setHighlighting(element, holder,
                        SltHighlighterColors.CONSTANT);
                break;
            case SPECIAL_VARIABLE:
                SltHighlighterColors.setHighlighting(element, holder,
                        SltHighlighterColors.SPECIAL_VARIABLE);
                break;
            case KEYWORD:
                SltHighlighterColors.setHighlighting(element, holder,
                        SltHighlighterColors.KEYWORD);
                break;
        }
    }
}
