package com.en_circle.slt.plugin.highlights.annotators;

import com.en_circle.slt.plugin.highlights.SltHighlighterColors;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.intellij.lang.ASTNode;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;

public class StaticHighlightAnnotator implements Annotator {

    @Override
    public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
        ASTNode node = element.getNode();
        if (node != null && LispTypes.REAL_PAIR.equals(node.getElementType())) {
            SltHighlighterColors.setHighlighting(element, holder,
                    SltHighlighterColors.NUMBER);
        }
    }

}
