package com.en_circle.slt.plugin.highlights.annotators;

import com.en_circle.slt.plugin.highlights.SltHighlighterColors;
import com.en_circle.slt.plugin.highlights.annotators.RainbowAnnotator.RainbowInfo;
import com.en_circle.slt.plugin.indentation.SltIndentationSettings;
import com.en_circle.slt.plugin.lisp.psi.LispLparenthesis;
import com.en_circle.slt.plugin.lisp.psi.LispRparenthesis;
import com.en_circle.slt.plugin.lisp.psi.LispVisitor;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.ExternalAnnotator;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.ui.JBColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.awt.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class RainbowAnnotator extends ExternalAnnotator<RainbowInfo, RainbowInfo> {

    @Override
    public @Nullable RainbowInfo collectInformation(@NotNull PsiFile file, @NotNull Editor editor, boolean hasErrors) {
        RainbowInfo rainbowInfo = new RainbowInfo();
        if (SltIndentationSettings.getInstance(file.getProject()).rainbow) {
            rainbowInfo.enabled = true;
            rainbowInfo.load(file);
        }
        return rainbowInfo;
    }

    @Override
    public @Nullable RainbowInfo doAnnotate(RainbowInfo collectedInfo) {
        collectedInfo.calculate();
        return collectedInfo;
    }

    @Override
    public void apply(@NotNull PsiFile file, RainbowInfo annotationResult, @NotNull AnnotationHolder holder) {
        if (annotationResult.enabled) {
            file.accept(new LispVisitor() {

                @Override
                public void visitLparenthesis(@NotNull LispLparenthesis o) {
                    if (annotationResult.opens.containsKey(o.getNode().getStartOffset())) {
                        SltHighlighterColors.setHighlightingTextAttribute(o, holder,
                                annotationResult.opens.get(o.getNode().getStartOffset()).color);
                    }
                }

                @Override
                public void visitRparenthesis(@NotNull LispRparenthesis o) {
                    if (annotationResult.closes.containsKey(o.getNode().getStartOffset())) {
                        SltHighlighterColors.setHighlightingTextAttribute(o, holder,
                                annotationResult.closes.get(o.getNode().getStartOffset()).color);
                    }
                }

                @Override
                public void visitElement(@NotNull PsiElement element) {
                    element.acceptChildren(this);
                }
            });
        }
    }

    public static class RainbowInfo {

        private boolean enabled = false;
        private int maxStackSize = 0;

        private final Map<Integer, ParenthesisPair> opens = new HashMap<>();
        private final Map<Integer, ParenthesisPair> closes = new HashMap<>();

        public void calculate() {
            List<JBColor> colors = new ArrayList<>();

            float step = 1.0f / (maxStackSize+1);
            float c = 0.0f;
            for (int i=0; i<maxStackSize+1; i++) {
                Color colorNormal = Color.getHSBColor(c, 0.5f, 0.5f);
                Color colorInverted = Color.getHSBColor(c + 0.5f, 0.5f, 0.5f);
                JBColor color = new JBColor(colorNormal, colorInverted);
                colors.add(color);
                c += step;
            }

            List<TextAttributes> textAttributes = new ArrayList<>();
            for (JBColor color : colors) {
                TextAttributes textAttribute = new TextAttributes();
                textAttribute.setForegroundColor(color);
                textAttributes.add(textAttribute);
            }

            for (ParenthesisPair pp : opens.values()) {
                pp.color = textAttributes.get(pp.positionInStack);
            }
        }

        public void load(PsiFile file) {
            List<ParenthesisPair> allInOrder = new ArrayList<>();
            List<ParenthesisPair> stack = new ArrayList<>();

            file.accept(new LispVisitor() {

                @Override
                public void visitLparenthesis(@NotNull LispLparenthesis o) {
                    ParenthesisPair parenthesisPair = new ParenthesisPair();
                    parenthesisPair.openParenPosition = o.getNode().getStartOffset();
                    parenthesisPair.positionInStack = stack.size();
                    maxStackSize = Math.max(maxStackSize, parenthesisPair.positionInStack);

                    allInOrder.add(parenthesisPair);
                    stack.add(parenthesisPair);
                }

                @Override
                public void visitRparenthesis(@NotNull LispRparenthesis o) {
                    if (!stack.isEmpty()) {
                        stack.get(stack.size() - 1).closeParenPosition = o.getNode().getStartOffset();
                        stack.remove(stack.size() - 1);
                    }
                }

                @Override
                public void visitElement(@NotNull PsiElement element) {
                    element.acceptChildren(this);
                }
            });

            for (ParenthesisPair pp : allInOrder) {
                if (pp.openParenPosition >= 0 && pp.closeParenPosition >= 0) {
                    opens.put(pp.openParenPosition, pp);
                    closes.put(pp.closeParenPosition, pp);
                }
            }
        }

    }

    private static class ParenthesisPair {

        private int openParenPosition = -1;
        private int closeParenPosition = -1;
        private int positionInStack;
        private TextAttributes color;

    }

}
