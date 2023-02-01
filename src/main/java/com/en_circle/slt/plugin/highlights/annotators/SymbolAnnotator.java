package com.en_circle.slt.plugin.highlights.annotators;

import com.en_circle.slt.plugin.SymbolState;
import com.en_circle.slt.plugin.highlights.SltHighlighterColors;
import com.en_circle.slt.plugin.highlights.annotators.SymbolAnnotator.AnnotationResult;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.en_circle.slt.plugin.lisp.psi.LispVisitor;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.services.lisp.components.SltLispEnvironmentSymbolCache.BatchedSymbolRefreshAction;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.ExternalAnnotator;
import com.intellij.openapi.editor.Editor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class SymbolAnnotator extends ExternalAnnotator<BatchedSymbolRefreshAction, AnnotationResult> {

    @Override
    public @Nullable BatchedSymbolRefreshAction collectInformation(@NotNull PsiFile file, @NotNull Editor editor, boolean hasErrors) {
        BatchedSymbolRefreshAction holder = LispEnvironmentService.getInstance(file.getProject()).refreshSymbolsFromServer();

        file.accept(new LispVisitor() {

            @Override
            public void visitSymbol(@NotNull LispSymbol element) {
                String text = element.getText();
                String packageName = LispParserUtil.getPackage(element);
                holder.add(text, packageName);
            }

            @Override
            public void visitElement(@NotNull PsiElement element) {
                element.acceptChildren(this);
            }
        });

        return holder;
    }

    @Override
    public @Nullable AnnotationResult doAnnotate(BatchedSymbolRefreshAction collectedInfo) {
        if (collectedInfo != null) {
            boolean result = collectedInfo.getResult();
            return result ? AnnotationResult.OK : AnnotationResult.FAILED;
        }
        return AnnotationResult.FAILED;
    }

    @Override
    public void apply(@NotNull PsiFile file, AnnotationResult annotationResult, @NotNull AnnotationHolder holder) {
        if (annotationResult == AnnotationResult.FAILED)
            return;

        file.accept(new LispVisitor() {

            @Override
            public void visitSymbol(@NotNull LispSymbol element) {
                String text = element.getText();
                String packageName = LispParserUtil.getPackage(element);
                SymbolState state = LispEnvironmentService.getInstance(element.getProject()).refreshSymbolFromServer(packageName, text);
                setHighlight(element, text, holder, state);
            }

            @Override
            public void visitElement(@NotNull PsiElement element) {
                element.acceptChildren(this);
            }
        });
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

    public enum AnnotationResult {
        OK, FAILED
    }
}
