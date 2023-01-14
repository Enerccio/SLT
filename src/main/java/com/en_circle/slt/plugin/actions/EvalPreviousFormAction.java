package com.en_circle.slt.plugin.actions;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.editor.Document;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import org.jetbrains.annotations.NotNull;

public class EvalPreviousFormAction extends EvalFormActionBase {

    @Override
    public void update(@NotNull AnActionEvent event) {
        super.update(event);

        event.getPresentation().setText(SltBundle.message("slt.actions.eval.previous"));
    }

    private PsiElement findElementAt(PsiFile psiFile, int offset) {
        for (int ix=offset-1; ix>=0; ix--) {
            PsiElement element = psiFile.findElementAt(ix);
            if (element != null) {
                if (!(element instanceof PsiWhiteSpace)) {
                    ASTNode node = element.getNode();
                    if (node.getElementType() == LispTypes.RPAREN) {
                        return element;
                    }
                }
            }
        }
        return null;
    }

    @Override
    protected PsiElement findList(PsiFile psiFile, int caretOffset, Document document) {
        PsiElement element = findElementAt(psiFile, caretOffset);
        if (element != null) {
            return element.getParent();
        }
        return null;
    }
}
