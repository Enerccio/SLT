package com.en_circle.slt.plugin.actions;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.lisp.psi.LispList;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.editor.Document;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

public class EvalCurrentFormAction extends EvalFormActionBase {

    @Override
    public void update(@NotNull AnActionEvent event) {
        super.update(event);

        event.getPresentation().setText(SltBundle.message("slt.actions.eval.current"));
    }

    @Override
    protected PsiElement findList(PsiFile psiFile, int caretOffset, Document document) {
        PsiElement element = psiFile.findElementAt(caretOffset);
        if (element != null) {
            PsiElement parent = element;
            while (parent != null) {
                parent = parent.getParent();
                if (parent instanceof LispList) {
                    return parent;
                }
            }
        }
        return null;
    }

}
