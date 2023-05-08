package com.en_circle.slt.plugin.actions;

import com.en_circle.slt.plugin.lisp.psi.LispToplevel;
import com.intellij.openapi.editor.Document;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;

public class EvalTopLevelAction extends EvalFormActionBase {

    @Override
    protected PsiElement findList(PsiFile psiFile, int caretOffset, Document document) {
        while (caretOffset > 0) {
            PsiElement element = psiFile.findElementAt(caretOffset);
            if (element != null) {
                LispToplevel toplevel = PsiTreeUtil.getTopmostParentOfType(element, LispToplevel.class);
                if (toplevel != null) {
                    return toplevel;
                }
                caretOffset--;
            }
        }
        return null;
    }

}
