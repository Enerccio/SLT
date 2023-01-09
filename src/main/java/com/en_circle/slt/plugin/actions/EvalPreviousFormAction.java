package com.en_circle.slt.plugin.actions;

import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.editor.Document;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;

public class EvalPreviousFormAction extends EvalFormActionBase {

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
