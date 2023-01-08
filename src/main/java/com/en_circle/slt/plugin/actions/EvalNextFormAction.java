package com.en_circle.slt.plugin.actions;

import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.editor.Document;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;

public class EvalNextFormAction extends EvalFormActionBase {

    @Override
    protected PsiElement findList(PsiFile psiFile, int caretOffset, Document document) {
        PsiElement element = findElementAtAfter(psiFile, caretOffset, document.getTextLength());
        if (element != null) {
            return element.getParent();
        }
        return null;
    }

    private PsiElement findElementAtAfter(PsiFile psiFile, int offset, int maxSize) {
        for (int ix=offset; ix<maxSize; ix++) {
            PsiElement element = psiFile.findElementAt(ix);
            if (element != null) {
                if (!(element instanceof PsiWhiteSpace)) {
                    ASTNode node = element.getNode();
                    if (node.getElementType() == LispTypes.LPARAM) {
                        return element;
                    }
                }
            }
        }
        return null;
    }

}
