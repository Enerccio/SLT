package com.en_circle.slt.plugin.lisp.psi.impl;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.lisp.psi.LispFile;
import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFileFactory;

public class LispPsiImplUtil {

    public static String getSExpressionHead(PsiElement element) {
        ASTNode node = element.getNode();
        if (node != null && node.getElementType() == LispTypes.SYMBOL) {
            ASTNode sexpr = node.getTreeParent().getTreeParent();
            ASTNode prev = sexpr.getTreePrev();
            if (prev != null && prev.getElementType() == LispTypes.LPAREN) {
                // we are first call operation
                return element.getFirstChild().getText();
            }
        }
        return null;
    }

    public static String getName(LispSymbol element) {
        ASTNode identifier = element.getNode().findChildByType(LispTypes.IDENTIFIER_TOKEN);
        assert identifier != null;
        return identifier.getText();
    }

    private static LispSymbol createSymbol(Project project, String name) {
        LispFile file = createFile(project, name);
        return (LispSymbol) file.findChildByClass(LispSymbol.class);
    }

    private static LispFile createFile(Project project, String text) {
        String name = "dummy.cl";
        return (LispFile) PsiFileFactory.getInstance(project).createFileFromText(name, SltCommonLispFileType.INSTANCE,
                String.format("%s", text));
    }

    public static PsiElement setName(LispSymbol element, String newName) {
        ASTNode identifier = element.getNode().findChildByType(LispTypes.IDENTIFIER_TOKEN);
        if (identifier != null) {
            LispSymbol symbol = createSymbol(element.getProject(), newName);
            ASTNode newNameNode = symbol.getNode().findChildByType(LispTypes.IDENTIFIER_TOKEN);
            assert newNameNode != null;
            element.getNode().replaceChild(identifier, newNameNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(LispSymbol element) {
        ASTNode identifier = element.getNode().findChildByType(LispTypes.IDENTIFIER_TOKEN);
        if (identifier != null) {
            return identifier.getPsi();
        } else {
            return null;
        }
    }

}