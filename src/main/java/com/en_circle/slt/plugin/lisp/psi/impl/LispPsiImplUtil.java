package com.en_circle.slt.plugin.lisp.psi.impl;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.lisp.LispSymbolPresentation;
import com.en_circle.slt.plugin.lisp.psi.LispFile;
import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.intellij.lang.ASTNode;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.PsiReference;
import com.intellij.psi.PsiReferenceService.Hints;
import com.intellij.psi.impl.source.resolve.reference.ReferenceProvidersRegistry;
import com.intellij.psi.util.PsiTreeUtil;

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

    public static LispSymbol getNotSExpressionHead(PsiElement element) {
        ASTNode node = element.getNode();
        if (node != null && node.getElementType() == LispTypes.SYMBOL) {
            String head = getSExpressionHead(element);
            if (head == null) {
                return (LispSymbol) element;
            }
        }
        return null;
    }

    public static String getName(LispSymbol element) {
        ASTNode identifier = element.getNode().findChildByType(LispTypes.SYMBOL_TOKEN);
        assert identifier != null;
        return identifier.getText();
    }

    private static LispSymbol createSymbol(Project project, String name) {
        LispFile file = createFile(project, name);
        return PsiTreeUtil.findChildOfType(file, LispSymbol.class);
    }

    private static LispFile createFile(Project project, String text) {
        String name = "dummy.cl";
        return (LispFile) PsiFileFactory.getInstance(project).createFileFromText(name, SltCommonLispFileType.INSTANCE,
                String.format("%s", text));
    }

    public static PsiElement setName(LispSymbol element, String newName) {
        ASTNode identifier = element.getNode().findChildByType(LispTypes.SYMBOL_TOKEN);
        if (identifier != null) {
            LispSymbol symbol = createSymbol(element.getProject(), newName);
            ASTNode newNameNode = symbol.getNode().findChildByType(LispTypes.SYMBOL_TOKEN);
            assert newNameNode != null;
            element.getNode().replaceChild(identifier, newNameNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(LispSymbol element) {
        ASTNode identifier = element.getNode().findChildByType(LispTypes.SYMBOL_TOKEN);
        if (identifier != null) {
            return identifier.getPsi();
        } else {
            return null;
        }
    }

    public static PsiReference[] getReferences(LispSymbol symbol) {
        return ReferenceProvidersRegistry.getReferencesFromProviders(symbol, Hints.NO_HINTS);
    }

    public static ItemPresentation getPresentation(LispSymbol symbol) {
        return new LispSymbolPresentation(symbol);
    }

}