package com.en_circle.slt.plugin.lisp.psi.impl;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.LispParserUtil.LispSexpressionInfo;
import com.en_circle.slt.plugin.lisp.LispParserUtil.SexpressionType;
import com.en_circle.slt.plugin.lisp.LispSymbolPresentation;
import com.en_circle.slt.plugin.lisp.psi.*;
import com.intellij.lang.ASTNode;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.NlsSafe;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.PsiReference;
import com.intellij.psi.PsiReferenceService.Hints;
import com.intellij.psi.impl.source.resolve.reference.ReferenceProvidersRegistry;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

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

    public static String getName(LispComment element) {
        return element.getText();
    }

    public static String getName(LispToplevel element) {
        return LispParserUtil.determineTopLevelType(element.getSexpr()).getShortForm();
    }

    public static String getName(LispList element) {
        if (element.getParent() instanceof LispDatum && element.getParent().getParent() instanceof LispSexpr) {
            LispSexpressionInfo type =  LispParserUtil.determineTopLevelType((LispSexpr) element.getParent().getParent());
            if (type.getType() != SexpressionType.EXPRESSION)
                return type.getShortForm();
            return null;
        }
        return null;
    }

    public static PsiElement setName(LispComment element, String newName) {
        ASTNode commentLineNode = element.getNode().findChildByType(LispTypes.LINE_COMMENT);
        if (commentLineNode != null) {
            LispComment comment = createComment(element.getProject(), newName);
            ASTNode newComment = comment.getNode().findChildByType(LispTypes.LINE_COMMENT);
            assert newComment != null;
            element.getNode().replaceChild(commentLineNode, newComment);
            return element;
        }
        ASTNode commentBlockNode = element.getNode().findChildByType(LispTypes.BLOCK_COMMENT);
        if (commentBlockNode != null) {
            LispComment comment = createComment(element.getProject(), newName);
            ASTNode newComment = comment.getNode().findChildByType(LispTypes.BLOCK_COMMENT);
            assert newComment != null;
            element.getNode().replaceChild(commentBlockNode, newComment);
            return element;
        }
        return element;
    }

    public static String getName(LispString element) {
        return element.getText();
    }

    public static PsiElement setName(LispString element, String newName) {
        ASTNode stringLineNode = element.getNode().findChildByType(LispTypes.STRING_TOKEN);
        if (stringLineNode != null) {
            LispComment comment = createComment(element.getProject(), newName);
            ASTNode newString = comment.getNode().findChildByType(LispTypes.STRING_TOKEN);
            assert newString != null;
            element.getNode().replaceChild(stringLineNode, newString);
            return element;
        }
        return element;
    }

    public static LispSymbol createSymbol(Project project, String name) {
        LispFile file = createFile(project, name);
        return PsiTreeUtil.findChildOfType(file, LispSymbol.class);
    }

    public static LispComment createComment(Project project, String name) {
        LispFile file = createFile(project, name);
        return PsiTreeUtil.findChildOfType(file, LispComment.class);
    }

    public static LispString createString(Project project, String name) {
        LispFile file = createFile(project, name);
        return PsiTreeUtil.findChildOfType(file, LispString.class);
    }

    public static LispFile createFile(Project project, String text) {
        String name = "dummy.cl";
        return (LispFile) PsiFileFactory.getInstance(project).createFileFromText(name, SltCommonLispFileType.INSTANCE,
                String.format("%s", text));
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

    public static ItemPresentation getPresentation(LispToplevel toplevel) {
        LispSexpressionInfo type = LispParserUtil.determineTopLevelType(toplevel.getSexpr());
        return new ItemPresentation() {
            @Override
            public @NlsSafe @Nullable String getPresentableText() {
                return type.getShortForm();
            }

            @Override
            public @Nullable Icon getIcon(boolean unused) {
                return type.getIcon();
            }
        };
    }

    public static ItemPresentation getPresentation(LispList list) {
        if (list.getParent() instanceof LispDatum && list.getParent().getParent() instanceof LispSexpr) {
            LispSexpressionInfo type = LispParserUtil.determineTopLevelType((LispSexpr) list.getParent().getParent());
            if (type.getType() != SexpressionType.EXPRESSION) {
                return new ItemPresentation() {
                    @Override
                    public @NlsSafe @Nullable String getPresentableText() {
                        return type.getShortForm();
                    }

                    @Override
                    public @Nullable Icon getIcon(boolean unused) {
                        return type.getIcon();
                    }
                };
            }
        }
        return null;
    }

}