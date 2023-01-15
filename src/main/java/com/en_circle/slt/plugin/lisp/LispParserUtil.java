package com.en_circle.slt.plugin.lisp;

import com.en_circle.slt.plugin.SltLispEnvironmentProvider;
import com.en_circle.slt.plugin.lisp.lisp.LispUtils;
import com.en_circle.slt.plugin.lisp.psi.*;
import com.intellij.lang.ASTNode;
import com.intellij.lang.FileASTNode;
import com.intellij.lang.parser.GeneratedParserUtilBase;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;

import java.util.List;

public class LispParserUtil extends GeneratedParserUtilBase {

    public static String getPackage(PsiFile psiFile, int offset) {
        FileASTNode node = psiFile.getNode();
        ASTNode locationNode = node.findLeafElementAt(offset);
        if (locationNode != null)
            return getPackage(locationNode.getPsi());
        else
            return SltLispEnvironmentProvider.getInstance().getGlobalPackage();
    }

    public static String getPackage(PsiElement element) {
        while (!(element instanceof LispToplevel)) {
            element = element.getParent();
            if (element == null) {
                return SltLispEnvironmentProvider.getInstance().getGlobalPackage();
            }
        }
        PsiElement previous = element.getPrevSibling();
        while (previous != null) {
            LispList ll;
            if (((ll = isLispList(previous)) != null)) {
                String packageName = getPackageName(ll);
                if (packageName != null) {
                    return packageName;
                }
            }
            previous = previous.getPrevSibling();
        }

        return SltLispEnvironmentProvider.getInstance().getGlobalPackage();
    }

    private static LispList isLispList(PsiElement form) {
        if (form instanceof LispList) {
            return (LispList) form;
        }
        if (form instanceof LispToplevel) {
            LispToplevel toplevel = (LispToplevel) form;
            LispSexpr sexpr = toplevel.getSexpr();
            if (sexpr.getDatum() != null && sexpr.getDatum().getList() != null) {
                return sexpr.getDatum().getList();
            }
        }
        return null;
    }

    private static String getPackageName(LispList form) {
        List<LispSexpr> sexpressions = form.getSexprList();
        if (sexpressions.size() == 2) {
            LispSexpr seSymbol = sexpressions.get(0);
            String symbol = getAsSymbol(seSymbol);
            if (symbol != null) {
                if ("in-package".equalsIgnoreCase(symbol)) {
                    LispSexpr packageName = sexpressions.get(1);
                    if (packageName.getDatum() != null) {
                        LispDatum datum = packageName.getDatum();
                        if (datum.getCompoundSymbol() != null) {
                            LispCompoundSymbol symbolElement = datum.getCompoundSymbol();
                            String text = symbolElement.getSymbol().getText();
                            if (text.startsWith(":")) {
                                return text.substring(1);
                            } else if (!text.contains(":")) {
                                return text;
                            }
                        } else if (datum.getString() != null) {
                            String text = datum.getString().getText();
                            return LispUtils.unescape(text.substring(1, text.length() - 1));
                        }
                    }
                }
            }
        }
        return null;
    }

    private static String getAsSymbol(LispSexpr seSymbol) {
        LispDatum datum = seSymbol.getDatum();
        if (datum != null) {
            LispCompoundSymbol symbol = datum.getCompoundSymbol();
            if (symbol != null) {
                return symbol.getSymbol().getName();
            }
        }
        return null;
    }

    public static LispList getIfHead(PsiElement element) {
        PsiElement original = element;
        while (!(element instanceof LispList)) {
            element = element.getParent();
            if (element == null) {
                return null;
            }
        }
        LispList list = (LispList) element;
        LispSexpr firstElement = list.getSexprList().get(0);
        if (firstElement.getDatum() != null && firstElement.getDatum().getCompoundSymbol() != null &&
                firstElement.getDatum().getCompoundSymbol().getSymbol().equals(original)) {
            return list;
        }
        return null;
    }
}
