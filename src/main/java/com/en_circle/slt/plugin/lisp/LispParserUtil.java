package com.en_circle.slt.plugin.lisp;

import com.en_circle.slt.plugin.lisp.lisp.LispUtils;
import com.en_circle.slt.plugin.lisp.psi.*;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.intellij.lang.ASTNode;
import com.intellij.lang.FileASTNode;
import com.intellij.lang.parser.GeneratedParserUtilBase;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;

import java.util.List;
import java.util.function.Function;

public class LispParserUtil extends GeneratedParserUtilBase {

    public static final Function<Project, String> NULL_RETURN = (project) -> null;
    public static final Function<Project, String> GLOBAL_PACKAGE_RETURN = (project) ->
            LispEnvironmentService.getInstance(project).getGlobalPackage();

    public static String getPackage(PsiFile psiFile, int offset) {
        return getPackage(psiFile, offset, GLOBAL_PACKAGE_RETURN);
    }

    public static String getPackage(PsiFile psiFile, int offset, Function<Project, String> notFoundSupplier) {
        FileASTNode node = psiFile.getNode();
        ASTNode locationNode = node.findLeafElementAt(offset);
        if (locationNode == null && offset == psiFile.getTextLength()) {
            // can be the end of file, check character before
            locationNode = node.findLeafElementAt(offset - 1);
        }

        if (locationNode != null)
            return getPackage(locationNode.getPsi(), notFoundSupplier);
        else
            return notFoundSupplier.apply(psiFile.getProject());
    }

    public static String getPackage(PsiElement element) {
        return getPackage(element,  GLOBAL_PACKAGE_RETURN);
    }

    public static String getPackage(PsiElement element, Function<Project, String> notFoundSupplier) {
        Project project = element.getProject();
        while (element instanceof PsiWhiteSpace) {
            element = element.getPrevSibling();
            if (element == null) {
                return notFoundSupplier.apply(project);
            }
        }
        while (!(element instanceof LispToplevel)) {
            element = element.getParent();
            if (element == null) {
                return notFoundSupplier.apply(project);
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

        return notFoundSupplier.apply(project);
    }

    private static LispList isLispList(PsiElement form) {
        if (form instanceof LispList) {
            return (LispList) form;
        }
        if (form instanceof LispToplevel toplevel) {
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
        while (!(element instanceof LispList list)) {
            element = element.getParent();
            if (element == null) {
                return null;
            }
        }
        LispSexpr firstElement = list.getSexprList().get(0);
        if (firstElement.getDatum() != null && firstElement.getDatum().getCompoundSymbol() != null &&
                firstElement.getDatum().getCompoundSymbol().getSymbol().equals(original)) {
            return list;
        }
        return null;
    }
}
