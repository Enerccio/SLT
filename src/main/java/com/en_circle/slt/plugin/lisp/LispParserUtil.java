package com.en_circle.slt.plugin.lisp;

import com.en_circle.slt.plugin.SltIconProvider;
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
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.Stack;

import javax.swing.*;
import java.util.List;
import java.util.function.Function;
import java.util.regex.Pattern;

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
        return getPackage(element, GLOBAL_PACKAGE_RETURN);
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

    public static QuoteState getQuoteState(PsiElement o) {
        if (o instanceof LispList list) {
            return getQuoteState(list);
        } else {
            QuoteState quoteState = QuoteState.NO_STATE;

            LispSexpr parent = PsiTreeUtil.getParentOfType(o, LispSexpr.class);
            if (parent == null) {
                return quoteState;
            }

            LispList plist = PsiTreeUtil.getParentOfType(o, LispList.class);

            if (plist == null) {
                return quoteState;
            }

            quoteState = getQuoteState(plist, parent);
//            if (o instanceof PsiWhiteSpace) {
//                return quoteState;
//            }
            return getQuoteState(parent, quoteState);
        }
    }

    public static QuoteState getQuoteState(LispList o, LispSexpr self) {
        QuoteState quoteState = QuoteState.NO_STATE;
        LispSexpr sexpr = PsiTreeUtil.getParentOfType(o, LispSexpr.class);
        if (sexpr == null) {
            return quoteState;
        }
        PsiElement parent = sexpr.getParent();
        if (parent == null) {
            return quoteState;
        } else {
            if (parent instanceof LispList list) {
                QuoteState parentState = getQuoteState(list, self);
                quoteState = combineQuoteStates(quoteState, parentState);
            }
        }
        if (sexpr == self) {
            return quoteState;
        }
        return getQuoteState(sexpr, quoteState);
    }

    private static QuoteState getQuoteState(LispSexpr self, QuoteState quoteState) {
        LispSexpr superExpr = PsiTreeUtil.getParentOfType(self, LispSexpr.class);
        if (superExpr != null) {
            LispDatum datum = superExpr.getDatum();
            if (datum != null) {
                LispList plist = datum.getList();
                if (plist != null) {
                    LispSexpr first = plist.getSexprList().get(0);
                    if (first != self) {
                        if (first.getDatum() != null && first.getDatum().getCompoundSymbol() != null) {
                            LispSymbol symbol = first.getDatum().getCompoundSymbol().getSymbol();
                            QuoteState symbolState = getQuoteStateForSymbol(symbol.getName());
                            if (symbolState != QuoteState.NO_STATE) {
                                boolean checked = false;
                                for (int i=1; i<plist.getSexprList().size(); i++) {
                                    LispSexpr other = plist.getSexprList().get(i);
                                    if (other.getDatum() != null) {
                                        if (other == self) {
                                            if (!checked) {
                                                checked = true;
                                            }
                                        }
                                    }
                                }
                                if (checked) {
                                    quoteState = combineQuoteStates(symbolState, quoteState);
                                } else {
                                    quoteState = QuoteState.ERROR_STATE;
                                }
                            }
                        }
                    }
                }
            }
        }

        for (LispEnhancement enhancement : self.getEnhancementList()) {
            String text = enhancement.getText();
            if (text.equals("`")) {
                quoteState = combineQuoteStates(QuoteState.BACKQUOTE, quoteState);
            } else if (text.equals(",")) {
                quoteState = combineQuoteStates(QuoteState.UNQUOTE, quoteState);
            } else if (text.equals(",@")) {
                quoteState = combineQuoteStates(QuoteState.UNQUOTE_SPLICE, quoteState);
            } else if (text.equals("'")) {
                quoteState = combineQuoteStates(QuoteState.QUOTE, quoteState);
            }
        }
        return quoteState;
    }

    private static QuoteState getQuoteStateForSymbol(String name) {
        if (name == null)
            return QuoteState.NO_STATE;

        if (name.equalsIgnoreCase("quote"))
            return QuoteState.QUOTE;
        if (name.equalsIgnoreCase("unquote"))
            return QuoteState.UNQUOTE;
        if (name.equalsIgnoreCase("unquote-splice"))
            return QuoteState.UNQUOTE_SPLICE;
        if (name.equalsIgnoreCase("backquote"))
            return QuoteState.BACKQUOTE;

        return QuoteState.NO_STATE;
    }

    private static QuoteState combineQuoteStates(QuoteState quoteState, QuoteState parentState) {
        if (parentState == QuoteState.ERROR_STATE)
            return QuoteState.ERROR_STATE;
        if (parentState == QuoteState.QUOTE)
            return QuoteState.QUOTE;
        if (parentState == QuoteState.BACKQUOTE) {
            if (quoteState == QuoteState.NO_STATE)
                return QuoteState.BACKQUOTE;
            if (quoteState == QuoteState.UNQUOTE || quoteState == QuoteState.UNQUOTE_SPLICE)
                return QuoteState.NO_STATE;
        }
        if (parentState == QuoteState.NO_STATE) {
            if (quoteState == QuoteState.UNQUOTE || quoteState == QuoteState.UNQUOTE_SPLICE)
                return QuoteState.ERROR_STATE;
            if (quoteState == QuoteState.BACKQUOTE)
                return QuoteState.BACKQUOTE;
            if (quoteState == QuoteState.QUOTE)
                return QuoteState.QUOTE;
        }
        if (quoteState == QuoteState.UNQUOTE || quoteState == QuoteState.UNQUOTE_SPLICE)
            return QuoteState.NO_STATE;
        if (quoteState == QuoteState.NO_STATE && parentState == QuoteState.NO_STATE)
            return QuoteState.NO_STATE;

        return QuoteState.ERROR_STATE;
    }

    public static QuoteState getQuoteStateUnfinished(PsiElement o) {
        assert (o.getParent() instanceof PsiFile);

        Stack<PsiElement> elementStack = new Stack<>();
        PsiElement e = o;
        int parenthesis = 0;

        while (e.getPrevSibling() != null && !(e.getPrevSibling() instanceof LispToplevel)) {
            ASTNode node = e.getNode();
            if (parenthesis == 0) {
                if (node.getElementType() == LispTypes.RPAREN) {
                    ++parenthesis;
                } else {
                    elementStack.push(e);
                }
            } else {
                if (node.getElementType() == LispTypes.LPAREN) {
                    --parenthesis;
                }
            }

            e = e.getPrevSibling();
        }

        QuoteState quoteState = QuoteState.NO_STATE;
        boolean firstElementCheck = false;
        for (PsiElement element : elementStack) {
            ASTNode node = element.getNode();
            if (node.getElementType() == LispTypes.QUOTE) {
                quoteState = combineQuoteStates(QuoteState.QUOTE, quoteState);
            } else if (node.getElementType() == LispTypes.UNQUOTE) {
                quoteState = combineQuoteStates(QuoteState.UNQUOTE, quoteState);
            } else if (node.getElementType() == LispTypes.UNQUOTE_SPLICE) {
                quoteState = combineQuoteStates(QuoteState.UNQUOTE_SPLICE, quoteState);
            } else if (node.getElementType() == LispTypes.BACKQUOTE) {
                quoteState = combineQuoteStates(QuoteState.BACKQUOTE, quoteState);
            } else {
                if (node.getElementType() == LispTypes.LPAREN) {
                    firstElementCheck = true;
                }

                if (node.getElementType() == LispTypes.SYMBOL_TOKEN && firstElementCheck && node != o.getNode()) {
                    firstElementCheck = false;
                    QuoteState state = getQuoteStateForSymbol(node.getText());
                    quoteState = combineQuoteStates(state, quoteState);
                }
            }
        }
        return quoteState;
    }

    public static LispSexpressionInfo determineTopLevelType(LispSexpr sexpr) {
        LispSexpressionInfo info = new LispSexpressionInfo();
        info.type = SexpressionType.EXPRESSION;
        info.shortForm = "...";

        if (sexpr.getDatum() != null && sexpr.getDatum().getList() != null) {
            LispList list = sexpr.getDatum().getList();
            List<LispSexpr> elements = list.getSexprList();
            if (elements.size() > 0) {
                LispSexpr head = elements.get(0);
                if (head.getDatum() != null && head.getDatum().getCompoundSymbol() != null) {
                    String headName = head.getText().toLowerCase();
                    headName = getSymbolName(headName);
                    info.shortForm = headName;
                    if ("defclass".equals(headName)) {
                        info.type = SexpressionType.DEFCLASS;
                        info.icon = SltIconProvider.CLASS;
                        String secondElement = getSecondElement(elements);
                        info.shortForm += secondElement;
                        info.identification = secondElement.trim();
                    } else if ("defconstant".equals(headName)) {
                        info.type = SexpressionType.DEFCONSTANT;
                        info.icon = SltIconProvider.CONSTANT;
                        String secondElement = getSecondElement(elements);
                        info.shortForm += secondElement;
                        info.identification = secondElement.trim();
                    } else if ("defgeneric".equals(headName)) {
                        info.type = SexpressionType.DEFGENERIC;
                        info.icon = SltIconProvider.METHOD;
                        String secondElement = getSecondElement(elements);
                        info.shortForm += secondElement;
                        info.identification = secondElement.trim();
                        info.longForm = getLongForm(elements, 2);
                    } else if ("defmacro".equals(headName)) {
                        info.type = SexpressionType.DEFMACRO;
                        info.icon = SltIconProvider.MACRO;
                        String secondElement = getSecondElement(elements);
                        info.shortForm += secondElement;
                        info.identification = secondElement.trim();
                        info.longForm = getLongForm(elements, 2);
                    } else if ("defmethod".equals(headName)) {
                        info.type = SexpressionType.DEFMETHOD;
                        info.icon = SltIconProvider.METHOD;
                        String secondElement = getSecondElement(elements);
                        info.shortForm += secondElement;
                        info.identification = secondElement.trim();
                        if (elements.size() > 2) {
                            LispSexpr potentialArgs = elements.get(2);
                            if (potentialArgs.getDatum() != null && potentialArgs.getDatum().getCompoundSymbol() != null) {
                                info.shortForm += " " + potentialArgs.getText();
                                info.identification = potentialArgs.getText();
                                info.longForm = getLongForm(elements, 3);
                            } else {
                                info.longForm = getLongForm(elements, 2);
                            }
                        }
                    } else if ("defpackage".equals(headName)) {
                        info.type = SexpressionType.DEFPACKAGE;
                        info.icon = SltIconProvider.PACKAGE;
                        String secondElement = getSecondElement(elements);
                        info.shortForm += secondElement;
                        info.identification = secondElement.trim();
                    } else if ("defsetf".equals(headName)) {
                        info.type = SexpressionType.DEFSETF;
                        info.icon = SltIconProvider.FUNCTION;
                        String secondElement = getSecondElement(elements);
                        info.shortForm += secondElement;
                        info.identification = secondElement.trim();
                    } else if ("defstruct".equals(headName)) {
                        info.type = SexpressionType.DEFSTRUCT;
                        info.icon = SltIconProvider.STRUCTURE;
                        String secondElement = getSecondElement(elements);
                        info.shortForm += secondElement;
                        info.identification = secondElement.trim();
                    } else if ("deftype".equals(headName)) {
                        info.type = SexpressionType.DEFTYPE;
                        info.icon = SltIconProvider.STRUCTURE;
                        String secondElement = getSecondElement(elements);
                        info.shortForm += secondElement;
                        info.identification = secondElement.trim();
                        info.longForm = getLongForm(elements, 2);
                    } else if ("defun".equals(headName)) {
                        info.type = SexpressionType.DEFUN;
                        info.icon = SltIconProvider.FUNCTION;
                        String secondElement = getSecondElement(elements);
                        info.shortForm += secondElement;
                        info.identification = secondElement.trim();
                        info.longForm = getLongForm(elements, 2);
                    } else if ("defparameter".equals(headName)) {
                        info.type = SexpressionType.DEFPARAMETER;
                        info.icon = SltIconProvider.SPECIAL;
                        String secondElement = getSecondElement(elements);
                        info.shortForm += secondElement;
                        info.identification = secondElement.trim();
                    } else if ("defvar".equals(headName)) {
                        info.type = SexpressionType.DEFVAR;
                        info.icon = SltIconProvider.SPECIAL;
                        String secondElement = getSecondElement(elements);
                        info.shortForm += secondElement;
                        info.identification = secondElement.trim();
                    }
                }
            }
        }

        return info;
    }

    private static String getLongForm(List<LispSexpr> elements, int i) {
        if (elements.size() > i) {
            String text = elements.get(i).getText();
            return text.replaceAll(LispLexerUtils.whitespace.pattern() + "+", " ");
        }
        return null;
    }

    private static String getSymbolName(String symbol) {
        if (symbol.contains("::")) {
            return symbol.split(Pattern.quote("::"))[1];
        } else if (symbol.contains(":")) {
            return symbol.split(Pattern.quote(":"))[1];
        }
        return symbol;
    }

    private static String getSecondElement(List<LispSexpr> elements) {
        if (elements.size() > 1) {
            return " " + elements.get(1).getText();
        }
        return "";
    }

    public enum QuoteState {
        BACKQUOTE, QUOTE, UNQUOTE, UNQUOTE_SPLICE, NO_STATE, ERROR_STATE;

        public static boolean isQuoted(QuoteState quoteState) {
            return quoteState == BACKQUOTE || quoteState == QUOTE;
        }
    }

    public enum SexpressionType {
        EXPRESSION,
        DEFCLASS, DEFCONSTANT, DEFGENERIC,
        DEFMACRO, DEFMETHOD, DEFPACKAGE, DEFPARAMETER, DEFSETF, DEFSTRUCT,
        DEFTYPE, DEFUN, DEFVAR
    }

    public static class LispSexpressionInfo {

        private SexpressionType type;
        private String shortForm;
        private String identification;
        private String longForm;
        private Icon icon;

        public SexpressionType getType() {
            return type;
        }

        public String getShortForm() {
            return shortForm == null ? "" : shortForm;
        }

        public Icon getIcon() {
            return icon;
        }

        public String getIdentification() {
            return identification;
        }

        public String getLongForm() {
            return identification + (longForm == null ? "" : ": " + longForm);
        }
    }
}
