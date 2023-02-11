package com.en_circle.slt.plugin.lisp.lisp;

import com.en_circle.slt.plugin.lisp.LispLexerUtils;
import com.en_circle.slt.plugin.lisp.lisp.LispComplex.ComplexNumber;
import com.en_circle.slt.plugin.lisp.lisp.LispContainer.ContainerType;
import com.en_circle.slt.plugin.lisp.lisp.LispRational.RationalNumber;
import com.en_circle.slt.plugin.lisp.number.LispNumberLexerAdapter;
import com.en_circle.slt.plugin.lisp.number.LispNumberParser;
import com.en_circle.slt.plugin.lisp.psi.LispInteger;
import com.en_circle.slt.plugin.lisp.psi.LispString;
import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.en_circle.slt.plugin.lisp.psi.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiRecursiveVisitor;
import com.intellij.psi.tree.IElementType;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Stack;

public class LispUtils {

    public static List<LispElement> convertAst(PsiFile source) {
        return convertAst(source, null, null);
    }

    public static List<LispElement> convertAst(PsiFile source, Map<LispElement, OffsetInfo> lineOffsets, String offsetText) {
        List<LispElement> elements = new ArrayList<>();

        source.accept(new LispVisitorImpl(elements, lineOffsets, offsetText));

        return elements;
    }

    public static List<LispElement> convertAst(LispToplevel source) {
        return convertAst(source, null, null);
    }

    public static List<LispElement> convertAst(LispToplevel source, Map<LispElement, OffsetInfo> lineOffsets, String offsetText) {
        List<LispElement> elements = new ArrayList<>();

        source.accept(new LispVisitorImpl(elements, lineOffsets, offsetText));

        return elements;
    }

    public static String unescape(String text) {
        text = StringUtils.replace(text, "\\\"", "\"");
        return text;
    }

    public static boolean hasPValue(LispContainer src, com.en_circle.slt.plugin.lisp.lisp.LispSymbol lispSymbol) {
        int ix = src.getItems().indexOf(lispSymbol);
        return ix >= 0 && ix % 2 == 0 && ix < src.getItems().size() - 1;
    }

    public static LispElement pvalue(LispContainer src, com.en_circle.slt.plugin.lisp.lisp.LispSymbol lispSymbol) {
        int ix = src.getItems().indexOf(lispSymbol);
        return src.getItems().get(ix + 1);
    }

    private static class LispVisitorImpl extends LispVisitor implements PsiRecursiveVisitor {

        private final Stack<List<LispElement>> stack;
        private final LispNumberLexerAdapter lexer = new LispNumberLexerAdapter();
        private final Map<LispElement, OffsetInfo> lineOffsets;
        private final String offsetText;

        public LispVisitorImpl(List<LispElement> elements, Map<LispElement, OffsetInfo> lineOffsets, String offsetText) {
            this.stack = new Stack<>();
            this.stack.add(elements);
            this.lineOffsets = lineOffsets;
            this.offsetText = offsetText;
        }

        @Override
        public void visitArray(@NotNull LispArray o) {
            stack.push(new ArrayList<>());
            super.visitArray(o);
            List<LispElement> self = stack.pop();
            LispContainer container = new LispContainer(self, ContainerType.VECTOR);
            stack.peek().add(container);
            addOffset(o, container);
        }

        @Override
        public void visitBinaryNumber(@NotNull LispBinaryNumber o) {
            super.visitBinaryNumber(o);
            LispElement element = new LispUnparsedElement(o.getText());
            stack.lastElement().add(element);
            addOffset(o, element);
        }

        @Override
        public void visitHexNumber(@NotNull LispHexNumber o) {
            super.visitHexNumber(o);
            LispElement element = new LispUnparsedElement(o.getText());
            stack.lastElement().add(element);
            addOffset(o, element);
        }

        @Override
        public void visitInteger(@NotNull LispInteger o) {
            super.visitInteger(o);
            LispNumberParser parser = new LispNumberParser();
            IElementType elementType = parser.parse(lexer, o.getText());
            com.en_circle.slt.plugin.lisp.lisp.LispInteger lispInteger = new com.en_circle.slt.plugin.lisp.lisp.LispInteger(o.getText(),
                    parser.getAsBigInteger(elementType));
            stack.lastElement().add(lispInteger);
            addOffset(o, lispInteger);
        }

        @Override
        public void visitList(@NotNull LispList o) {
            stack.push(new ArrayList<>());
            super.visitList(o);
            List<LispElement> self = stack.pop();
            LispContainer container = new LispContainer(self, ContainerType.LIST);
            stack.peek().add(container);
            addOffset(o, container);
        }

        @Override
        public void visitOctalNumber(@NotNull LispOctalNumber o) {
            super.visitOctalNumber(o);
            LispElement element = new LispUnparsedElement(o.getText());
            stack.lastElement().add(element);
            addOffset(o, element);
        }

        @Override
        public void visitPathname(@NotNull LispPathname o) {
            super.visitPathname(o);
            LispElement element = new LispUnparsedElement(o.getText());
            stack.lastElement().add(element);
            addOffset(o, element);
        }

        @Override
        public void visitRadixNumber(@NotNull LispRadixNumber o) {
            super.visitRadixNumber(o);
            LispElement element = new LispUnparsedElement(o.getText());
            stack.lastElement().add(element);
            addOffset(o, element);
        }

        @Override
        public void visitRatio(@NotNull LispRatio o) {
            super.visitRatio(o);
            LispNumberParser parser = new LispNumberParser();
            parser.parse(lexer, o.getText());
            RationalNumber rationalNumber = new RationalNumber(
                    new BigInteger(parser.getCtx().p.toString()),
                    new BigInteger(parser.getCtx().q.toString()));
            LispRational lispRational = new LispRational(o.getText(), rationalNumber);
            stack.lastElement().add(lispRational);
            addOffset(o, lispRational);
        }

        @Override
        public void visitReal(@NotNull LispReal o) {
            super.visitReal(o);
            LispNumberParser parser = new LispNumberParser();
            IElementType elementType = parser.parse(lexer, o.getText());
            LispDouble lispDouble = new LispDouble(o.getText(),
                    parser.getAsBigDecimal(elementType));
            stack.lastElement().add(lispDouble);
            addOffset(o, lispDouble);
        }

        @Override
        public void visitRealPair(@NotNull LispRealPair o) {
            stack.push(new ArrayList<>());
            super.visitRealPair(o);
            List<LispElement> self = stack.pop();
            LispElement element = new LispComplex(o.getText(),
                    new ComplexNumber((LispDouble) self.get(0), (LispDouble) self.get(1)));
            stack.peek().add(element);
            addOffset(o, element);
        }

        @Override
        public void visitString(@NotNull LispString o) {
            super.visitString(o);
            LispElement element = new com.en_circle.slt.plugin.lisp.lisp.LispString(o.getText());
            stack.lastElement().add(element);
            addOffset(o, element);
        }

        @Override
        public void visitStructure(@NotNull LispStructure o) {
            stack.push(new ArrayList<>());
            stack.peek().add(new com.en_circle.slt.plugin.lisp.lisp.LispSymbol("defstructure"));
            super.visitStructure(o);
            List<LispElement> self = stack.pop();
            LispContainer container = new LispContainer(self, ContainerType.LIST);
            stack.peek().add(container);
            addOffset(o, container);
        }

        @Override
        public void visitSymbol(@NotNull LispSymbol o) {
            super.visitSymbol(o);
            LispElement element = new com.en_circle.slt.plugin.lisp.lisp.LispSymbol(o.getText());
            stack.lastElement().add(element);
            addOffset(o, element);
        }

        @Override
        public void visitVector(@NotNull LispVector o) {
            stack.push(new ArrayList<>());
            super.visitVector(o);
            List<LispElement> self = stack.pop();
            LispContainer container = new LispContainer(self, ContainerType.VECTOR);
            stack.peek().add(container);
            addOffset(o, container);
        }

        private void addOffset(PsiElement element, LispElement elementDef) {
            if (lineOffsets != null) {
                OffsetInfo offsetInfo = new OffsetInfo();
                offsetInfo.base = getOffsetFromLine(element);
                offsetInfo.parentForm = getOffsetFromElemenet(element);
                offsetInfo.elementOffset = getElementOffset(element);
                lineOffsets.put(elementDef, offsetInfo);
            }
        }

        private Integer getOffsetFromLine(PsiElement element) {
            int offset = element.getTextOffset();
            return getOffsetSinceNewLine(offsetText, offset);
        }

        private static int getOffsetSinceNewLine(String documentText, int textOffset) {
            int offset = 0;
            while (textOffset >= 0) {
                if (documentText.charAt(textOffset) == '\n') {
                    break;
                }
                if (LispLexerUtils.isWhitespace(documentText.charAt(textOffset))) {
                    ++offset;
                } else {
                    // anything else resets offset
                    offset = 0;
                }
                --textOffset;
            }
            return offset;
        }

        private Integer getOffsetFromElemenet(PsiElement element) {
            int offset = element.getTextOffset();
            return getOffsetForForm(offsetText, offset);
        }

        private Integer getOffsetForForm(String documentText, int textOffset) {
            int offset = 0;
            boolean parentMatched = false;
            while (textOffset >= 0) {
                if (documentText.charAt(textOffset) == '\n') {
                    break;
                }
                if (parentMatched) {
                    ++offset;
                }
                if (documentText.charAt(textOffset) == '(' && !parentMatched) {
                    parentMatched = true;
                }
                --textOffset;
            }
            return offset;
        }

        private Integer getElementOffset(PsiElement element) {
            int offset = element.getTextOffset();
            return getElementOffset(offsetText, offset);
        }

        private Integer getElementOffset(String documentText, int textOffset) {
            int offset = 0;
            while (textOffset >= 0) {
                if (documentText.charAt(textOffset) == '\n') {
                    break;
                }
                --textOffset;
                ++offset;
            }
            return offset;
        }

        @Override
        public void visitElement(@NotNull PsiElement element) {
            element.acceptChildren(this);
        }
    }

    public static class OffsetInfo {

        public static final OffsetInfo DEFAULT = new OffsetInfo();

        public int base;
        public int parentForm;
        public int elementOffset;

        @Override
        public String toString() {
            return "OFS(" + base + "," + parentForm + "," + elementOffset + ")";
        }

    }
}
