package com.en_circle.slt.plugin.lisp.lisp;

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
import org.codehaus.plexus.util.StringUtils;
import org.jetbrains.annotations.NotNull;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class LispUtils {

    public static List<LispElement> convertAst(PsiFile source) {
        List<LispElement> elements = new ArrayList<>();

        source.accept(new LispVisitorImpl(elements));

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

        public LispVisitorImpl(List<LispElement> elements) {
            stack = new Stack<>();
            stack.add(elements);
        }

        @Override
        public void visitArray(@NotNull LispArray o) {
            stack.push(new ArrayList<>());
            super.visitArray(o);
            List<LispElement> self = stack.pop();
            stack.peek().add(new LispContainer(self, ContainerType.VECTOR));
        }

        @Override
        public void visitBinaryNumber(@NotNull LispBinaryNumber o) {
            super.visitBinaryNumber(o);
            stack.lastElement().add(new LispUnparsedElement(o.getText()));
        }

        @Override
        public void visitHexNumber(@NotNull LispHexNumber o) {
            super.visitHexNumber(o);
            stack.lastElement().add(new LispUnparsedElement(o.getText()));
        }

        @Override
        public void visitInteger(@NotNull LispInteger o) {
            super.visitInteger(o);
            LispNumberParser parser = new LispNumberParser();
            IElementType elementType = parser.parse(lexer, o.getText());
            com.en_circle.slt.plugin.lisp.lisp.LispInteger lispInteger = new com.en_circle.slt.plugin.lisp.lisp.LispInteger(o.getText(),
                    parser.getAsBigInteger(elementType));
            stack.lastElement().add(lispInteger);
        }

        @Override
        public void visitList(@NotNull LispList o) {
            stack.push(new ArrayList<>());
            super.visitList(o);
            List<LispElement> self = stack.pop();
            stack.peek().add(new LispContainer(self, ContainerType.LIST));
        }

        @Override
        public void visitOctalNumber(@NotNull LispOctalNumber o) {
            super.visitOctalNumber(o);
            stack.lastElement().add(new LispUnparsedElement(o.getText()));
        }

        @Override
        public void visitPathname(@NotNull LispPathname o) {
            super.visitPathname(o);
            stack.lastElement().add(new LispUnparsedElement(o.getText()));
        }

        @Override
        public void visitRadixNumber(@NotNull LispRadixNumber o) {
            super.visitRadixNumber(o);
            stack.lastElement().add(new LispUnparsedElement(o.getText()));
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
        }

        @Override
        public void visitReal(@NotNull LispReal o) {
            super.visitReal(o);
            LispNumberParser parser = new LispNumberParser();
            IElementType elementType = parser.parse(lexer, o.getText());
            LispDouble lispDouble = new LispDouble(o.getText(),
                    parser.getAsBigDecimal(elementType));
            stack.lastElement().add(lispDouble);
        }

        @Override
        public void visitRealPair(@NotNull LispRealPair o) {
            stack.push(new ArrayList<>());
            super.visitRealPair(o);
            List<LispElement> self = stack.pop();
            stack.peek().add(new LispComplex(o.getText(),
                    new ComplexNumber((LispDouble) self.get(0), (LispDouble) self.get(1))));
        }

        @Override
        public void visitString(@NotNull LispString o) {
            super.visitString(o);
            stack.lastElement().add(new com.en_circle.slt.plugin.lisp.lisp.LispString(o.getText()));
        }

        @Override
        public void visitStructure(@NotNull LispStructure o) {
            stack.push(new ArrayList<>());
            stack.peek().add(new com.en_circle.slt.plugin.lisp.lisp.LispSymbol("defstructure"));
            super.visitStructure(o);
            List<LispElement> self = stack.pop();
            stack.peek().add(new LispContainer(self, ContainerType.PAIR));
        }

        @Override
        public void visitSymbol(@NotNull LispSymbol o) {
            super.visitSymbol(o);
            stack.lastElement().add(new com.en_circle.slt.plugin.lisp.lisp.LispSymbol(o.getText()));
        }

        @Override
        public void visitVector(@NotNull LispVector o) {
            stack.push(new ArrayList<>());
            super.visitVector(o);
            List<LispElement> self = stack.pop();
            stack.peek().add(new LispContainer(self, ContainerType.VECTOR));
        }

        @Override
        public void visitPair(@NotNull LispPair o) {
            stack.push(new ArrayList<>());
            super.visitPair(o);
            List<LispElement> self = stack.pop();
            stack.peek().add(new LispContainer(self, ContainerType.PAIR));
        }

        @Override
        public void visitElement(@NotNull PsiElement element) {
            element.acceptChildren(this);
        }
    }
}
