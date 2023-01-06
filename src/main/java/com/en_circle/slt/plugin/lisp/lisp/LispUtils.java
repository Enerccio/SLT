package com.en_circle.slt.plugin.lisp.lisp;

import com.en_circle.slt.plugin.lisp.psi.LispNumber;
import com.en_circle.slt.plugin.lisp.psi.LispPair;
import com.en_circle.slt.plugin.lisp.psi.LispSugar;
import com.en_circle.slt.plugin.lisp.psi.LispVisitor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiRecursiveVisitor;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class LispUtils {

    public static List<LispElement> convertAst(PsiFile source) {
        List<LispElement> elements = new ArrayList<>();

        source.accept(new LispVisitorImpl(elements));

        return elements;
    }

    private static class LispVisitorImpl extends LispVisitor implements PsiRecursiveVisitor {

        private final Stack<List<LispElement>> stack;

        public LispVisitorImpl(List<LispElement> elements) {
            stack = new Stack<>();
            stack.add(elements);
        }

        @Override
        public void visitList(com.en_circle.slt.plugin.lisp.psi.@NotNull LispList o) {
            stack.push(new ArrayList<>());
            super.visitList(o);
            List<LispElement> self = stack.pop();
            stack.peek().add(new LispList(self, false));
        }

        @Override
        public void visitPair(@NotNull LispPair o) {
            stack.push(new ArrayList<>());
            super.visitPair(o);
            List<LispElement> self = stack.pop();
            stack.peek().add(new LispList(self, true));
        }

        @Override
        public void visitNumber(@NotNull LispNumber o) {
            super.visitNumber(o);
            LispDouble lispDouble = new LispDouble(o.getText());
            stack.peek().add(lispDouble);
        }

        @Override
        public void visitString(com.en_circle.slt.plugin.lisp.psi.@NotNull LispString o) {
            super.visitString(o);
            LispString lispString = new LispString(o.getText());
            stack.peek().add(lispString);
        }

        @Override
        public void visitSymbol(com.en_circle.slt.plugin.lisp.psi.@NotNull LispSymbol o) {
            super.visitSymbol(o);
            LispSymbol lispSymbol = new LispSymbol(o.getText());
            stack.peek().add(lispSymbol);
        }

        @Override
        public void visitSugar(@NotNull LispSugar o) {
            super.visitSugar(o);
        }

        @Override
        public void visitElement(@NotNull PsiElement element) {
            element.acceptChildren(this);
        }
    }
}
