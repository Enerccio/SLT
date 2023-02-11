package com.en_circle.slt.plugin.lisp.psi.impl;

import com.en_circle.slt.plugin.lisp.psi.LispCommentElement;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

public class LispCommentElementImpl extends ASTWrapperPsiElement implements LispCommentElement {

    public LispCommentElementImpl(@NotNull ASTNode node) {
        super(node);
    }

    @Override
    public @NotNull IElementType getTokenType() {
        return getNode().getElementType();
    }
}
