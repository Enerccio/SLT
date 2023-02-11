package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.lisp.psi.LispString;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.en_circle.slt.plugin.spellcheck.LispCommentTokenizer;
import com.en_circle.slt.plugin.spellcheck.LispStringTokenizer;
import com.intellij.psi.PsiElement;
import com.intellij.spellchecker.tokenizer.SpellcheckingStrategy;
import com.intellij.spellchecker.tokenizer.Tokenizer;
import org.jetbrains.annotations.NotNull;

public class SltSpellCheckingStrategy extends SpellcheckingStrategy {
    private final LispStringTokenizer stringTokenizer = new LispStringTokenizer();
    private final LispCommentTokenizer commentTokenizer = new LispCommentTokenizer();

    @Override
    public @NotNull Tokenizer<?> getTokenizer(PsiElement element) {
        if (element instanceof LispString) {
            return stringTokenizer;
        }
        if (element.getNode().getElementType() == LispTypes.BLOCK_COMMENT) {
            return commentTokenizer;
        }
        if (element.getNode().getElementType() == LispTypes.LINE_COMMENT) {
            return commentTokenizer;
        }
        return EMPTY_TOKENIZER;
    }
}
