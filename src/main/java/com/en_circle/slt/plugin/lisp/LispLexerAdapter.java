package com.en_circle.slt.plugin.lisp;

import com.intellij.lexer.FlexAdapter;
import com.intellij.lexer.FlexLexer;
import org.jetbrains.annotations.NotNull;

public class LispLexerAdapter extends FlexAdapter {
    public LispLexerAdapter() {
        super(new LispLexer(null));
    }
}
