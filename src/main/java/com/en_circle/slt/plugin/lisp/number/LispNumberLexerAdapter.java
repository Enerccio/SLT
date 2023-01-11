package com.en_circle.slt.plugin.lisp.number;

import com.intellij.lexer.FlexAdapter;

public class LispNumberLexerAdapter extends FlexAdapter {

    public LispNumberLexerAdapter() {
        super(new LispNumberLexer(null));
    }

}
