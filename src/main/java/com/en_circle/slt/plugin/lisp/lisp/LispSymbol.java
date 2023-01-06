package com.en_circle.slt.plugin.lisp.lisp;

public class LispSymbol extends LispAtom<String> {

    public LispSymbol(String value) {
        super(value);
    }

    @Override
    protected String getValue() {
        return textValue;
    }

    @Override
    public LispElementType getType() {
        return LispElementType.SYMBOL;
    }
}
