package com.en_circle.slt.plugin.lisp.lisp;

public class LispString extends LispAtom<String> {

    public LispString(String value) {
        super(value);
    }

    @Override
    protected String getValue() {
        return textValue;
    }

    @Override
    public String toString() {
        return "\"" + textValue + "\"";
    }

    @Override
    public LispElementType getType() {
        return LispElementType.STRING;
    }
}
