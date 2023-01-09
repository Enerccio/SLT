package com.en_circle.slt.plugin.lisp.lisp;

public class LispString extends LispAtom<String> {

    public LispString(String value) {
        super(value.substring(1, value.length() - 1));
    }

    @Override
    public String getValue() {
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
