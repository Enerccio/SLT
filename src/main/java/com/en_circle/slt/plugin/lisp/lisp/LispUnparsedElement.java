package com.en_circle.slt.plugin.lisp.lisp;

public class LispUnparsedElement extends LispAtom<String> {

    public LispUnparsedElement(String value) {
        super(value);
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
        return LispElementType.UNPARSED;
    }
}
