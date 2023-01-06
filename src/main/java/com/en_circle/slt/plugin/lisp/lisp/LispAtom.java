package com.en_circle.slt.plugin.lisp.lisp;

public abstract class LispAtom<T> implements LispElement {

    protected final String textValue;

    public LispAtom(String value) {
        this.textValue = value;
    }

    protected abstract T getValue();

    @Override
    public String toString() {
        return textValue;
    }

}
