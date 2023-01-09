package com.en_circle.slt.plugin.lisp.lisp;

public interface LispElement {

    LispElementType getType();
    default String toPrettyString() {
        return toString();
    }

}
