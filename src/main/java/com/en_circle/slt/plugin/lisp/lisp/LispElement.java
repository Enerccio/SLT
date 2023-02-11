package com.en_circle.slt.plugin.lisp.lisp;

import java.util.Objects;

public interface LispElement {

    LispElementType getType();

    default String toPrettyString() {
        return toString();
    }

    default String nativeString() {
        if (this instanceof LispAtom<?> atom) {
            return Objects.toString(atom.getValue());
        } else {
            return toString();
        }
    }
}
