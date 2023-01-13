package com.en_circle.slt.plugin.lisp.lisp;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

public class LispSymbol extends LispAtom<String> {

    public LispSymbol(String value) {
        super(value);
    }

    @Override
    public String getValue() {
        return textValue;
    }

    @Override
    public LispElementType getType() {
        return LispElementType.SYMBOL;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        LispSymbol that = (LispSymbol) o;

        return new EqualsBuilder().append(textValue.toUpperCase(), that.textValue.toUpperCase()).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37).append(textValue.toUpperCase()).toHashCode();
    }

}
