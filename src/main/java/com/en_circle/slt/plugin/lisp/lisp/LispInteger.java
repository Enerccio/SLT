package com.en_circle.slt.plugin.lisp.lisp;

import java.math.BigInteger;

public class LispInteger extends LispAtom<BigInteger> {

    private final BigInteger value;

    public LispInteger(String value, BigInteger integer) {
        super(value);
        this.value = integer;
    }

    @Override
    public BigInteger getValue() {
        return value;
    }

    @Override
    public LispElementType getType() {
        return LispElementType.NUMBER;
    }
}
