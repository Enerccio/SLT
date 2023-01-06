package com.en_circle.slt.plugin.lisp.lisp;

import java.math.BigInteger;

public class LispInteger extends LispAtom<BigInteger> {

    private BigInteger value;

    public LispInteger(String value) {
        super(value);
        this.value = new BigInteger(value);
    }

    @Override
    protected BigInteger getValue() {
        return value;
    }

    @Override
    public LispElementType getType() {
        return LispElementType.NUMBER;
    }
}
