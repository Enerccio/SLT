package com.en_circle.slt.plugin.lisp.lisp;

import java.math.BigDecimal;

public class LispDouble extends LispAtom<BigDecimal> {

    private final BigDecimal value;

    public LispDouble(String value, BigDecimal bigDecimal) {
        super(value);
        this.value = bigDecimal;
    }

    @Override
    public BigDecimal getValue() {
        return value;
    }

    @Override
    public LispElementType getType() {
        return LispElementType.NUMBER;
    }
}
