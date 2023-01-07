package com.en_circle.slt.plugin.lisp.lisp;

import java.math.BigDecimal;

public class LispDouble extends LispAtom<BigDecimal> {

    private BigDecimal value;

    public LispDouble(String value) {
        super(value);
        this.value = new BigDecimal(value);
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
