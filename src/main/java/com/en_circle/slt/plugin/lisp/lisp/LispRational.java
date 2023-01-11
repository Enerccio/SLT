package com.en_circle.slt.plugin.lisp.lisp;

import com.en_circle.slt.plugin.lisp.lisp.LispRational.RationalNumber;

import java.math.BigInteger;

public class LispRational extends LispAtom<RationalNumber> {

    private final RationalNumber value;

    public LispRational(String textValue, RationalNumber rationalNumber) {
        super(textValue);
        this.value = rationalNumber;
    }

    @Override
    public RationalNumber getValue() {
        return value;
    }

    @Override
    public LispElementType getType() {
        return LispElementType.NUMBER;
    }

    public static class RationalNumber {

        public final BigInteger p;
        public final BigInteger q;

        public RationalNumber(BigInteger p, BigInteger q) {
            this.p = p;
            this.q = q;
        }

        public BigInteger getP() {
            return p;
        }

        public BigInteger getQ() {
            return q;
        }
    }
}
