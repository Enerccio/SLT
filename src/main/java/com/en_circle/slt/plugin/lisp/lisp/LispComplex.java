package com.en_circle.slt.plugin.lisp.lisp;

import com.en_circle.slt.plugin.lisp.lisp.LispComplex.ComplexNumber;

public class LispComplex extends LispAtom<ComplexNumber> {

    private final ComplexNumber value;

    public LispComplex(String textValue, ComplexNumber complexNumber) {
        super(textValue);
        this.value = complexNumber;
    }

    @Override
    public ComplexNumber getValue() {
        return value;
    }

    @Override
    public LispElementType getType() {
        return LispElementType.NUMBER;
    }

    public static class ComplexNumber {

        public final LispDouble r;
        public final LispDouble i;

        public ComplexNumber(LispDouble r, LispDouble i) {
            this.r = r;
            this.i = i;
        }

        public LispDouble getR() {
            return r;
        }

        public LispDouble getI() {
            return i;
        }
    }
}
