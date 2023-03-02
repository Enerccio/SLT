package com.en_circle.slt.plugin.lisp.lisp.components;

import com.en_circle.slt.plugin.lisp.psi.LispList;

public class LispLambdaSignature {

    public LispLambdaSignature(LispList list, boolean isMethod) {
        parseSignature(list, isMethod);
    }

    public LispLambdaSignature() {
        // NIL signature
    }

    private void parseSignature(LispList list, boolean isMethod) {

    }

}
