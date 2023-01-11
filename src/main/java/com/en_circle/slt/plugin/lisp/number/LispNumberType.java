package com.en_circle.slt.plugin.lisp.number;

import com.en_circle.slt.plugin.lisp.psi.LispElementType;
import com.intellij.psi.tree.IElementType;

public interface LispNumberType {

    IElementType SIGN = new LispElementType("SIGN");
    IElementType DIGIT = new LispElementType("DIGIT");
    IElementType SLASH = new LispElementType("SLASH");
    IElementType EXPONENT_MARKER = new LispElementType("EXPONENT_MARKER");

}
