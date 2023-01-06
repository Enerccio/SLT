package com.en_circle.slt.plugin.lisp.psi;

import com.en_circle.slt.plugin.SltCommonLispLanguage;
import com.intellij.psi.tree.IElementType;

public class LispElementType extends IElementType {

    public LispElementType(String debugName) {
        super(debugName, SltCommonLispLanguage.INSTANCE);
    }

}
