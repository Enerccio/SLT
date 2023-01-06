package com.en_circle.slt.plugin.lisp.psi;

import com.en_circle.slt.plugin.SltCommonLispLanguage;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class LispTokenType extends IElementType {
    public LispTokenType(@NotNull @NonNls String debugName) {
        super(debugName, SltCommonLispLanguage.INSTANCE);
    }

    @Override
    public String toString() {
        return "LispTokenType." + super.toString();
    }
}
