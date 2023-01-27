package com.en_circle.slt.plugin.references;

import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.AbstractElementManipulator;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class SltSymbolManipulator extends AbstractElementManipulator<LispSymbol> {

    @Override
    public @Nullable LispSymbol handleContentChange(@NotNull LispSymbol element, @NotNull TextRange range, String newContent) throws IncorrectOperationException {
        element.setName(newContent);
        return element;
    }

}
