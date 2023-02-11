package com.en_circle.slt.plugin.lisp;

import com.en_circle.slt.plugin.SltIconProvider;
import com.en_circle.slt.plugin.SymbolState;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.util.NlsSafe;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class LispSymbolPresentation implements ItemPresentation {

    private final com.en_circle.slt.plugin.lisp.psi.LispSymbol symbol;
    private final String packageName;

    public LispSymbolPresentation(com.en_circle.slt.plugin.lisp.psi.LispSymbol symbol) {
        this.symbol = symbol;
        this.packageName = LispParserUtil.getPackage(symbol);
    }

    @Override
    public @NlsSafe @Nullable String getPresentableText() {
        return symbol.getName();
    }

    private SymbolState refreshState() {
        return LispEnvironmentService.getInstance(symbol.getProject())
                .refreshSymbolFromServer(packageName, symbol.getName());
    }

    @Override
    public @NlsSafe @Nullable String getLocationString() {
        PsiFile containingFile = symbol.getContainingFile();
        if (containingFile != null) {
            Document d = PsiDocumentManager.getInstance(symbol.getProject()).getDocument(containingFile);
            if (d != null) {
                int offset = symbol.getTextOffset();
                int line = d.getLineNumber(offset);
                return containingFile.getName() + ": " + line;
            }
            return containingFile.getName();
        }
        return null;
    }

    @Override
    public @Nullable Icon getIcon(boolean unused) {
        SymbolState state = refreshState();

        return switch (state.binding) {
            case NONE, SPECIAL_FORM -> null;
            case FUNCTION -> SltIconProvider.FUNCTION;
            case MACRO -> SltIconProvider.MACRO;
            case CONSTANT, KEYWORD -> SltIconProvider.CONSTANT;
            case SPECIAL_VARIABLE -> SltIconProvider.SPECIAL;
            case CLASS -> SltIconProvider.CLASS;
            case METHOD -> SltIconProvider.METHOD;
        };
    }
}
