package com.en_circle.slt.plugin.lisp;

import com.en_circle.slt.plugin.SymbolState;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.intellij.icons.AllIcons.Nodes;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.NlsSafe;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class LispPotentialSymbolPresentation implements ItemPresentation {

    private final Project project;
    private final PsiFile file;
    private final String symbol;
    private final String packageName;
    private final int offset;

    public LispPotentialSymbolPresentation(Project project, PsiFile file, String name, String packageName, int offset) {
        this.project = project;
        this.file = file;
        this.symbol = name;
        this.packageName = packageName;
        this.offset = offset;
    }

    @Override
    public @NlsSafe @Nullable String getPresentableText() {
        return symbol;
    }

    private SymbolState refreshState() {
        return LispEnvironmentService.getInstance(project)
                .refreshSymbolFromServer(packageName, symbol);
    }

    @Override
    public @NlsSafe @Nullable String getLocationString() {
        if (file != null) {
            Document d = PsiDocumentManager.getInstance(project).getDocument(file);
            if (d != null) {
                int line = d.getLineNumber(offset);
                return file.getName() + ": " + line;
            }
            return file.getName();
        }
        return null;
    }

    @Override
    public @Nullable Icon getIcon(boolean unused) {
        SymbolState state = refreshState();

        return switch (state.binding) {
            case NONE, SPECIAL_FORM -> null;
            case FUNCTION -> Nodes.Function;
            case MACRO -> Nodes.Template;
            case CONSTANT, KEYWORD -> Nodes.Constant;
            case SPECIAL_VARIABLE -> Nodes.Gvariable;
            case CLASS -> Nodes.Class;
            case METHOD -> Nodes.Method;
        };
    }
}
