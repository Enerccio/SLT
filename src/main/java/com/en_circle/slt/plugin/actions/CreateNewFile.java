package com.en_circle.slt.plugin.actions;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.SltIconProvider;
import com.intellij.ide.actions.CreateFileFromTemplateAction;
import com.intellij.ide.actions.CreateFileFromTemplateDialog;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.NlsActions.ActionDescription;
import com.intellij.openapi.util.NlsActions.ActionText;
import com.intellij.openapi.util.NlsContexts.Command;
import com.intellij.psi.PsiDirectory;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class CreateNewFile extends CreateFileFromTemplateAction implements DumbAware {

    public CreateNewFile() {
        super("Create New Common Lisp File", "", SltIconProvider.getFileIcon());
    }

    @Override
    protected void buildDialog(@NotNull Project project, @NotNull PsiDirectory directory, @NotNull CreateFileFromTemplateDialog.Builder builder) {
        builder.setTitle("New Common Lisp File")
                .addKind(SltCommonLispFileType.INSTANCE.getName(), SltIconProvider.getFileIcon(), "Common Lisp File.cl.ft");
    }

    @Override
    protected @Command String getActionName(PsiDirectory directory, @NonNls @NotNull String newName, @NonNls String templateName) {
        return SltCommonLispFileType.INSTANCE.getName();
    }
}
