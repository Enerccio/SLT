package com.en_circle.slt.plugin.actions;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.environment.LispFeatures;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.intellij.openapi.actionSystem.ActionUpdateThread;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.DefaultActionGroup;
import com.intellij.openapi.editor.Editor;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;

public class MacroexpandGroup extends DefaultActionGroup {

    public MacroexpandGroup() {
        super(() -> SltBundle.message("action.slt.actions.macroexpand.group.text"), true);
    }

    @Override
    public void update(@NotNull AnActionEvent event) {
        event.getPresentation().setEnabledAndVisible(false);
        Editor editor = event.getData(CommonDataKeys.EDITOR);
        if (editor != null && event.getProject() != null) {
            PsiFile file = PsiDocumentManager.getInstance(Objects.requireNonNull(editor.getProject())).getPsiFile(editor.getDocument());
            if (file != null && SltCommonLispFileType.INSTANCE.equals(file.getFileType())) {
                event.getPresentation().setEnabledAndVisible(LispEnvironmentService.getInstance(event.getProject())
                        .hasFeature(LispFeatures.MACROEXPAND));
            }
        }
    }

    @Override
    public @NotNull ActionUpdateThread getActionUpdateThread() {
        return ActionUpdateThread.BGT;
    }

}
