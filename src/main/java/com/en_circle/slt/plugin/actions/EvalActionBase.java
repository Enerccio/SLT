package com.en_circle.slt.plugin.actions;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.SltSBCL;
import com.en_circle.slt.plugin.swank.requests.SltEval;
import com.en_circle.slt.plugin.swank.requests.SwankEvalFromVirtualFile;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

public abstract class EvalActionBase extends AnAction {
    private static final Logger LOG = LoggerFactory.getLogger(EvalActionBase.class);

    @Override
    public void update(@NotNull AnActionEvent event) {
        event.getPresentation().setEnabledAndVisible(false);

        Editor editor = event.getData(CommonDataKeys.EDITOR);
        if (editor != null) {
            PsiFile file = PsiDocumentManager.getInstance(Objects.requireNonNull(editor.getProject())).getPsiFile(editor.getDocument());
            if (file != null && SltCommonLispFileType.INSTANCE.equals(file.getFileType())) {
                event.getPresentation().setEnabledAndVisible(true);
            }
        }
    }

    protected void evaluate(Project project, String buffer, Runnable callback) {
        try {
            SltSBCL.getInstance().sendToSbcl(SltEval.eval(buffer, result -> callback.run()), true);
        } catch (Exception e) {
            LOG.warn("Error starting sbcl", e);
            Messages.showErrorDialog(project, e.getMessage(), "Failed to Start SBCL");
        }
    }

    protected void evaluateRegion(Project project, String buffer, String filename, int lineno, int charno, Runnable callback) {
        try {
            SltSBCL.getInstance().sendToSbcl(SwankEvalFromVirtualFile
                    .eval(buffer, filename, lineno, charno, result -> callback.run()), true);
        } catch (Exception e) {
            LOG.warn("Error starting sbcl", e);
            Messages.showErrorDialog(project, e.getMessage(), "Failed to Start SBCL");
        }
    }

}
