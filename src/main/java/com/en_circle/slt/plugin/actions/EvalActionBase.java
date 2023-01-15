package com.en_circle.slt.plugin.actions;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.SltLispEnvironmentProvider;
import com.en_circle.slt.plugin.swank.requests.LoadFile;
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
    private static final Logger log = LoggerFactory.getLogger(EvalActionBase.class);

    @Override
    public void update(@NotNull AnActionEvent event) {
        event.getPresentation().setEnabledAndVisible(false);

        Editor editor = event.getData(CommonDataKeys.EDITOR);
        if (editor != null && event.getProject() != null) {
            PsiFile file = PsiDocumentManager.getInstance(Objects.requireNonNull(editor.getProject())).getPsiFile(editor.getDocument());
            if (file != null && SltCommonLispFileType.INSTANCE.equals(file.getFileType())) {
                event.getPresentation().setEnabledAndVisible(SltLispEnvironmentProvider.getInstance().hasEventsSet());
            }
        }
    }

    protected void evaluate(Project project, String buffer, String packageName, Runnable callback) {
        try {
            SltLispEnvironmentProvider.getInstance().sendToLisp(SltEval.eval(buffer, packageName, result -> callback.run()), true);
        } catch (Exception e) {
            log.warn(SltBundle.message("slt.error.sbclstart"), e);
            Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.sbcl.start"));
        }
    }

    protected void evaluateRegion(Project project, String buffer, String packageName, String filename, int bufferPosition, int lineno, int charno, Runnable callback) {
        try {
            SltLispEnvironmentProvider.getInstance().sendToLisp(SwankEvalFromVirtualFile
                    .eval(buffer, filename, bufferPosition, lineno, charno, packageName, result -> callback.run()), true);
        } catch (Exception e) {
            log.warn(SltBundle.message("slt.error.sbclstart"), e);
            Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.sbcl.start"));
        }
    }

    protected void evaluateFile(Project project, String filename) {
        try {
            SltLispEnvironmentProvider.getInstance().sendToLisp(LoadFile.loadFile(filename), true);
        } catch (Exception e) {
            log.warn(SltBundle.message("slt.error.sbclstart"), e);
            Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.sbcl.start"));
        }
    }

}
