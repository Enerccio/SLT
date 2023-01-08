package com.en_circle.slt.plugin.actions;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.SltSBCL;
import com.en_circle.slt.plugin.swank.requests.SwankEvalRegion;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.ui.Messages;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

public class EvalPreviousFormAction extends AnAction {
    private static final Logger LOG = LoggerFactory.getLogger(EvalPreviousFormAction.class);

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        try {
            Editor editor = event.getData(CommonDataKeys.EDITOR);
            if (editor != null) {
                String selectedText = editor.getSelectionModel().getSelectedText(true);
                if (StringUtils.isNotBlank(selectedText)) {
                    SltSBCL.getInstance().sendToSbcl(SwankEvalRegion.eval(selectedText, returnValue -> {
                        // ignored
                    }));
                }
            }
        } catch (Exception e) {
            LOG.warn("Error starting sbcl", e);
            Messages.showErrorDialog(event.getProject(), e.getMessage(), "Failed to Start SBCL");
        }
    }

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
}
