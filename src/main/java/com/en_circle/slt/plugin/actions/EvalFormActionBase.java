package com.en_circle.slt.plugin.actions;

import com.en_circle.slt.plugin.SltSBCL;
import com.en_circle.slt.plugin.swank.requests.SwankEvalRegion;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.ui.Messages;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

public abstract class EvalFormActionBase extends EvalActionBase {
    private static final Logger LOG = LoggerFactory.getLogger(EvalRegionAction.class);

    protected abstract PsiElement findList(PsiFile psiFile, int caretOffset, Document document);

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        String forms = "";
        EditorEx editor = (EditorEx) event.getData(CommonDataKeys.EDITOR);
        if (editor != null) {
            PsiDocumentManager psiMgr = PsiDocumentManager.getInstance(Objects.requireNonNull(editor.getProject()));
            psiMgr.commitDocument(editor.getDocument());
            PsiFile psiFile = psiMgr.getPsiFile(editor.getDocument());
            if (psiFile != null) {
                int caretOffset = editor.getExpectedCaretOffset();
                PsiElement list = findList(psiFile, caretOffset, editor.getDocument());
                if (list != null) {
                    forms = list.getText();
                    editor.getSelectionModel().setSelection(list.getTextOffset(), list.getTextOffset() + list.getTextLength());
                }
            }
        }

        try {

            if (StringUtils.isNotBlank(forms)) {
                SltSBCL.getInstance().sendToSbcl(SwankEvalRegion.eval(forms, returnValue -> {
                    // ignored
                }));
            }
        } catch (Exception e) {
            LOG.warn("Error starting sbcl", e);
            Messages.showErrorDialog(event.getProject(), e.getMessage(), "Failed to Start SBCL");
        }
    }

}
