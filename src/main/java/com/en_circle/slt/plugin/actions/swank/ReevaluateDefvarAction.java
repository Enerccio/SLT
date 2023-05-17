package com.en_circle.slt.plugin.actions.swank;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.LispParserUtil.SexpressionType;
import com.en_circle.slt.plugin.lisp.psi.LispToplevel;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentState;
import com.en_circle.slt.plugin.swank.requests.ReevaluateDefvar;
import com.intellij.codeInsight.hint.HintManager;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.ui.Messages;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

public class ReevaluateDefvarAction extends SltSwankAction {
    private static final Logger log = LoggerFactory.getLogger(ReevaluateDefvarAction.class);

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        EditorEx editor = (EditorEx) event.getData(CommonDataKeys.EDITOR);
        if (editor != null) {
            PsiDocumentManager psiMgr = PsiDocumentManager.getInstance(Objects.requireNonNull(editor.getProject()));
            psiMgr.commitDocument(editor.getDocument());
            PsiFile psiFile = psiMgr.getPsiFile(editor.getDocument());
            assert psiFile != null;

            int caretOffset = editor.getExpectedCaretOffset();
            PsiElement element = psiFile.findElementAt(caretOffset);
            while (element != null) {
                element = PsiTreeUtil.getParentOfType(element, LispToplevel.class, false);
                if (element == null) {
                    element = psiFile.findElementAt(--caretOffset);
                } else {
                    break;
                }
            }

            if (element == null)
                return;

            if (LispParserUtil.determineTopLevelType(((LispToplevel) element).getSexpr())
                    .getType() != SexpressionType.DEFVAR) {
                ApplicationManager.getApplication().invokeLater(() -> HintManager.getInstance()
                        .showErrorHint(editor, SltBundle.message("slt.ui.swank.reevaluatedefvar.notdefvar")));
                return;
            }

            try {
                LispEnvironmentService.getInstance(editor.getProject())
                        .sendToLisp(ReevaluateDefvar.reevaluateDefvar(element.getText()), false);
            } catch (Exception e) {
                log.warn(SltBundle.message("slt.error.start"), e);
                Messages.showErrorDialog(editor.getProject(), e.getMessage(), SltBundle.message("slt.ui.errors.lisp.start"));
            }
        }

    }

    @Override
    protected boolean innerActionCheck(Editor e, PsiFile psiFile) {
        return LispEnvironmentService.getInstance(psiFile.getProject())
                .getState() == LispEnvironmentState.READY;
    }

}
