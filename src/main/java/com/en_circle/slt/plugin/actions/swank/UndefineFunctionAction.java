package com.en_circle.slt.plugin.actions.swank;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentState;
import com.en_circle.slt.plugin.swank.requests.UndefineFunction;
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

public class UndefineFunctionAction extends SltSwankAction {
    private static final Logger log = LoggerFactory.getLogger(UndefineFunctionAction.class);

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        EditorEx editor = (EditorEx) event.getData(CommonDataKeys.EDITOR);
        PsiElement element = null;
        if (editor != null) {
            PsiDocumentManager psiMgr = PsiDocumentManager.getInstance(Objects.requireNonNull(editor.getProject()));
            psiMgr.commitDocument(editor.getDocument());
            PsiFile psiFile = psiMgr.getPsiFile(editor.getDocument());
            if (psiFile != null) {
                int caretOffset = editor.getExpectedCaretOffset();
                element = psiFile.findElementAt(caretOffset);
                while (element != null && !isSymbol(element)) {
                    element = psiFile.findElementAt(--caretOffset);
                }
            }

            if (element == null) {
                ApplicationManager.getApplication().invokeLater(() -> HintManager.getInstance()
                        .showErrorHint(editor, SltBundle.message("slt.ui.swank.undefinefunction.notsymbol")));
                return;
            }

            String packageName = LispParserUtil.getPackage(element);
            String symbolname = element.getText();
            if (!symbolname.contains(":")) {
                // assume it's in current package
                symbolname = packageName + "::" + symbolname;
            }

            try {
                LispEnvironmentService.getInstance(editor.getProject())
                        .sendToLisp(UndefineFunction.undefineFunction(symbolname, packageName), false);
            } catch (Exception e) {
                log.warn(SltBundle.message("slt.error.start"), e);
                Messages.showErrorDialog(editor.getProject(), e.getMessage(), SltBundle.message("slt.ui.errors.lisp.start"));
            }
        }
    }

    private boolean isSymbol(PsiElement element) {
        return PsiTreeUtil.getParentOfType(element, LispSymbol.class, false) != null;
    }

    @Override
    protected boolean innerActionCheck(Editor editor, PsiFile file) {
        return LispEnvironmentService.getInstance(file.getProject())
                .getState() == LispEnvironmentState.READY;
    }
}
