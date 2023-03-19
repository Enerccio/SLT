package com.en_circle.slt.plugin.actions;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.en_circle.slt.plugin.services.SltProjectService;
import com.intellij.openapi.actionSystem.ActionUpdateThread;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;

public class OpenCLHSAction extends AnAction {

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        Editor editor = event.getData(CommonDataKeys.EDITOR);
        if (editor != null && event.getProject() != null) {
            PsiFile file = PsiDocumentManager.getInstance(Objects.requireNonNull(editor.getProject())).getPsiFile(editor.getDocument());
            if (file != null && SltCommonLispFileType.INSTANCE.equals(file.getFileType())) {
                PsiElement element = file.findElementAt(editor.getCaretModel().getOffset());
                LispSymbol symbol = null;
                if (element instanceof LispSymbol lispSymbol) {
                    symbol = lispSymbol;
                } else {
                    symbol = PsiTreeUtil.getParentOfType(element, LispSymbol.class);
                    if (symbol == null) {
                        symbol = PsiTreeUtil.getChildOfType(element, LispSymbol.class);
                    }
                }

                if (symbol != null) {
                    SltProjectService.getInstance(editor.getProject())
                            .showCLHSSymbol(Objects.requireNonNull(symbol.getName()).toLowerCase());
                }
            }
        }
    }

    @Override
    public void update(@NotNull AnActionEvent event) {
        event.getPresentation().setEnabledAndVisible(false);

        Editor editor = event.getData(CommonDataKeys.EDITOR);
        if (editor != null && event.getProject() != null) {
            PsiFile file = PsiDocumentManager.getInstance(Objects.requireNonNull(editor.getProject())).getPsiFile(editor.getDocument());
            if (file != null && SltCommonLispFileType.INSTANCE.equals(file.getFileType())) {
                event.getPresentation().setEnabledAndVisible(true);
            }
        }
    }

    @Override
    public @NotNull ActionUpdateThread getActionUpdateThread() {
        return ActionUpdateThread.BGT;
    }

}
