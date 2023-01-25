package com.en_circle.slt.plugin.actions;

import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;

public class EvalFileFromEditor extends EvalActionBase {

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        EditorEx editor = (EditorEx) event.getData(CommonDataKeys.EDITOR);
        if (editor != null) {
            PsiDocumentManager psiMgr = PsiDocumentManager.getInstance(Objects.requireNonNull(editor.getProject()));
            psiMgr.commitDocument(editor.getDocument());
            FileDocumentManager.getInstance().saveDocument(editor.getDocument());

            VirtualFile vf = FileDocumentManager.getInstance().getFile(editor.getDocument());
            if (vf != null) {
                evaluateFile(editor.getProject(), vf.getPath(), vf);
            }
        }
    }
}
