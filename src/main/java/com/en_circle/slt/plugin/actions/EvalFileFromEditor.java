package com.en_circle.slt.plugin.actions;

import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;

public class EvalFileFromEditor extends EvalActionBase {

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        EditorEx editor = (EditorEx) event.getData(CommonDataKeys.EDITOR);
        if (editor != null) {
            VirtualFile vf = FileDocumentManager.getInstance().getFile(editor.getDocument());
            if (vf != null) {
                evaluateFile(editor.getProject(), vf.getPath());
            }
        }
    }
}
