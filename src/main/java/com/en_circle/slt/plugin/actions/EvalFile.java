package com.en_circle.slt.plugin.actions;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.intellij.ide.SaveAndSyncHandler;
import com.intellij.ide.SaveAndSyncHandler.SaveTask;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.FileContentUtilCore;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;

public class EvalFile extends EvalActionBase {

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        VirtualFile vf = event.getData(CommonDataKeys.VIRTUAL_FILE);
        if (vf != null) {
            SaveAndSyncHandler.getInstance().scheduleSave(new SaveTask(Objects.requireNonNull(event.getProject())));
            evaluateFile(event.getProject(), vf.getPath(), vf);
            for (VirtualFile openedFiles : FileEditorManager.getInstance(Objects.requireNonNull(event.getProject()))
                    .getOpenFiles()) {
                FileContentUtilCore.reparseFiles(openedFiles);
            }
        }
    }

    @Override
    public void update(@NotNull AnActionEvent event) {
        event.getPresentation().setEnabledAndVisible(false);
        if (event.getProject() != null) {
            VirtualFile vf = event.getData(CommonDataKeys.VIRTUAL_FILE);
            if (vf != null) {
                if (vf.getFileType().equals(SltCommonLispFileType.INSTANCE)) {
                    event.getPresentation().setEnabledAndVisible(true);
                }
            }
        }
    }
}
