package com.en_circle.slt.plugin.actions;

import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;

public abstract class EvalFormActionBase extends EvalActionBase {

    protected abstract PsiElement findList(PsiFile psiFile, int caretOffset, Document document);

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        EditorEx editor = (EditorEx) event.getData(CommonDataKeys.EDITOR);
        String forms = "";
        PsiElement list = null;

        if (editor != null) {
            PsiDocumentManager psiMgr = PsiDocumentManager.getInstance(Objects.requireNonNull(editor.getProject()));
            psiMgr.commitDocument(editor.getDocument());
            PsiFile psiFile = psiMgr.getPsiFile(editor.getDocument());
            if (psiFile != null) {
                int caretOffset = editor.getExpectedCaretOffset();
                list = findList(psiFile, caretOffset, editor.getDocument());
                if (list != null) {
                    forms = list.getText();
                    editor.getSelectionModel().setSelection(list.getTextOffset(), list.getTextOffset() + list.getTextLength());
                }
            }

            boolean hasEvalRegion = false;
            ProjectFileIndex index = ProjectFileIndex.getInstance(editor.getProject());
            VirtualFile vf = FileDocumentManager.getInstance().getFile(editor.getDocument());

            if (vf != null) {
                VirtualFile contentRoot = index.getContentRootForFile(vf);
                if (contentRoot != null && list != null) {
                    String filename = VfsUtil.getRelativePath(vf, contentRoot);
                    int offset = list.getTextOffset();
                    int lineno = editor.getDocument().getLineNumber(offset);
                    int charno = offset - editor.getDocument().getLineStartOffset(lineno);
                    hasEvalRegion = true;
                    evaluateRegion(event.getProject(), forms, filename, lineno, charno, () -> {});
                }
            }

            if (!hasEvalRegion) {
                evaluate(editor.getProject(), forms, () -> {});
            }
        }
    }

}
