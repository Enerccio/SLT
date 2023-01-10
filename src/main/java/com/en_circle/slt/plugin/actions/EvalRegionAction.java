package com.en_circle.slt.plugin.actions;

import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.editor.Caret;
import com.intellij.openapi.editor.CaretModel;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public class EvalRegionAction extends EvalActionBase {

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        Editor editor = event.getData(CommonDataKeys.EDITOR);
        boolean hasEvalRegion = false;

        if (editor != null && editor.getProject() != null) {
            ProjectFileIndex index = ProjectFileIndex.getInstance(editor.getProject());
            VirtualFile vf = FileDocumentManager.getInstance().getFile(editor.getDocument());

            if (vf != null) {
                VirtualFile contentRoot = index.getContentRootForFile(vf);
                if (contentRoot != null) {
                    String filename = VfsUtil.getRelativePath(vf, contentRoot);
                    CaretModel caretModel = editor.getCaretModel();
                    List<Caret> carets = caretModel.getAllCarets();
                    hasEvalRegion = true;
                    evalEachCaret(editor, filename, carets);
                }
            }

            if (!hasEvalRegion) {
                String selectedText = editor.getSelectionModel().getSelectedText(false);
                evaluate(editor.getProject(), selectedText, () -> {});
            }
        }
    }

    private void evalEachCaret(Editor editor, String filename, List<Caret> carets) {
        if (carets.size() > 0) {
            Caret caret = carets.get(0);
            carets = carets.subList(1, carets.size());
            List<Caret> cl = new ArrayList<>(carets);
            Runnable continueEvaluation = () -> {
                evalEachCaret(editor, filename, cl);
            };

            String text = caret.getSelectedText();
            if (StringUtils.isBlank(text)) {
                continueEvaluation.run();
            } else {
                int offset = caret.getOffset();
                int lineno = editor.getDocument().getLineNumber(offset);
                int charno = offset - editor.getDocument().getLineStartOffset(lineno);
                evaluateRegion(editor.getProject(), text, filename, lineno, charno,continueEvaluation);
            }
        }
    }

}
