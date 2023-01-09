package com.en_circle.slt.plugin.ui.console;

import com.intellij.ide.DataManager;
import com.intellij.openapi.actionSystem.IdeActions;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.LogicalPosition;
import com.intellij.openapi.editor.actionSystem.EditorActionHandler;
import com.intellij.openapi.editor.actionSystem.EditorActionManager;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiDocumentManager;

public class SltConsoleEnterHandler {

    public boolean handleEnterPressed(EditorEx editor) {
        Project project = editor.getProject();
        assert(project != null);
        int lineCount = editor.getDocument().getLineCount();
        if (lineCount > 0) {
            editor.getSelectionModel().removeSelection();
            LogicalPosition caretPosition = editor.getCaretModel().getLogicalPosition();
            if (caretPosition.line == lineCount - 1) {
                int lineEndOffset = editor.getDocument().getLineEndOffset(caretPosition.line);
                editor.getCaretModel().moveToOffset(lineEndOffset);
            } else {
                executeEnterHandler(project, editor);
                return false;
            }
        } else {
            return true;
        }

        PsiDocumentManager psiMgr = PsiDocumentManager.getInstance(project);
        psiMgr.commitDocument(editor.getDocument());

        int caretOffset = editor.getExpectedCaretOffset();
        // TODO: more sophistication?
        boolean isAtTheEndOfCommand = editor.getDocument().getLineNumber(caretOffset) == editor.getDocument().getLineCount() - 1;
        String prevLine = getLineAtOffset(editor.getDocument(), caretOffset);

        executeEnterHandler(project, editor);
        return isAtTheEndOfCommand && prevLine.isBlank();
    }

    private void executeEnterHandler(Project project, EditorEx editor) {
        EditorActionHandler enterHandler = EditorActionManager.getInstance().getActionHandler(IdeActions.ACTION_EDITOR_ENTER);
        WriteCommandAction.runWriteCommandAction(project, () -> {
            enterHandler.execute(editor, null, DataManager.getInstance().getDataContext(editor.getComponent()));
        });
    }

    private String getLineAtOffset(Document doc, int offset) {
        int line = doc.getLineNumber(offset);
        int start = doc.getLineStartOffset(line);
        int end = doc.getLineEndOffset(line);
        return doc.getText(new TextRange(start, end));
    }

}
