package com.en_circle.slt.plugin.ui.console;

import com.intellij.execution.console.LanguageConsoleView;
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
import org.apache.commons.lang3.StringUtils;

public class SltConsoleEnterHandler {

    private final LanguageConsoleView console;
    private final SltConsole parent;

    public SltConsoleEnterHandler(LanguageConsoleView console, SltConsole parent) {
        this.console = console;
        this.parent = parent;
    }

    public boolean handleEnterPressed(EditorEx editor) {
        Project project = editor.getProject();
        assert(project != null);
        boolean isAtTheEndOfCommand;

        int lineCount = editor.getDocument().getLineCount();
        if (lineCount > 0) {
            editor.getSelectionModel().removeSelection();
            LogicalPosition caretPosition = editor.getCaretModel().getLogicalPosition();
            if (caretPosition.line == lineCount - 1) {
                int lineEndOffset = editor.getDocument().getLineEndOffset(caretPosition.line);
                editor.getCaretModel().moveToOffset(lineEndOffset);
                isAtTheEndOfCommand = true;
            } else {
                // try checking other lines if they are empty
                boolean allEmpty = true;
                for (int i=caretPosition.line+1; i<editor.getDocument().getLineCount(); i++) {
                    int startLine = editor.getDocument().getLineStartOffset(i);
                    int endLine = editor.getDocument().getLineEndOffset(i);
                    String lineValue = editor.getDocument().getText(new TextRange(startLine, endLine));
                    if (StringUtils.isNotBlank(lineValue)) {
                        allEmpty = false;
                        break;
                    }
                }
                if (!allEmpty) {
                    executeEnterHandler(project, editor);
                    return false;
                } else {
                    isAtTheEndOfCommand = true;
                }
            }
        } else {
            return true;
        }

        PsiDocumentManager psiMgr = PsiDocumentManager.getInstance(project);
        psiMgr.commitDocument(editor.getDocument());

        int caretOffset = editor.getExpectedCaretOffset();
        // TODO: more sophistication?
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
