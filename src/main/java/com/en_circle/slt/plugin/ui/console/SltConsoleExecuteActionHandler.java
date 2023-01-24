package com.en_circle.slt.plugin.ui.console;

import com.intellij.execution.console.BaseConsoleExecuteActionHandler;
import com.intellij.execution.console.LanguageConsoleView;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.RangeMarker;
import org.jetbrains.annotations.NotNull;

public abstract class SltConsoleExecuteActionHandler extends BaseConsoleExecuteActionHandler  {

    private final SltConsoleEnterHandler enterHandler;

    public SltConsoleExecuteActionHandler(LanguageConsoleView consoleView, SltConsole parent) {
        super(true);
        enterHandler = new SltConsoleEnterHandler(consoleView, parent);
    }

    protected abstract void execute(String code);
    @Override
    public void runExecuteAction(@NotNull LanguageConsoleView consoleView) {
        Document doc = consoleView.getEditorDocument();
        RangeMarker endMarker = doc.createRangeMarker(doc.getTextLength(), doc.getTextLength());
        endMarker.setGreedyToRight(true);
        endMarker.setGreedyToLeft(false);

        boolean isComplete = enterHandler.handleEnterPressed(consoleView.getConsoleEditor());

        if (isComplete) {
            deleteString(doc, endMarker);
            copyToHistoryAndExecute(consoleView);
        }
    }

    private void copyToHistoryAndExecute(LanguageConsoleView consoleView) {
        super.runExecuteAction(consoleView);
    }

    private void deleteString(Document document, RangeMarker endMarker) {
        if (endMarker.getEndOffset() - endMarker.getStartOffset() > 0) {
            ApplicationManager.getApplication().runWriteAction(() -> {
                CommandProcessor.getInstance().runUndoTransparentAction(() -> {
                    document.deleteString(endMarker.getStartOffset(), endMarker.getEndOffset());
                });
            });
        }
    }

    @Override
    protected void execute(@NotNull String text, @NotNull LanguageConsoleView console) {
        execute(text);
    }
}
