package com.en_circle.slt.plugin.indentation;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.ui.console.SltConsole;
import com.intellij.codeInsight.editorActions.enter.EnterHandlerDelegate;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.*;
import com.intellij.openapi.editor.actionSystem.EditorActionHandler;
import com.intellij.openapi.editor.actionSystem.EditorActionManager;
import com.intellij.openapi.editor.ex.util.EditorUIUtil;
import com.intellij.openapi.editor.ex.util.LexerEditorHighlighter.InvalidStateException;
import com.intellij.openapi.editor.textarea.TextComponentEditor;
import com.intellij.openapi.util.Ref;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Objects;

public class SltIndentator implements EnterHandlerDelegate {

    @Override
    public Result preprocessEnter(@NotNull PsiFile file, @NotNull Editor editor, @NotNull Ref<Integer> caretOffset, @NotNull Ref<Integer> caretAdvance,
                                  @NotNull DataContext dataContext, @Nullable EditorActionHandler originalHandler) {
        if (file.getFileType().equals(SltCommonLispFileType.INSTANCE)) {
            if (!EditorModificationUtil.checkModificationAllowed(editor)) return Result.Continue;
            if (!ApplicationManager.getApplication().isWriteAccessAllowed() && !EditorModificationUtil.requestWriting(editor)) return Result.Continue;
            if (!SltIndentationSettings.getInstance(editor.getProject()).applies) return Result.Continue;

            PsiDocumentManager.getInstance(file.getProject()).commitDocument(editor.getDocument());
            Integer additionalOffset = calculateIndentOffset(file, editor);
            if (additionalOffset == null) {
                return Result.Continue;
            }
            indentDocument(editor, additionalOffset);
            return Result.Stop;
        }
        return Result.Continue;
    }

    @Override
    public Result postProcessEnter(@NotNull PsiFile file, @NotNull Editor editor, @NotNull DataContext dataContext) {
        if (file.getFileType().equals(SltCommonLispFileType.INSTANCE)) {
            return Result.Stop;
        }
        return Result.Continue;
    }

    private Integer calculateIndentOffset(PsiFile file, Editor editor) {
        int offset = editor.getCaretModel().getOffset();
        String packageOverride = getPackageOverride(editor.getContentComponent());

        SltIndentCalculator calculator = new SltIndentCalculator(editor.getProject());
        return calculator.calculateIndent(file, offset, editor.getDocument().getText(),
                packageOverride);
    }

    private String getPackageOverride(JComponent contentComponent) {
        while (contentComponent != null) {
            if (contentComponent.getClientProperty(SltConsole.KEY) != null) {
                return ((SltConsole) contentComponent.getClientProperty(SltConsole.KEY)).getPackage();
            }
            if (contentComponent.getParent() instanceof JComponent) {
                contentComponent = (JComponent) contentComponent.getParent();
            } else {
                return null;
            }
        }
        return null;
    }

    private void indentDocument(Editor editor, int additionalOffset) {
        DocumentRunnable runnable = new DocumentRunnable(editor.getDocument(), editor.getProject()) {
            @Override
            public void run() {
                final Document doc = editor.getDocument();

                doc.startGuardedBlockChecking();
                try {
                    EditorUIUtil.hideCursorInEditor(editor);
                    Document document = editor.getDocument();
                    if (!editor.isInsertMode()) {
                        int caretLine = editor.getCaretModel().getLogicalPosition().line;
                        int lineCount = document.getLineCount();
                        if (caretLine < lineCount) {
                            if (caretLine == lineCount - 1) {
                                document.insertString(document.getTextLength(), "\n");
                            }
                            LogicalPosition pos = new LogicalPosition(caretLine + 1, 0);
                            editor.getCaretModel().moveToLogicalPosition(pos);
                            editor.getSelectionModel().removeSelection();
                            EditorModificationUtil.scrollToCaret(editor);
                        }
                        return;
                    }
                    EditorModificationUtil.deleteSelectedText(editor);
                    int caretOffset = editor.getCaretModel().getOffset();
                    String s = "\n" + StringUtils.repeat(' ', additionalOffset);
                    try {
                        document.insertString(caretOffset, s);
                    } catch (InvalidStateException ignored) {
                        // lexer error is ignored
                    }
                    editor.getCaretModel().moveToOffset(caretOffset + s.length());
                    EditorModificationUtil.scrollToCaret(editor);
                    editor.getSelectionModel().removeSelection();
                    PsiDocumentManager.getInstance(Objects.requireNonNull(editor.getProject())).commitDocument(document);
                } catch (ReadOnlyFragmentModificationException e) {
                    EditorActionManager.getInstance().getReadonlyFragmentModificationHandler(doc).handle(e);
                } finally {
                    doc.stopGuardedBlockChecking();
                }
            }
        };
        if (editor instanceof TextComponentEditor) {
            runnable.run();
        } else {
            ApplicationManager.getApplication().runWriteAction(runnable);
        }
    }
}
