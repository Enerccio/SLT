package com.en_circle.slt.plugin.indentation;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.lisp.psi.LispToplevel;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.ui.console.SltConsole;
import com.intellij.codeInsight.editorActions.enter.EnterHandlerDelegate;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.*;
import com.intellij.openapi.editor.actionSystem.EditorActionHandler;
import com.intellij.openapi.editor.actionSystem.EditorActionManager;
import com.intellij.openapi.editor.ex.util.EditorUIUtil;
import com.intellij.openapi.editor.textarea.TextComponentEditor;
import com.intellij.openapi.util.Ref;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
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
        boolean wasAfter = false;
        int offset = editor.getCaretModel().getOffset();

        PsiElement element = file.findElementAt(offset);
        while (element == null) {
            element = file.findElementAt(offset--);
            wasAfter = true;
            if (offset == 0) {
                return 0;
            }
        }

        while (element instanceof PsiWhiteSpace) {
            element = file.findElementAt(offset--);
            wasAfter = true;
            if (offset == 0) {
                return 0;
            }
        }

        if (element == null) {
            return null;
        }

        if (element.getNode().getElementType() == LispTypes.LPAREN) {
            // left parenthesis means we need to check if we are toplevel last next element
            // in which case we are moving to new line
            if (element.getParent() == file) {
                // incomplete
                PsiElement previousToplevel = element.getPrevSibling();
                while (!(previousToplevel instanceof LispToplevel)) {
                    if (previousToplevel == null)
                        break;
                    previousToplevel = previousToplevel.getPrevSibling();
                }
                if (previousToplevel == null)
                    return null;
                if (previousToplevel.getNextSibling() == element ||
                        PsiTreeUtil.firstChild(previousToplevel.getNextSibling()) == element) {
                    // we are first ( of next toplevel
                    return 0;
                }
            } else {
                PsiElement topLevel = PsiTreeUtil.getParentOfType(element, LispToplevel.class);
                PsiManager manager = PsiManager.getInstance(element.getProject());
                if (topLevel != null) {
                    if (manager.areElementsEquivalent(topLevel.getFirstChild(), element)
                            || manager.areElementsEquivalent(PsiTreeUtil.firstChild(topLevel), element)) {
                        // we are first ( of next toplevel
                        return 0;
                    }
                }
            }
        }

        if (element.getNode().getElementType() == LispTypes.BLOCK_COMMENT) {
            return null;
        }

        String packageOverride = getPackageOverride(editor.getContentComponent());

        return LispEnvironmentService.getInstance(Objects.requireNonNull(editor.getProject()))
                .calculateOffset(element, file, wasAfter, editor.getDocument().getText(), editor.getCaretModel().getOffset(),
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
                    document.insertString(caretOffset, s);
                    editor.getCaretModel().moveToOffset(caretOffset + s.length());
                    EditorModificationUtil.scrollToCaret(editor);
                    editor.getSelectionModel().removeSelection();
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
