package com.en_circle.slt.plugin.actions.refactor;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.lisp.psi.*;
import com.en_circle.slt.plugin.lisp.psi.impl.LispPsiImplUtil;
import com.intellij.codeInsight.hint.HintManager;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.ui.popup.JBPopup;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.ReadonlyStatusHandler;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiParserFacade;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.ui.components.JBTextField;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.util.Objects;
import java.util.regex.Pattern;

public class ExtractFunctionLambda extends SltRefactorAction {
    private static final Logger log = LoggerFactory.getLogger(ExtractFunctionLambda.class);

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        EditorEx editor = (EditorEx) event.getData(CommonDataKeys.EDITOR);
        PsiElement element = null;
        if (editor != null) {
            if (!ReadonlyStatusHandler.ensureDocumentWritable(Objects.requireNonNull(editor.getProject()), editor.getDocument()))
                return;

            PsiDocumentManager psiMgr = PsiDocumentManager.getInstance(Objects.requireNonNull(editor.getProject()));
            psiMgr.commitDocument(editor.getDocument());
            PsiFile psiFile = psiMgr.getPsiFile(editor.getDocument());
            if (psiFile != null) {
                int caretOffset = editor.getExpectedCaretOffset();
                element = psiFile.findElementAt(caretOffset);
                while (element != null && !isLambda(element)) {
                    element = element.getParent();
                }
            }

            if (element == null) {
                ApplicationManager.getApplication().invokeLater(() -> HintManager.getInstance()
                        .showErrorHint(editor, SltBundle.message("slt.ui.refactor.extract.lambda.notinlambda")));
            } else {
                LispList list = (LispList) element;
                editor.getSelectionModel().setSelection(list.getTextOffset(), list.getTextOffset() + list.getTextLength());

                ApplicationManager.getApplication().invokeLater(() -> {
                    JBTextField name = new JBTextField();
                    JPanel root = new JPanel(new BorderLayout());
                    JButton save = new JButton(SltBundle.message("slt.ui.refactor.extract.lambda.refactor"));

                    root.add(name, BorderLayout.CENTER);
                    root.add(save, BorderLayout.EAST);

                    name.addActionListener(e -> save.doClick());

                    JBPopup popup = JBPopupFactory.getInstance().createComponentPopupBuilder(root, null)
                            .setProject(editor.getProject())
                            .setTitle(SltBundle.message("slt.ui.refactor.extract.lambda.title"))
                            .setShowBorder(false)
                            .setMovable(true)
                            .setFocusable(true)
                            .setRequestFocus(true)
                            .createPopup();
                    popup.showInBestPositionFor(editor.getDataContext());
                    name.requestFocus();

                    save.addActionListener(ev -> {
                        popup.cancel();
                        if (StringUtils.isBlank(name.getText())) {
                            HintManager.getInstance().showErrorHint(editor,
                                    SltBundle.message("slt.ui.refactor.extract.lambda.emptyname"));
                        } else {
                            int lineStart = editor.getDocument().getLineNumber(list.getTextOffset());
                            int offset = list.getTextOffset() - editor.getDocument().getLineStartOffset(lineStart);
                            CommandProcessor.getInstance().executeCommand(editor.getProject(),
                                    () -> ApplicationManager.getApplication().runWriteAction(() ->
                                            doRefactor(editor, editor.getDocument(), list, name.getText(), offset)),
                                    event.getPresentation().getText(), ExtractFunctionLambda.class.getName());
                        }
                    });
                });
            }
        }
    }

    private void doRefactor(Editor editor, Document document, LispList lambdaList, String functionName, int lineOffset) {
        StringBuilder newFunction = new StringBuilder();
        newFunction.append("(defun ");
        newFunction.append(functionName);
        newFunction.append(" ");
        if (lambdaList.getSexprList().size() > 1) {
            int offsetStart = lambdaList.getSexprList().get(0).getTextOffset() + lambdaList.getSexprList().get(0).getTextLength() + 1;
            int offsetEnd = offsetStart +
                    lambdaList.getTextLength() - lambdaList.getSexprList().get(1).getStartOffsetInParent();
            newFunction.append(document.getText(new TextRange(offsetStart, offsetEnd)));
        }
        String[] lines = newFunction.toString().split(Pattern.quote("\n"));
        StringBuilder moved = new StringBuilder();
        for (String line : lines) {
            moved.append(trimOffset(line, lineOffset));
            moved.append("\n");
        }
        moved.append("\n");

        PsiFile replacement = LispPsiImplUtil.createFile(editor.getProject(), moved.toString());
        if (!replacement.isValid()) {
            log.error("Replacement not valid");
            SwingUtilities.invokeLater(() -> HintManager.getInstance().showErrorHint(editor,
                    SltBundle.message("slt.ui.refactor.failed")));
            return;
        }

        LispToplevel toplevel = PsiTreeUtil.findChildOfType(replacement, LispToplevel.class);
        if (toplevel == null) {
            log.error("Toplevel not found");
            SwingUtilities.invokeLater(() -> HintManager.getInstance().showErrorHint(editor,
                    SltBundle.message("slt.ui.refactor.failed")));
            return;
        }

        LispToplevel currentTopLevel = PsiTreeUtil.getParentOfType(lambdaList, LispToplevel.class);
        if (currentTopLevel == null) {
            log.error("Current Toplevel not found");
            SwingUtilities.invokeLater(() -> HintManager.getInstance().showErrorHint(editor,
                    SltBundle.message("slt.ui.refactor.failed")));
            return;
        }

        currentTopLevel.getParent().addBefore(toplevel, currentTopLevel);
        currentTopLevel.getParent().addBefore(PsiParserFacade.getInstance(Objects.requireNonNull(editor.getProject()))
                .createWhiteSpaceFromText("\n\n"), currentTopLevel);

        PsiElement symbol = LispPsiImplUtil.createSymbol(editor.getProject(), functionName);

        LispSexpr sexpr = PsiTreeUtil.getParentOfType(lambdaList, LispSexpr.class);
        assert sexpr != null;
        for (LispEnhancement enhancement : sexpr.getEnhancementList()) {
            if (enhancement instanceof LispFunctionEnhancement) {
                lambdaList.replace(symbol);
                return;
            }
        }

        LispList parentList = PsiTreeUtil.getParentOfType(lambdaList, LispList.class);
        if (parentList == null) {
            // lambda was toplevel?
            lambdaList.replace(PsiParserFacade.getInstance(Objects.requireNonNull(editor.getProject()))
                    .createWhiteSpaceFromText(""));
        } else {
            if (parentList.getSexprList().get(0) == sexpr) {
                // we are first element, insert symbol
                lambdaList.replace(symbol);
            } else {
                PsiFile hashbanged = LispPsiImplUtil.createFile(editor.getProject(), "#'" + functionName);
                LispSexpr s = PsiTreeUtil.findChildOfType(hashbanged, LispSexpr.class);
                assert s != null;
                lambdaList.replace(s);
            }
        }
    }

    private String trimOffset(String line, int lineOffset) {
        int i=0;
        for (; i<lineOffset; i++) {
            if (line.length() < i)
                return line;
            if (line.charAt(i) != ' ') {
                break;
            }
        }
        return line.substring(i);
    }

    private boolean isLambda(PsiElement element) {
        if (element instanceof LispList list) {
            if (!list.getSexprList().isEmpty()) {
                LispSexpr head = list.getSexprList().get(0);
                return isLambdaSymbol(head);
            }
        }
        return false;
    }

    private boolean isLambdaSymbol(LispSexpr head) {
        if (head.getDatum() != null && head.getDatum().getCompoundSymbol() != null) {
            String headName = head.getText().toLowerCase();
            return "lambda".equals(headName);
        }
        return false;
    }


}
