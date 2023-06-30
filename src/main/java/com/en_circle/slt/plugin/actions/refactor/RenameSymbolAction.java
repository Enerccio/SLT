package com.en_circle.slt.plugin.actions.refactor;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.lisp.LispLexerAdapter;
import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.en_circle.slt.plugin.lisp.psi.LispToplevel;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.en_circle.slt.plugin.lisp.psi.impl.LispPsiImplUtil;
import com.intellij.codeInsight.hint.HintManager;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.ui.popup.JBPopup;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.vfs.ReadonlyStatusHandler;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.ui.components.JBTextField;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.util.Objects;

public class RenameSymbolAction extends SltRefactorAction {
    private static final Logger log = LoggerFactory.getLogger(RenameSymbolAction.class);

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
                element = PsiTreeUtil.getParentOfType(psiFile.findElementAt(caretOffset), LispSymbol.class);

                while (element == null && caretOffset >= 0) {
                    element = psiFile.findElementAt(--caretOffset);
                    if (element != null)
                        element = PsiTreeUtil.getParentOfType(psiFile.findElementAt(caretOffset), LispSymbol.class);
                }
            }

            if (element == null) {
                ApplicationManager.getApplication().invokeLater(() -> HintManager.getInstance()
                        .showErrorHint(editor, SltBundle.message("slt.ui.refactor.renamesymbol.notinsymbol")));
            } else {
                LispSymbol selectedSymbol = (LispSymbol) element;
                LispToplevel toplevel = PsiTreeUtil.getTopmostParentOfType(selectedSymbol, LispToplevel.class);
                assert toplevel != null;

                editor.getSelectionModel().setSelection(selectedSymbol.getTextOffset(),
                        selectedSymbol.getTextOffset() + selectedSymbol.getTextLength());

                ApplicationManager.getApplication().invokeLater(() -> {
                    JBTextField name = new JBTextField();
                    JPanel root = new JPanel(new BorderLayout());
                    JButton save = new JButton(SltBundle.message("slt.ui.refactor.renamesymbol.refactor"));

                    root.add(name, BorderLayout.CENTER);
                    root.add(save, BorderLayout.EAST);

                    name.addActionListener(e -> save.doClick());

                    JBPopup popup = JBPopupFactory.getInstance().createComponentPopupBuilder(root, null)
                            .setProject(editor.getProject())
                            .setTitle(SltBundle.message("slt.ui.refactor.renamesymbol.title"))
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
                                    SltBundle.message("slt.ui.refactor.renamesymbol.emptyname"));
                        } else {
                            String symbolName = reformatSymbolName(name.getText());
                            CommandProcessor.getInstance().executeCommand(editor.getProject(),
                                    () -> ApplicationManager.getApplication().runWriteAction(() ->
                                            doRefactor(editor, toplevel, selectedSymbol, symbolName)),
                                    event.getPresentation().getText(), RenameSymbolAction.class.getName());
                        }
                    });
                });
            }
        }
    }

    private String reformatSymbolName(String name) {
        LispLexerAdapter adapter = new LispLexerAdapter();
        adapter.start(name);
        if (adapter.getTokenType() == LispTypes.SYMBOL_TOKEN) {
            return name;
        } else {
            return "|" + StringUtils.replace(name, "|", "\\|") + "|";
        }
    }

    private void doRefactor(Editor editor, LispToplevel lispToplevel, LispSymbol originalSymbol, String symbolName) {
        LispSymbol s = LispPsiImplUtil.createSymbol(editor.getProject(), symbolName);
        String originalSymbolName = originalSymbol.getName();
        assert originalSymbolName != null;

        for (LispSymbol symbol : PsiTreeUtil.findChildrenOfType(lispToplevel, LispSymbol.class)) {
            if (originalSymbolName.equals(symbol.getName()))
                symbol.replace(s);
        }
    }

}
