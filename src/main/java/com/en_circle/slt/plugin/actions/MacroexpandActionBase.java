package com.en_circle.slt.plugin.actions;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.environment.LispFeatures;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.psi.LispList;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.intellij.openapi.actionSystem.ActionUpdateThread;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.popup.JBPopup;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.util.text.HtmlBuilder;
import com.intellij.openapi.util.text.HtmlChunk;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.text.html.HTMLEditorKit;
import java.awt.*;
import java.util.Objects;

public abstract class MacroexpandActionBase extends AnAction {

    protected abstract void macroexpand(Project project, LispList form, String packageName, MacroexpandCallback callback);

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        EditorEx editor = (EditorEx) event.getData(CommonDataKeys.EDITOR);
        LispList list = null;

        if (editor != null) {
            PsiDocumentManager psiMgr = PsiDocumentManager.getInstance(Objects.requireNonNull(editor.getProject()));
            psiMgr.commitDocument(editor.getDocument());
            PsiFile psiFile = psiMgr.getPsiFile(editor.getDocument());
            if (psiFile != null) {
                int caretOffset = editor.getExpectedCaretOffset();
                list = findList(psiFile, caretOffset, editor.getDocument());
                if (list != null) {
                    editor.getSelectionModel().setSelection(list.getTextOffset(), list.getTextOffset() + list.getTextLength());
                }
            }

            if (list != null) {
                int offset = list.getTextOffset();
                String packageName = LispParserUtil.getPackage(psiFile, offset);
                macroexpand(editor.getProject(), list, packageName, text -> SwingUtilities.invokeLater(() -> {
                    HtmlBuilder builder = new HtmlBuilder();
                    String macroExpand = StringUtils.replace(StringUtils.replace(StringEscapeUtils.escapeHtml4(text), " ", "&nbsp;"),
                            "\n", HtmlChunk.br().toString());
                    builder.append(HtmlChunk.raw(macroExpand));

                    JEditorPane editorPane = new JEditorPane("text/html", "");
                    editorPane.setEditorKit(new HTMLEditorKit());
                    editorPane.setText(builder.toString());

                    JPanel textPanel = new JPanel(new BorderLayout());
                    textPanel.add(editorPane, BorderLayout.CENTER);

                    JBPopup popup = JBPopupFactory.getInstance()
                            .createComponentPopupBuilder(textPanel, null)
                            .setProject(editor.getProject())
                            .setTitle(SltBundle.message("slt.ui.macroexpand.title"))
                            .setShowBorder(true)
                            .setMovable(true)
                            .setFocusable(true)
                            .setRequestFocus(true)
                            .createPopup();
                    popup.showInBestPositionFor(editor.getDataContext());
                }));
            }
        }
    }

    protected LispList findList(PsiFile psiFile, int caretOffset, Document document) {
        PsiElement element = psiFile.findElementAt(caretOffset);
        if (element != null) {
            PsiElement parent = element;
            while (parent != null) {
                parent = parent.getParent();
                if (parent instanceof LispList) {
                    return (LispList) parent;
                }
            }
        }
        return null;
    }

    @Override
    public void update(@NotNull AnActionEvent event) {
        event.getPresentation().setEnabledAndVisible(false);
        Editor editor = event.getData(CommonDataKeys.EDITOR);
        if (editor != null && event.getProject() != null) {
            PsiFile file = PsiDocumentManager.getInstance(Objects.requireNonNull(editor.getProject())).getPsiFile(editor.getDocument());
            if (file != null && SltCommonLispFileType.INSTANCE.equals(file.getFileType())) {
                event.getPresentation().setEnabledAndVisible(LispEnvironmentService.getInstance(event.getProject())
                        .hasFeature(LispFeatures.MACROEXPAND));
            }
        }
    }

    @Override
    public @NotNull ActionUpdateThread getActionUpdateThread() {
        return ActionUpdateThread.BGT;
    }

    protected interface MacroexpandCallback {

        void showMacroexpand(String text);

    }
}
