package com.en_circle.slt.plugin.actions;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltLispEnvironmentProvider;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.editor.Caret;
import com.intellij.openapi.editor.CaretModel;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class EvalRegionAction extends EvalActionBase {

    @Override
    public void update(@NotNull AnActionEvent event) {
        super.update(event);

        event.getPresentation().setText(SltBundle.message("slt.actions.eval.region"));
    }

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
                    String filename = vf.getPath();
                    CaretModel caretModel = editor.getCaretModel();
                    List<Caret> carets = caretModel.getAllCarets();
                    hasEvalRegion = true;
                    evalEachCaret(editor, filename, carets);
                }
            }

            if (!hasEvalRegion) {
                String selectedText = editor.getSelectionModel().getSelectedText(false);
                PsiDocumentManager psiMgr = PsiDocumentManager.getInstance(Objects.requireNonNull(editor.getProject()));
                psiMgr.commitDocument(editor.getDocument());
                PsiFile psiFile = psiMgr.getPsiFile(editor.getDocument());

                if (psiFile != null) {
                    int offset = editor.getSelectionModel().getSelectionStart();
                    evaluate(editor.getProject(), selectedText, LispParserUtil.getPackage(psiFile, offset), () -> { });
                } else {
                    evaluate(editor.getProject(), selectedText, SltLispEnvironmentProvider.getInstance().getGlobalPackage(), () -> { });
                }
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
                PsiDocumentManager psiMgr = PsiDocumentManager.getInstance(Objects.requireNonNull(editor.getProject()));
                psiMgr.commitDocument(editor.getDocument());
                PsiFile psiFile = psiMgr.getPsiFile(editor.getDocument());
                String packageName = SltLispEnvironmentProvider.getInstance().getGlobalPackage();
                if (psiFile != null) {
                    packageName = LispParserUtil.getPackage(psiFile, offset);
                }
                evaluateRegion(editor.getProject(), text, packageName, filename, offset, lineno, charno, continueEvaluation);
            }
        }
    }

}
