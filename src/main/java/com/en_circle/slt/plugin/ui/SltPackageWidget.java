package com.en_circle.slt.plugin.ui;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentState;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.EditorFactory;
import com.intellij.openapi.editor.event.*;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.fileEditor.FileEditorManagerEvent;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.NlsContexts.Label;
import com.intellij.openapi.util.NlsContexts.Tooltip;
import com.intellij.openapi.wm.StatusBar;
import com.intellij.openapi.wm.StatusBarWidget;
import com.intellij.openapi.wm.impl.status.EditorBasedWidget;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.util.Alarm;
import com.intellij.util.Consumer;
import com.intellij.util.ui.update.MergingUpdateQueue;
import com.intellij.util.ui.update.Update;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.awt.*;
import java.awt.event.MouseEvent;

public class SltPackageWidget extends EditorBasedWidget
        implements StatusBarWidget.TextPresentation, CaretListener, BulkAwareDocumentListener.Simple,
        SelectionListener, StatusBarWidget.Multiframe {

    public static final Key<Object> DISABLE_FOR_EDITOR = new Key<>("positionPanel.disableForEditor");

    private Alarm alarm;
    private MergingUpdateQueue queue;
    private String packageName;
    private StatusBar statusBar;

    public SltPackageWidget(@NotNull Project project) {
        super(project);
    }

    @Override
    public @NonNls @NotNull String ID() {
        return SltPackageWidget.class.getSimpleName();
    }

    @Override
    public StatusBarWidget copy() {
        return new SltPackageWidget(getProject());
    }

    @Override
    public WidgetPresentation getPresentation() {
        return this;
    }

    @Override
    public @NotNull @Label String getText() {
        String currentPackage = getPackage();
        return SltBundle.message("slt.ui.statusbar.info") + " " + currentPackage;
    }

    private String getPackage() {
        if (packageName == null) {
            if (LispEnvironmentService.getInstance(myProject).getState() == LispEnvironmentState.READY) {
                return LispEnvironmentService.getInstance(myProject).getGlobalPackage();
            }
            return "CL-USER";
        }
        return packageName;
    }

    @Override
    public float getAlignment() {
        return Component.CENTER_ALIGNMENT;
    }

    @Override
    public @Nullable @Tooltip String getTooltipText() {
        return null;
    }

    @Override
    public @Nullable Consumer<MouseEvent> getClickConsumer() {
        return null;
    }

    @Override
    public void install(@NotNull StatusBar statusBar) {
        super.install(statusBar);
        alarm = new Alarm(Alarm.ThreadToUse.POOLED_THREAD, this);
        queue = new MergingUpdateQueue("CommonLispPackage", 100, true, null, this);
        EditorEventMulticaster multicaster = EditorFactory.getInstance().getEventMulticaster();
        multicaster.addCaretListener(this, this);
        multicaster.addSelectionListener(this, this);
        multicaster.addDocumentListener(this, this);
        this.statusBar = statusBar;
    }

    @Override
    public void selectionChanged(@NotNull final SelectionEvent e) {
        Editor editor = e.getEditor();
        if (isFocusedEditor(editor))
            updatePosition(editor);
    }

    @Override
    public void caretPositionChanged(@NotNull final CaretEvent e) {
        Editor editor = e.getEditor();
        // When multiple carets exist in editor, we don't show information about caret positions
        if (editor.getCaretModel().getCaretCount() == 1 && isFocusedEditor(editor)) updatePosition(editor);
    }

    @Override
    public void caretAdded(@NotNull CaretEvent e) {
        updatePosition(e.getEditor());
    }

    @Override
    public void caretRemoved(@NotNull CaretEvent e) {
        updatePosition(e.getEditor());
    }

    @Override
    public void selectionChanged(@NotNull FileEditorManagerEvent event) {
        updatePosition(getEditor());
    }

    @Override
    public void afterDocumentChange(@NotNull Document document) {
        EditorFactory.getInstance().editors(document)
                .filter(this::isFocusedEditor)
                .findFirst()
                .ifPresent(this::updatePosition);
    }

    private boolean isFocusedEditor(Editor editor) {
        return isOurEditor(editor);
    }

    private void updatePosition(Editor editor) {
        queue.queue(Update.create(this, () -> {
            boolean empty = editor == null || DISABLE_FOR_EDITOR.isIn(editor);
            if (empty || !isOurEditor(editor)) return;
            if (!(editor instanceof EditorEx ex)) return;

            packageName = null;
            Document document = editor.getDocument();
            PsiFile psiFile = PsiDocumentManager.getInstance(myProject).getPsiFile(document);
            if (psiFile != null) {
                if (psiFile.getFileType() == SltCommonLispFileType.INSTANCE) {
                    int caretOffset = ex.getExpectedCaretOffset();
                    packageName = LispParserUtil.getPackage(psiFile, caretOffset, LispParserUtil.NULL_RETURN);
                }
            }
            if (statusBar != null) {
                statusBar.updateWidget(ID());
            }
        }));
    }

}
