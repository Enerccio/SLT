package com.en_circle.slt.plugin.ui.debug;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltLispEnvironmentProvider;
import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.swank.debug.SltInspectedObject;
import com.en_circle.slt.plugin.swank.debug.SltInspectedObject.SltInspectionElement;
import com.en_circle.slt.plugin.swank.requests.SltInspectFrameVar;
import com.en_circle.slt.plugin.swank.requests.SltInspectNth;
import com.en_circle.slt.plugin.swank.requests.SltInspectorAction;
import com.en_circle.slt.plugin.swank.requests.SltInspectorAction.ActionType;
import com.en_circle.slt.plugin.ui.debug.SltFrameInfo.Local;
import com.intellij.icons.AllIcons.Actions;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.util.text.HtmlBuilder;
import com.intellij.openapi.util.text.HtmlChunk;
import com.intellij.ui.components.JBScrollPane;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.event.HyperlinkEvent.EventType;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.html.HTMLEditorKit;
import java.awt.*;
import java.math.BigInteger;
import java.util.regex.Pattern;

public class SltInspector {
    private static final Logger log = LoggerFactory.getLogger(SltInspector.class);

    private final Project project;
    private final JPanel content;
    private final BigInteger threadId;
    private final JEditorPane inspectorGeneratedContents;

    public SltInspector(Project project, BigInteger threadId) {
        this.project = project;
        this.threadId = threadId;

        content = new JPanel(new BorderLayout());
        DefaultActionGroup controlGroup = new DefaultActionGroup();
        controlGroup.add(new GoBackAction());
        controlGroup.add(new GoForwardAction());
        controlGroup.add(new RefreshAction());
        ActionToolbar toolbar = ActionManager.getInstance()
                .createActionToolbar(SltInspector.class.getName(), controlGroup, true);
        toolbar.setTargetComponent(content);
        content.add(toolbar.getComponent(), BorderLayout.NORTH);

        inspectorGeneratedContents = new JEditorPane("text/html", "");
        inspectorGeneratedContents.setEditorKit(new HTMLEditorKit());
        inspectorGeneratedContents.setEditable(false);
        inspectorGeneratedContents.addHyperlinkListener(e -> {
            if (e.getEventType() == EventType.ACTIVATED)
                navigate(e.getDescription());
        });
        content.add(new JBScrollPane(inspectorGeneratedContents), BorderLayout.CENTER);
    }

    public void loadLocal(Local local, BigInteger frame) {
        try {
            SltLispEnvironmentProvider.getInstance()
                    .sendToLisp(SltInspectFrameVar.inspectVariable(BigInteger.valueOf(local.ix), frame, threadId,
                            result -> ApplicationManager.getApplication().invokeLater(() -> processResult(result))));
        } catch (Exception e) {
            log.warn(SltBundle.message("slt.error.sbclstart"), e);
            Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.sbcl.start"));
        }
    }

    private void processResult(LispElement result) {
        SltInspectedObject parsedResult = null;
        try {
            parsedResult = new SltInspectedObject((LispContainer) result);
        } catch (Exception ignored) {
            // in case we get garbage we show error
        }
        clear();
        if (parsedResult == null) {
            inspectorGeneratedContents.setText("Failed to parse result. Please report bug with this text: \n" + result.toString());
            inspectorGeneratedContents.setEditorKit(new DefaultEditorKit());
        } else {
            inspectorGeneratedContents.setContentType("text/html");
            inspectorGeneratedContents.setEditorKit(new HTMLEditorKit());
            loadInspectedObject(parsedResult);
        }
        inspectorGeneratedContents.setCaretPosition(0);
    }

    private void loadInspectedObject(SltInspectedObject inspectedObject) {
        HtmlBuilder contentBuilder = new HtmlBuilder();
        contentBuilder.append(HtmlChunk.text(inspectedObject.getTitle())
                .wrapWith("h2"));
        contentBuilder.append(HtmlChunk.br());
        for (SltInspectionElement element : inspectedObject.getElements()) {
            String text = element.getText();
            if (element.getId() == null) {
                String[] parts = text.split(Pattern.quote("\n"));
                for (int i=0; i<parts.length; i++) {
                    contentBuilder.append(parts[i]);
                    if (i < parts.length-1) {
                        contentBuilder.append(HtmlChunk.br());
                    }
                }
            } else {
                contentBuilder.append(HtmlChunk.link(mkLink(inspectedObject, element), text));
            }
        }

        inspectorGeneratedContents.setText(contentBuilder.toString());
    }

    private String mkLink(SltInspectedObject inspectedObject, SltInspectionElement element) {
        return String.format("%s,%s", inspectedObject.getId(), element.getId());
    }

    private void clear() {
        inspectorGeneratedContents.setContentType("text/plain");
        inspectorGeneratedContents.setText("");
    }

    private void navigate(String description) {
        String[] parts = description.split(Pattern.quote(","));
        BigInteger ix = new BigInteger(parts[1]);

        try {
            SltLispEnvironmentProvider.getInstance()
                    .sendToLisp(SltInspectNth.inspectVariable(ix, threadId, result ->
                            ApplicationManager.getApplication().invokeLater(() -> processResult(result))));
        } catch (Exception e) {
            log.warn(SltBundle.message("slt.error.sbclstart"), e);
            Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.sbcl.start"));
        }
    }

    private class GoBackAction extends AnAction {

        private GoBackAction() {
            super(SltBundle.message("slt.ui.inspector.navigation.back"), "", Actions.Back);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent event) {
            try {
                SltLispEnvironmentProvider.getInstance()
                        .sendToLisp(SltInspectorAction.action(ActionType.GO_BACK, threadId, result ->
                                ApplicationManager.getApplication().invokeLater(() -> processResult(result))));
            } catch (Exception e) {
                log.warn(SltBundle.message("slt.error.sbclstart"), e);
                Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.sbcl.start"));
            }
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(SltLispEnvironmentProvider.getInstance().isLispEnvironmentActive());
        }
    }

    private class GoForwardAction extends AnAction {

        private GoForwardAction() {
            super(SltBundle.message("slt.ui.inspector.navigation.forward"), "", Actions.Forward);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent event) {
            try {
                SltLispEnvironmentProvider.getInstance()
                        .sendToLisp(SltInspectorAction.action(ActionType.GO_FORWARD, threadId, result ->
                                ApplicationManager.getApplication().invokeLater(() -> processResult(result))));
            } catch (Exception e) {
                log.warn(SltBundle.message("slt.error.sbclstart"), e);
                Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.sbcl.start"));
            }
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(SltLispEnvironmentProvider.getInstance().isLispEnvironmentActive());
        }
    }

    private class RefreshAction extends AnAction {

        private RefreshAction() {
            super(SltBundle.message("slt.ui.inspector.navigation.refresh"), "", Actions.Refresh);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent event) {
            try {
                SltLispEnvironmentProvider.getInstance()
                        .sendToLisp(SltInspectorAction.action(ActionType.REFRESH, threadId, result ->
                                ApplicationManager.getApplication().invokeLater(() -> processResult(result))));
            } catch (Exception e) {
                log.warn(SltBundle.message("slt.error.sbclstart"), e);
                Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.sbcl.start"));
            }
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(SltLispEnvironmentProvider.getInstance().isLispEnvironmentActive());
        }
    }

    public JComponent getContent() {
        return content;
    }

}
