package com.en_circle.slt.plugin.ui.debug;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltUIConstants;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.swank.debug.SltDebugAction;
import com.en_circle.slt.plugin.swank.debug.SltDebugArgument;
import com.en_circle.slt.plugin.swank.debug.SltDebugInfo;
import com.en_circle.slt.plugin.swank.debug.SltDebugStackTraceElement;
import com.en_circle.slt.plugin.swank.requests.FrameLocalsAndCatchTags;
import com.en_circle.slt.plugin.swank.requests.InvokeNthRestart;
import com.en_circle.slt.plugin.swank.requests.ThrowToToplevel;
import com.intellij.icons.AllIcons.Actions;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.actionSystem.ActionPlaces;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.DefaultActionGroup;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.JBSplitter;
import com.intellij.ui.ScrollPaneFactory;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.ui.components.JBTextField;
import com.intellij.ui.tabs.TabInfo;
import com.intellij.ui.tabs.impl.JBTabsImpl;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.font.TextAttribute;
import java.io.File;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SltDebuggerImpl implements SltDebugger, Disposable {
    private static final Logger log = LoggerFactory.getLogger(SltDebuggerImpl.class);

    private final JComponent content;
    private final TabInfo tabInfo;
    private final SltDebuggers parent;
    private BigInteger lastDebugId;
    private JPanel singleFrameComponent;
    private final List<JPanel> stackframes = new ArrayList<>();
    private BigInteger debugContext;

    public SltDebuggerImpl(SltDebuggers parent) {
        this.parent = parent;
        this.content = new JPanel(new BorderLayout());
        this.tabInfo = new TabInfo(this.content);
        this.tabInfo.setText(SltBundle.message("slt.ui.debugger.title"));
        this.tabInfo.setBlinkCount(5);
        this.tabInfo.setTabLabelActions(new DefaultActionGroup(
                new AnAction(SltBundle.message("slt.ui.debugger.close"), "", Actions.Close) {
            @Override
            public void actionPerformed(@NotNull AnActionEvent e) {
                close();
            }

        }), ActionPlaces.EDITOR_TAB);
    }

    @Override
    public TabInfo getTab() {
        return tabInfo;
    }

    @Override
    public void redraw(SltDebugInfo debugInfo) {
        this.tabInfo.setText(SltBundle.message("slt.ui.debugger.title") + debugInfo.getThreadId());
        this.stackframes.clear();

        this.content.removeAll();
        this.lastDebugId = debugInfo.getThreadId();

        JBSplitter splitter = new JBSplitter(false);
        splitter.setProportion(0.5f);
        this.content.add(splitter, BorderLayout.CENTER);

        JBTextField errorName = new JBTextField();
        errorName.setEditable(false);
        errorName.setText(getText(debugInfo.getInfo()));

        JBTextField condition = new JBTextField();
        condition.setEditable(false);
        condition.setText(getText(debugInfo.getConditionInfo()));

        JPanel infoPanel = new JPanel();
        infoPanel.setLayout(new BoxLayout(infoPanel, BoxLayout.X_AXIS));
        infoPanel.add(new Label(SltBundle.message("slt.ui.debugger.errormessage") + " "));
        infoPanel.add(errorName);
        infoPanel.add(new Label(SltBundle.message("slt.ui.debugger.condition") + " "));
        infoPanel.add(condition);
        content.add(infoPanel, BorderLayout.NORTH);

        JBSplitter splitter2 = new JBSplitter(false);
        splitter2.setProportion(0.4f);
        splitter.setFirstComponent(splitter2);

        JPanel actionsPanel = new JPanel();
        actionsPanel.setLayout(new BoxLayout(actionsPanel, BoxLayout.Y_AXIS));

        for (SltDebugAction action : debugInfo.getActions()) {
            JLabel label = new JLabel(getText(action.getActionName()));
            label.setCursor(new Cursor(Cursor.HAND_CURSOR));
            Map<TextAttribute, Object> attributes = new HashMap<>(label.getFont().getAttributes());
            attributes.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON);
            label.setFont(label.getFont().deriveFont(attributes));
            label.setForeground(SltUIConstants.HYPERLINK_COLOR);
            label.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    actionAccepted(action, debugInfo);
                }
            });
            JPanel labelPanel = new JPanel(new BorderLayout());
            labelPanel.add(label, BorderLayout.CENTER);

            JBTextArea textArea = new JBTextArea(action.getActionDescription());
            textArea.setEditable(false);
            textArea.setWrapStyleWord(true);
            textArea.setLineWrap(true);

            JPanel actionInfo = new JPanel();
            actionInfo.setLayout(new BoxLayout(actionInfo, BoxLayout.Y_AXIS));

            actionInfo.add(labelPanel);
            actionInfo.add(new JScrollPane(textArea));

            actionsPanel.add(actionInfo);
        }

        JBScrollPane pane = new JBScrollPane(actionsPanel);
        JPanel actionsPanelDecorator = new JPanel(new BorderLayout());
        actionsPanelDecorator.setBorder(BorderFactory.createTitledBorder(SltBundle.message("slt.ui.debugger.actions")));
        actionsPanelDecorator.add(pane, BorderLayout.CENTER);
        splitter2.setFirstComponent(actionsPanelDecorator);

        JPanel stackframes = new JPanel();
        stackframes.setLayout(new BoxLayout(stackframes, BoxLayout.Y_AXIS));
        for (SltDebugStackTraceElement element : debugInfo.getStacktrace()) {
            JLabel label = new JLabel(element.getLine());
            label.setCursor(new Cursor(Cursor.HAND_CURSOR));
            label.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    stackframeClicked(element, debugInfo);
                }
            });
            JPanel p = new JPanel(new BorderLayout());
            p.setBackground(SltUIConstants.DEBUG_FRAMES_COLOR);
            p.add(label, BorderLayout.CENTER);
            stackframes.add(p);
            this.stackframes.add(p);
        }

        JPanel stackframesContainer = new JPanel(new BorderLayout());
        stackframesContainer.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(),
                SltBundle.message("slt.ui.debugger.frames")));
        stackframesContainer.add(ScrollPaneFactory.createScrollPane(stackframes), BorderLayout.CENTER);
        splitter2.setSecondComponent(stackframesContainer);

        singleFrameComponent = new JPanel(new BorderLayout());
        TabInfo singleFrame = new TabInfo(singleFrameComponent);
        singleFrame.setText(SltBundle.message("slt.ui.debugger.frame"));
        JBTabsImpl singleFrameParent = new JBTabsImpl(parent.getProject());
        singleFrameParent.addTab(singleFrame);
        splitter.setSecondComponent(singleFrameParent);
    }

    private String getText(String text) {
        return StringUtils.replace(text, "\n", " ");
    }

    private void actionAccepted(SltDebugAction action, SltDebugInfo debugInfo) {
        int ix = debugInfo.getActions().indexOf(action);
        if (action.getArguments().isEmpty()) {
            try {
                LispEnvironmentService.getInstance(parent.getProject()).sendToLisp(InvokeNthRestart.nthRestart(debugInfo.getThreadId(),
                        BigInteger.valueOf(ix), debugInfo.getDebugLevel(), "NIL", "NIL", () -> {}));
            } catch (Exception e) {
                log.warn(SltBundle.message("slt.error.start"), e);
                Messages.showErrorDialog(parent.getProject(), e.getMessage(), SltBundle.message("slt.ui.errors.lisp.start"));
            }
        } else {
            List<String> arguments = new ArrayList<>();
            String rest = "NIL";
            for (SltDebugArgument argument : action.getArguments()) {
                if (!argument.isRest()) {
                    String arg = Messages.showInputDialog(SltBundle.message("slt.ui.debugger.arg.text") + " " + argument.getName(),
                            SltBundle.message("slt.ui.debugger.arg.title"),null);
                    arguments.add(StringUtils.isBlank(arg) ? "NIL" : arg);
                } else {
                    String arg = Messages.showInputDialog(SltBundle.message("slt.ui.debugger.rest.text") + " " + argument.getName(),
                            SltBundle.message("slt.ui.debugger.rest.title"),null);
                    if (StringUtils.isNotBlank(arg)) {
                        rest = "(" + arg + ")";
                    }
                }
            }
            String args = arguments.size() == 0 ? "NIL" : "(" + String.join(" ", arguments) + ")";
            try {
                LispEnvironmentService.getInstance(parent.getProject()).sendToLisp(InvokeNthRestart.nthRestart(debugInfo.getThreadId(),
                        BigInteger.valueOf(ix), debugInfo.getDebugLevel(), args, rest, () -> {}));
            } catch (Exception e) {
                log.warn(SltBundle.message("slt.error.start"), e);
                Messages.showErrorDialog(parent.getProject(), e.getMessage(), SltBundle.message("slt.ui.errors.lisp.start"));
            }
        }

    }

    private void stackframeClicked(SltDebugStackTraceElement element, SltDebugInfo debugInfo) {
        int ix = debugInfo.getStacktrace().indexOf(element);
        for (int ix2=0; ix2<stackframes.size(); ix2++) {
            JPanel p = stackframes.get(ix2);
            if (ix2 == ix) {
                p.setBackground(SltUIConstants.DEBUG_FRAMES_SELECTED_COLOR);
            } else {
                p.setBackground(SltUIConstants.DEBUG_FRAMES_COLOR);
            }
        }
        if (element.isFile() && element.getPosition() >= 0) {
            Project project = parent.getProject();
            ProjectFileIndex index = ProjectFileIndex.getInstance(project);
            VirtualFile vf = LocalFileSystem.getInstance().findFileByIoFile(new File(element.getLocation()));
            if (vf != null) {
                if (index.isInSource(vf)) {
                    FileEditorManager.getInstance(project)
                            .openTextEditor(new OpenFileDescriptor(project, vf, element.getPosition()), true);
                } else {
                    FileEditorManager.getInstance(project)
                            .openEditor(new OpenFileDescriptor(project, vf, element.getPosition()), true);
                }
            }
        }
        try {
            LispEnvironmentService.getInstance(parent.getProject()).sendToLisp(FrameLocalsAndCatchTags.getLocals(BigInteger.valueOf(ix),
                    debugInfo.getThreadId(), result -> {
                ApplicationManager.getApplication().invokeLater(() ->
                        ApplicationManager.getApplication().runWriteAction(() -> {
                            SltFrameInfo frameInfo = new SltFrameInfo(parent.getProject(), debugInfo.getThreadId(), BigInteger.valueOf(ix),
                                    element.getFramePackage());
                            singleFrameComponent.removeAll();
                            singleFrameComponent.add(frameInfo.getContent(), BorderLayout.CENTER);
                            frameInfo.refreshFrameValues(result);
                }));
            }), true);
        } catch (Exception e) {
            log.warn(SltBundle.message("slt.error.start"), e);
            Messages.showErrorDialog(parent.getProject(), e.getMessage(), SltBundle.message("slt.ui.errors.lisp.start"));
        }
    }

    private void close() {
        try {
            LispEnvironmentService.getInstance(parent.getProject()).sendToLisp(new ThrowToToplevel(lastDebugId));
        } catch (Exception e) {
            log.warn(SltBundle.message("slt.error.start"), e);
            Messages.showErrorDialog(parent.getProject(), e.getMessage(), SltBundle.message("slt.ui.errors.lisp.start"));
        }
    }

    @Override
    public void activate() {
        this.tabInfo.fireAlert();
    }

    @Override
    public void dispose() {

    }
}
