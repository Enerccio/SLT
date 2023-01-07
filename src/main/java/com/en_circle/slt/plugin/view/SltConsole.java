package com.en_circle.slt.plugin.view;

import com.en_circle.slt.plugin.SltSBCL;
import com.en_circle.slt.plugin.swank.SwankServer.SwankServerOutput;
import com.en_circle.slt.plugin.swank.requests.SwankIteractiveEval;
import com.intellij.icons.AllIcons;
import com.intellij.icons.AllIcons.Toolwindows;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.ui.Messages;
import com.intellij.ui.JBSplitter;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.tabs.TabInfo;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.text.DefaultCaret;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

public class SltConsole implements SltComponent {
    private static final Logger LOG = Logger.getInstance(SltConsole.class);

    private final SltCoreWindow parent;
    private final JPanel content;
    private JTextArea history;
    private LispEditor repl;
    private TabInfo tabInfo;

    private String currentModule = "cl-user";

    public SltConsole(SltCoreWindow parent) {
        this.parent = parent;
        this.content = new JPanel(new BorderLayout());
    }

    @Override
    public TabInfo create() {
        JBSplitter splitter = new JBSplitter(true, 0.7f);
        content.add(splitter, BorderLayout.CENTER);

        createHistorySection(splitter);
        createConsole(splitter);

        tabInfo = new TabInfo(content);
        tabInfo.setText(getTitle());
        return tabInfo;
    }

    @Override
    public TabInfo getTabInfo() {
        return tabInfo;
    }

    private void createHistorySection(JBSplitter splitter) {
        JPanel historyPanel = new JPanel(new BorderLayout());
        history = new JTextArea();
        history.setEditable(false);
        DefaultCaret caret = (DefaultCaret) history.getCaret();
        caret.setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE);
        JBScrollPane scrollPane = new JBScrollPane(history);
        historyPanel.add(scrollPane, BorderLayout.CENTER);

        DefaultActionGroup controlGroup = new DefaultActionGroup();
        ActionToolbar toolbar = ActionManager.getInstance().createActionToolbar("SltProcessWindowWrapEvent", controlGroup, false);
        toolbar.setTargetComponent(historyPanel);
        historyPanel.add(toolbar.getComponent(), BorderLayout.EAST);

        controlGroup.add(new AnAction("Clear Text", "", AllIcons.Actions.GC) {

            @Override
            public void actionPerformed(@NotNull AnActionEvent e) {
                history.setText("");
            }
        });
        controlGroup.add(new ToggleAction("Soft Wrap", "", AllIcons.Actions.ToggleSoftWrap) {

            @Override
            public boolean isSelected(@NotNull AnActionEvent e) {
                return history.getLineWrap();
            }

            @Override
            public void setSelected(@NotNull AnActionEvent e, boolean state) {
                history.setLineWrap(state);
            }

        });
        controlGroup.add(new ToggleAction("Scroll to End", "", AllIcons.RunConfigurations.Scroll_down) {

            @Override
            public boolean isSelected(@NotNull AnActionEvent e) {
                return caret.getUpdatePolicy() == DefaultCaret.ALWAYS_UPDATE;
            }

            @Override
            public void setSelected(@NotNull AnActionEvent e, boolean state) {
                if (state) {
                    caret.setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE);
                    history.setCaretPosition(history.getDocument().getLength());
                } else {
                    caret.setUpdatePolicy(DefaultCaret.NEVER_UPDATE);
                }
            }

        });
        splitter.setFirstComponent(historyPanel);
    }

    private void createConsole(JBSplitter splitter) {
        JPanel console = new JPanel(new BorderLayout());

        repl = new LispEditor();
        repl.addKeyListener(new KeyListener() {
            @Override
            public void keyTyped(KeyEvent keyEvent) {

            }

            @Override
            public void keyPressed(KeyEvent keyEvent) {

            }

            @Override
            public void keyReleased(KeyEvent keyEvent) {
                // TODO: configurable?
                if (keyEvent.isAltDown() && keyEvent.getKeyCode() == KeyEvent.VK_ENTER) {
                    eval();
                }
            }

        });

        DefaultActionGroup controlGroup = new DefaultActionGroup();
        ActionToolbar toolbar = ActionManager.getInstance().createActionToolbar("SltConsole", controlGroup, true);
        toolbar.setTargetComponent(content);
        console.add(toolbar.getComponent(), BorderLayout.SOUTH);

        controlGroup.add(new AnAction("Evaluate", "", Toolwindows.ToolWindowRun) {

            @Override
            public void actionPerformed(@NotNull AnActionEvent event) {
                eval();
            }

        });

        JBScrollPane scrollPane = new JBScrollPane(repl);
        console.add(scrollPane, BorderLayout.CENTER);
        splitter.setSecondComponent(console);
    }

    private void eval() {
        String data = repl.getText();
        try {
            if (StringUtils.isNotBlank(data)) {
                appendText("\n" + repl.getText() + "\n");

                SltSBCL.getInstance().sendToSbcl(SwankIteractiveEval.eval(data, currentModule,
                        result -> appendText(result.substring(1, result.length() - 1) + "\n")));
            }
        } catch (Exception e) {
            LOG.warn("Error starting sbcl", e);
            Messages.showErrorDialog(parent.getProject(), e.getMessage(), "Failed to Start SBCL");
        } finally {
            repl.setText("");
        }
    }

    private void appendText(String data) {
        history.setText(history.getText() + data);
        DefaultCaret caret = (DefaultCaret) history.getCaret();
        if (caret.getUpdatePolicy() == DefaultCaret.ALWAYS_UPDATE) {
            history.setCaretPosition(history.getDocument().getLength());
        }
    }

    @Override
    public void onPreStart() {

    }

    @Override
    public void onPostStart() {

    }

    @Override
    public void handleOutput(SwankServerOutput output, String data) {

    }

    @Override
    public void onPreStop() {

    }

    @Override
    public void onPostStop() {
        history.setText(history.getText() + "\n\n Process Stopped \n\n");
    }

    @Override
    public String getTitle() {
        return "REPL";
    }

    public void close() {
        parent.removeComponent(this);
    }
}
