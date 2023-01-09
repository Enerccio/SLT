package com.en_circle.slt.plugin.ui;

import com.en_circle.slt.plugin.swank.SlimeListener.RequestResponseLogger;
import com.en_circle.slt.plugin.swank.SwankServer.SwankServerOutput;
import com.en_circle.slt.tools.BufferedString;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.*;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.tabs.TabInfo;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.text.DefaultCaret;
import java.awt.*;

public class SltGeneralLog implements SltComponent, RequestResponseLogger {

    private final JPanel dataContainer;
    private JTextArea area;
    private TabInfo tabInfo;
    private BufferedString bufferedString;

    public SltGeneralLog() {
        this.dataContainer = new JPanel(new BorderLayout());
    }

    @Override
    public TabInfo create() {
        area = new JTextArea();
        area.setEditable(false);
        DefaultCaret caret = (DefaultCaret) area.getCaret();
        caret.setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE);

        JBScrollPane scrollPane = new JBScrollPane(area);
        dataContainer.add(scrollPane, BorderLayout.CENTER);

        DefaultActionGroup controlGroup = new DefaultActionGroup();
        ActionToolbar toolbar = ActionManager.getInstance().createActionToolbar("SltProcessWindowWrapEvent", controlGroup, false);
        toolbar.setTargetComponent(dataContainer);
        dataContainer.add(toolbar.getComponent(), BorderLayout.EAST);

        bufferedString = new BufferedString(data -> SwingUtilities.invokeLater(() -> {
            area.setText(area.getText() + data);
            if (caret.getUpdatePolicy() == DefaultCaret.ALWAYS_UPDATE)
                area.setCaretPosition(area.getDocument().getLength());
        }), fullData -> SwingUtilities.invokeLater(() -> {
            area.setText(fullData);
            if (caret.getUpdatePolicy() == DefaultCaret.ALWAYS_UPDATE)
                area.setCaretPosition(area.getDocument().getLength());
        }));

        controlGroup.add(new AnAction("Clear Text", "", AllIcons.Actions.GC) {

            @Override
            public void actionPerformed(@NotNull AnActionEvent e) {
                bufferedString.clear();
            }
        });
        controlGroup.add(new ToggleAction("Soft Wrap", "", AllIcons.Actions.ToggleSoftWrap) {

            @Override
            public boolean isSelected(@NotNull AnActionEvent e) {
                return area.getLineWrap();
            }

            @Override
            public void setSelected(@NotNull AnActionEvent e, boolean state) {
                area.setLineWrap(state);
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
                    area.setCaretPosition(area.getDocument().getLength());
                } else {
                    caret.setUpdatePolicy(DefaultCaret.NEVER_UPDATE);
                }
            }

        });

        tabInfo = new TabInfo(dataContainer);
        tabInfo.setText(getTitle());
        return tabInfo;
    }

    @Override
    public TabInfo getTabInfo() {
        return tabInfo;
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

    }

    @Override
    public String getTitle() {
        return "Slime Log";
    }

    @Override
    public void logRequest(String request) {
        bufferedString.append("\n\n" + request);
    }

    @Override
    public void logResponse(String response) {
        bufferedString.append("\n\n" + response);
    }
}
