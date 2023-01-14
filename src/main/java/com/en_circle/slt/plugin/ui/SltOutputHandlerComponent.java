package com.en_circle.slt.plugin.ui;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.swank.SwankServer.SwankServerOutput;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.*;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.tabs.TabInfo;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.text.DefaultCaret;
import java.awt.*;

public class SltOutputHandlerComponent implements SltComponent {

    private final SwankServerOutput output;
    private final JPanel dataContainer;
    private JTextArea area;
    private TabInfo tabInfo;

    public SltOutputHandlerComponent(SltCoreWindow coreWindow, SwankServerOutput output) {
        this.output = output;
        this.dataContainer = new JPanel(new BorderLayout());
    }

    public SwankServerOutput getOutput() {
        return output;
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

        controlGroup.add(new AnAction(SltBundle.message("slt.ui.process.gl.clear"), "", AllIcons.Actions.GC) {

            @Override
            public void actionPerformed(@NotNull AnActionEvent e) {
                area.setText("");
            }
        });
        controlGroup.add(new ToggleAction(SltBundle.message("slt.ui.process.gl.wrap"), "", AllIcons.Actions.ToggleSoftWrap) {

            @Override
            public boolean isSelected(@NotNull AnActionEvent e) {
                return area.getLineWrap();
            }

            @Override
            public void setSelected(@NotNull AnActionEvent e, boolean state) {
                area.setLineWrap(state);
            }

        });
        controlGroup.add(new ToggleAction(SltBundle.message("slt.ui.process.gl.scroll"), "", AllIcons.RunConfigurations.Scroll_down) {

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
        if (StringUtils.isNotBlank(area.getText())) {
            area.setText(area.getText() + "\n\n***\n\n");
        }
    }

    @Override
    public void onPostStart() {

    }

    @Override
    public void handleOutput(SwankServerOutput output, String data) {
        if (output == this.output) {
            area.setText(area.getText() + data);
            DefaultCaret caret = (DefaultCaret) area.getCaret();
            if (caret.getUpdatePolicy() == DefaultCaret.ALWAYS_UPDATE)
                area.setCaretPosition(area.getDocument().getLength());
        }
    }

    @Override
    public void onPreStop() {

    }

    @Override
    public void onPostStop() {

    }

    @Override
    public String getTitle() {
        return getOutput() == SwankServerOutput.STDERR ? SltBundle.message("slt.ui.process.log.error.title") :
                SltBundle.message("slt.ui.process.log.output.title");
    }

}
