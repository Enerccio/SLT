package com.en_circle.slt.plugin.view;

import com.en_circle.slt.plugin.SltState;
import com.en_circle.slt.plugin.swank.SwankServer;
import com.en_circle.slt.plugin.swank.SwankServer.SwankServerOutput;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.ui.components.JBTabbedPane;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

public class SltCoreWindow {

    public SltCoreWindow INSTANCE;

    private final JTextField process;
    private final JPanel content;
    private final List<SltComponent> components = new ArrayList<>();

    public SltCoreWindow(ToolWindow toolWindow) {
        INSTANCE = this;

        content = new JPanel(new BorderLayout());
        components.add(new SltOutputHandlerComponent(this, SwankServerOutput.STDOUT));
        components.add(new SltOutputHandlerComponent(this, SwankServerOutput.STDERR));

        createSbclControls();

        JLabel processLabel = new JLabel("SBCL Process Pid: ");
        process = new JTextField(20);
        process.setEditable(false);
        process.setMaximumSize(new Dimension(150, Integer.MAX_VALUE));

        JPanel processInfo = new JPanel();
        processInfo.setLayout(new BoxLayout(processInfo, BoxLayout.LINE_AXIS));
        processInfo.add(processLabel);
        processInfo.add(process);

        content.add(processInfo, BorderLayout.NORTH);

        JBTabbedPane tabbedPane = new JBTabbedPane();
        for (SltComponent component : components) {
            tabbedPane.addTab(component.getTitle(), component.create());
        }
        content.add(tabbedPane, BorderLayout.CENTER);
    }

    private void createSbclControls() {
        DefaultActionGroup controlGroup = new DefaultActionGroup();
        controlGroup.add(new StartSbclAction());
        controlGroup.add(new StopSbclAction());

        JPanel west = new JPanel(new BorderLayout());
        ActionToolbar toolbar = ActionManager.getInstance()
                .createActionToolbar("SltProcessWindowSbclEvent", controlGroup, false);
        toolbar.setTargetComponent(content);
        west.add(toolbar.getComponent(), BorderLayout.NORTH);
        content.add(west, BorderLayout.WEST);
    }



    public void start() {
        for (SltComponent component : components) {
            component.onPreStart();
        }
        try {
            SwankServer.startSbcl(SltState.getInstance().sbclExecutable, SltState.getInstance().port, (output, newData) -> {
                for (SltComponent component : components) {
                    component.handleOutput(output, newData);
                }
            });
            for (SltComponent component : components) {
                component.onPostStart();
            }

            Process p = SwankServer.getProcess();
            process.setText("" + p.pid());
        } catch (Exception e) {
            // TODO: show error
        }
    }

    public void stop() {
        for (SltComponent component : components) {
            component.onPreStop();
        }
        SwankServer.stop();
        for (SltComponent component : components) {
            component.onPostStop();
        }
        process.setText("");
    }

    public JComponent getContent() {
        return content;
    }

    private class StartSbclAction extends AnAction {

        private StartSbclAction() {
            super("Start SBCL Instance", "", AllIcons.RunConfigurations.TestState.Run);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            SwingUtilities.invokeLater(SltCoreWindow.this::start);
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(!SwankServer.INSTANCE.isActive());
        }
    }

    private class StopSbclAction extends AnAction {

        private StopSbclAction() {
            super("Stop SBCL Instance", "", AllIcons.Actions.Suspend);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            stop();
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(SwankServer.INSTANCE.isActive());
        }
    }

}
