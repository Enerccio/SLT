package com.en_circle.slt.plugin.view;

import com.en_circle.slt.plugin.SltSBCL;
import com.en_circle.slt.plugin.SltSBCL.SBCLServerListener;
import com.en_circle.slt.plugin.swank.SwankServer;
import com.en_circle.slt.plugin.swank.SwankServer.SwankServerOutput;
import com.intellij.icons.AllIcons;
import com.intellij.icons.AllIcons.Actions;
import com.intellij.icons.AllIcons.General;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.ui.tabs.impl.JBTabsImpl;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class SltCoreWindow implements SBCLServerListener {
    private static final Logger LOG = Logger.getInstance(SltCoreWindow.class);

    private Project project;
    private final JTextField process;
    private final JPanel content;
    private final JBTabsImpl tabs;
    private final List<SltComponent> components = Collections.synchronizedList(new ArrayList<>());


    public SltCoreWindow(ToolWindow toolWindow) {
        this.project = toolWindow.getProject();

        SltSBCL.getInstance().addServerListener(this);
        SltSBCL.getInstance().setProject(toolWindow.getProject());

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

        tabs = new JBTabsImpl(toolWindow.getProject());
        for (SltComponent component : components) {
            tabs.addTab(component.create());
        }
        content.add(tabs.getComponent(), BorderLayout.CENTER);
    }

    private void createSbclControls() {
        DefaultActionGroup controlGroup = new DefaultActionGroup();
        controlGroup.add(new StartSbclAction());
        controlGroup.add(new StopSbclAction());
        controlGroup.addSeparator();
        controlGroup.add(new ConsoleWindowAction());

        JPanel west = new JPanel(new BorderLayout());
        ActionToolbar toolbar = ActionManager.getInstance()
                .createActionToolbar("SltProcessWindowSbclEvent", controlGroup, false);
        toolbar.setTargetComponent(content);
        west.add(toolbar.getComponent(), BorderLayout.NORTH);
        content.add(west, BorderLayout.WEST);
    }

    public void start() {
        try {
            SltSBCL.getInstance().start();
        } catch (Exception e) {
            LOG.warn("Error starting sbcl", e);

            Messages.showErrorDialog(project, e.getMessage(), "Failed to Start SBCL");
        }
    }

    public void stop() {
        try {
            SltSBCL.getInstance().stop();
        } catch (Exception e) {
            LOG.warn("Error stopping sbcl", e);

            Messages.showErrorDialog(project, e.getMessage(), "Failed to Stop SBCL");
        }
    }

    public JComponent getContent() {
        return content;
    }

    public void removeComponent(SltComponent component) {
        components.remove(component);
        tabs.removeTab(component.getTabInfo());
    }

    private void addRepl() {
        SltConsole console = new SltConsole(this);
        components.add(console);
        tabs.addTab(console.create());
        console.getTabInfo().setTabLabelActions(new DefaultActionGroup(new AnAction("Close", "", Actions.Close) {

            @Override
            public void actionPerformed(@NotNull AnActionEvent e) {
                console.close();
            }

        }), ActionPlaces.EDITOR_TAB);
        tabs.select(console.getTabInfo(), true);
    }

    @Override
    public void onPreStart() {
        for (SltComponent component : components) {
            component.onPreStart();
        }
    }

    @Override
    public void onPostStart() {
        for (SltComponent component : components) {
            component.onPostStart();
        }

        Process p = SwankServer.getProcess();
        process.setText("" + p.pid());
    }

    @Override
    public void onPreStop() {
        for (SltComponent component : components) {
            component.onPreStop();
        }
    }

    @Override
    public void onPostStop() {
        for (SltComponent component : components) {
            component.onPostStop();
        }
        process.setText("");
    }

    @Override
    public void onOutputChanged(SwankServerOutput output, String newData) {
        for (SltComponent component : components) {
            component.handleOutput(output, newData);
        }
    }

    public Project getProject() {
        return project;
    }

    private class StartSbclAction extends AnAction {

        private StartSbclAction() {
            super("Start SBCL Instance", "", AllIcons.RunConfigurations.TestState.Run);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            ApplicationManager.getApplication().invokeLater(SltCoreWindow.this::start);
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

    private class ConsoleWindowAction extends AnAction {

        private ConsoleWindowAction() {
            super("Create SBCL REPL", "", General.Add);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            addRepl();
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(SwankServer.INSTANCE.isActive());
        }
    }

}
