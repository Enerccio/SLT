package com.en_circle.slt.plugin.ui;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.environment.LispFeatures;
import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltOutput;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentListener;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentState;
import com.en_circle.slt.plugin.ui.console.SltConsole;
import com.en_circle.slt.plugin.ui.console.SltREPL;
import com.intellij.icons.AllIcons;
import com.intellij.icons.AllIcons.Actions;
import com.intellij.icons.AllIcons.General;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.ui.tabs.JBTabs;
import com.intellij.ui.tabs.JBTabsFactory;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class SltCoreWindow implements LispEnvironmentListener, Disposable {

    private final Project project;
    private final JTextField process;
    private final JPanel content;
    private final JBTabs tabs;
    private final List<SltComponent> components = Collections.synchronizedList(new ArrayList<>());


    public SltCoreWindow(ToolWindow toolWindow) {
        this.project = toolWindow.getProject();
        LispEnvironmentService.getInstance(project).addServerListener(this);

        content = new JPanel(new BorderLayout());
        SltOutputHandlerComponent outputHandlerComponent = new SltOutputHandlerComponent(project, SltOutput.STDOUT);
        components.add(outputHandlerComponent);
        Disposer.register(this, outputHandlerComponent);

        outputHandlerComponent = new SltOutputHandlerComponent(project, SltOutput.STDERR);
        components.add(outputHandlerComponent);
        Disposer.register(this, outputHandlerComponent);

        SltGeneralLog generalLog = new SltGeneralLog(project);
        Disposer.register(this, generalLog);

        components.add(generalLog);
        LispEnvironmentService.getInstance(project).setRequestResponseLogger(generalLog);

        createSbclControls();

        JLabel processLabel = new JLabel(SltBundle.message("slt.ui.process.processpid") + " ");
        process = new JTextField(20);
        process.setEditable(false);
        process.setMaximumSize(new Dimension(150, Integer.MAX_VALUE));

        JPanel processInfo = new JPanel();
        processInfo.setLayout(new BoxLayout(processInfo, BoxLayout.LINE_AXIS));
        processInfo.add(processLabel);
        processInfo.add(process);

        content.add(processInfo, BorderLayout.NORTH);

        tabs = JBTabsFactory.createTabs(toolWindow.getProject());
        for (SltComponent component : components) {
            tabs.addTab(component.create());
        }
        content.add(tabs.getComponent(), BorderLayout.CENTER);
    }

    private void createSbclControls() {
        DefaultActionGroup controlGroup = new DefaultActionGroup();
        controlGroup.add(new StartLispAction());
        controlGroup.add(new StopSbclAction());
        controlGroup.addSeparator();
        controlGroup.add(new ConsoleWindowAction());

        JPanel west = new JPanel(new BorderLayout());
        ActionToolbar toolbar = ActionManager.getInstance()
                .createActionToolbar("SltProcessWindowEvent", controlGroup, false);
        toolbar.setTargetComponent(content);
        west.add(toolbar.getComponent(), BorderLayout.NORTH);
        content.add(west, BorderLayout.WEST);
    }

    public void start() {
        LispEnvironmentService.getInstance(project).start();
    }

    public void stop() {
        LispEnvironmentService.getInstance(project).stop();
    }

    public JComponent getContent() {
        return content;
    }

    public void removeComponent(SltComponent component) {
        components.remove(component);
        tabs.removeTab(component.getTabInfo());
    }

    private void addRepl() {
        SltConsole console = new SltREPL(this.project, this);
        components.add(console);
        tabs.addTab(console.create());
        console.getTabInfo().setTabLabelActions(new DefaultActionGroup(
                new AnAction(SltBundle.message("slt.ui.repl.close"), "", Actions.Close) {

            @Override
            public void actionPerformed(@NotNull AnActionEvent e) {
                console.close();
            }

                    @Override
                    public @NotNull ActionUpdateThread getActionUpdateThread() {
                        return ActionUpdateThread.EDT;
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

        process.setText(LispEnvironmentService.getInstance(project).getEnvironment().getInformation().getPid());
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
    public void onOutputChanged(SltOutput output, String newData) {
        for (SltComponent component : components) {
            component.handleOutput(output, newData);
        }
    }

    public Project getProject() {
        return project;
    }

    @Override
    public void dispose() {

    }

    private class StartLispAction extends AnAction {

        private StartLispAction() {
            super(SltBundle.message("slt.ui.process.startinstance"), "", AllIcons.RunConfigurations.TestState.Run);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
           start();
        }

        @Override
        public @NotNull ActionUpdateThread getActionUpdateThread() {
            return ActionUpdateThread.EDT;
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(LispEnvironmentService.getInstance(project).getState() == LispEnvironmentState.STOPPED);
        }
    }

    private class StopSbclAction extends AnAction {

        private StopSbclAction() {
            super(SltBundle.message("slt.ui.process.stopinstance"), "", AllIcons.Actions.Suspend);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            stop();
        }

        @Override
        public @NotNull ActionUpdateThread getActionUpdateThread() {
            return ActionUpdateThread.EDT;
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(LispEnvironmentService.getInstance(project).getState() != LispEnvironmentState.STOPPED);
        }
    }

    private class ConsoleWindowAction extends AnAction {

        private ConsoleWindowAction() {
            super(SltBundle.message("slt.ui.process.openrepl"), "", General.Add);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            addRepl();
        }

        @Override
        public @NotNull ActionUpdateThread getActionUpdateThread() {
            return ActionUpdateThread.EDT;
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(LispEnvironmentService.getInstance(project).getState() == LispEnvironmentState.READY &&
                    LispEnvironmentService.getInstance(project).hasFeature(LispFeatures.REPL));
        }
    }

}
