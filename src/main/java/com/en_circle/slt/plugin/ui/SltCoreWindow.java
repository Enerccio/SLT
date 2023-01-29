package com.en_circle.slt.plugin.ui;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltOutput;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentListener;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentState;
import com.en_circle.slt.plugin.ui.console.SltConsole;
import com.en_circle.slt.plugin.ui.console.SltREPL;
import com.intellij.icons.AllIcons;
import com.intellij.icons.AllIcons.Actions;
import com.intellij.icons.AllIcons.General;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.ui.tabs.impl.JBTabsImpl;
import com.intellij.util.FileContentUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class SltCoreWindow implements LispEnvironmentListener {

    private final Project project;
    private final JTextField process;
    private final JPanel content;
    private final JBTabsImpl tabs;
    private final List<SltComponent> components = Collections.synchronizedList(new ArrayList<>());


    public SltCoreWindow(ToolWindow toolWindow) {
        this.project = toolWindow.getProject();
        LispEnvironmentService.getInstance(project).addServerListener(this);

        content = new JPanel(new BorderLayout());
        components.add(new SltOutputHandlerComponent(this, SltOutput.STDOUT));
        components.add(new SltOutputHandlerComponent(this, SltOutput.STDERR));
        SltGeneralLog generalLog = new SltGeneralLog();
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
        LispEnvironmentService.getInstance(project).start();

        PsiManager psiManager = PsiManager.getInstance(project);
        List<VirtualFile> toReparse = new ArrayList<>();
        for (VirtualFile vf : FileEditorManager.getInstance(project).getOpenFiles()) {
            PsiFile psiFile = psiManager.findFile(vf);
            if (psiFile != null && psiFile.getFileType().equals(SltCommonLispFileType.INSTANCE)) {
                toReparse.add(vf);
            }
        }
        FileContentUtil.reparseFiles(project, toReparse, false);
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

    private class StartSbclAction extends AnAction {

        private StartSbclAction() {
            super(SltBundle.message("slt.ui.process.startinstance"), "", AllIcons.RunConfigurations.TestState.Run);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            ApplicationManager.getApplication().invokeLater(SltCoreWindow.this::start);
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
            super(SltBundle.message("slt.ui.process.openrepl.sbcl"), "", General.Add);
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

            e.getPresentation().setEnabled(LispEnvironmentService.getInstance(project).getState() == LispEnvironmentState.READY);
        }
    }

}
