package com.en_circle.slt.plugin.ui;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.environment.LispFeatures;
import com.en_circle.slt.plugin.services.SltProjectService;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.ui.debug.SltInspector;
import com.intellij.icons.AllIcons.Actions;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.ui.components.JBTextField;
import com.intellij.ui.content.Content;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

public class SltSymbolInspectorView implements Disposable {

    private final ToolWindow toolWindow;
    private final Project project;
    private final JComponent panel;
    private final JBTextField inputSymbol;
    private final SltInspector inspector;
    private Content self;

    public SltSymbolInspectorView(ToolWindow toolWindow) {
        this.toolWindow = toolWindow;
        this.project = toolWindow.getProject();

        inputSymbol = new JBTextField();
        JPanel top = new JPanel(new BorderLayout());
        top.add(inputSymbol, BorderLayout.CENTER);
        DefaultActionGroup controlGroup = new DefaultActionGroup();
        controlGroup.add(new AnAction(SltBundle.message("slt.ui.sideinspect.send"), "", Actions.Refresh) {

            @Override
            public void actionPerformed(@NotNull AnActionEvent e) {
                if (StringUtils.isNotBlank(inputSymbol.getText()))
                    loadSymbol(inputSymbol.getText());
            }

            @Override
            public @NotNull ActionUpdateThread getActionUpdateThread() {
                return ActionUpdateThread.EDT;
            }

            @Override
            public void update(@NotNull AnActionEvent e) {
                e.getPresentation().setEnabled(LispEnvironmentService.getInstance(project)
                        .hasFeature(LispFeatures.INSPECTOR));
            }
        });
        ActionToolbar toolbar = ActionManager.getInstance()
                .createActionToolbar("SltSymbolInspectorViewControlGroup", controlGroup, false);
        toolbar.setTargetComponent(top);
        top.add(toolbar.getComponent(), BorderLayout.EAST);

        panel = new JPanel(new BorderLayout());
        panel.add(top, BorderLayout.NORTH);
        inspector = new SltInspector(project, null);
        panel.add(inspector.getContent(), BorderLayout.CENTER);

        SltProjectService.getInstance(project).setInspectorView(this);
    }

    private void loadSymbol(String symbolName) {
        inspector.loadSymbol(symbolName);
    }

    public JComponent getContent() {
        return panel;
    }

    @Override
    public void dispose() {

    }

    public void setSelf(Content self) {
        this.self = self;
    }

    public void setSymbolAndLoad(String symbolName) {
        inputSymbol.setText(symbolName);
        loadSymbol(symbolName);
    }

    public void showContent() {
        toolWindow.getContentManager().setSelectedContent(self, true);
    }
}
