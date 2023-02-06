package com.en_circle.slt.plugin.ui.instance;

import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltOutput;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentListener;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;

import javax.swing.*;
import java.util.ArrayList;
import java.util.List;

public class InstanceInfoPanel implements LispEnvironmentListener {

    private final JPanel content;
    private final List<InstanceInfoPanelComponent> panels = new ArrayList<>();

    public InstanceInfoPanel(ToolWindow toolWindow) {
        Project project = toolWindow.getProject();
        LispEnvironmentService.getInstance(project).addServerListener(this);
        content = new JPanel();
        content.setLayout(new BoxLayout(content, BoxLayout.LINE_AXIS));

        addInfoPanel(new ThreadList());
        addInfoPanel(new EmptyInfoPanel());

        for (InstanceInfoPanelComponent component : panels) {
            content.add(component.create(project));
        }
    }

    private void addInfoPanel(InstanceInfoPanelComponent component) {
        panels.add(component);
    }

    public JComponent getContent() {
        return content;
    }

    @Override
    public void onOutputChanged(SltOutput output, String newData) {
        for (InstanceInfoPanelComponent component : panels) {
            component.onOutputChanged(output, newData);
        }
    }

    @Override
    public void onPreStart() {
        for (InstanceInfoPanelComponent component : panels) {
            component.onPreStart();
        }
    }

    @Override
    public void onPostStart() {
        for (InstanceInfoPanelComponent component : panels) {
            component.onPostStart();
        }
    }

    @Override
    public void onPreStop() {
        for (InstanceInfoPanelComponent component : panels) {
            component.onPreStop();
        }
    }

    @Override
    public void onPostStop() {
        for (InstanceInfoPanelComponent component : panels) {
            component.onPostStop();
        }
    }
}
