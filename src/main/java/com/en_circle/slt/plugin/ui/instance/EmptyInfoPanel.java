package com.en_circle.slt.plugin.ui.instance;

import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltOutput;
import com.intellij.openapi.project.Project;

import javax.swing.*;
import java.awt.*;

public class EmptyInfoPanel implements InstanceInfoPanelComponent {

    private JPanel content;

    @Override
    public Component create(Project project) {
        content = new JPanel(new BorderLayout());
        return content;
    }

    @Override
    public Component getComponent() {
        return content;
    }

    @Override
    public void onOutputChanged(SltOutput output, String newData) {

    }

    @Override
    public void onPreStart() {

    }

    @Override
    public void onPostStart() {

    }

    @Override
    public void onPreStop() {

    }

    @Override
    public void onPostStop() {

    }

}
