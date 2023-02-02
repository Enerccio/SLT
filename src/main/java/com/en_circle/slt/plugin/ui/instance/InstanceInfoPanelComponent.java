package com.en_circle.slt.plugin.ui.instance;

import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentListener;
import com.intellij.openapi.project.Project;

import java.awt.*;

public interface InstanceInfoPanelComponent extends LispEnvironmentListener {

    Component create(Project project);
    Component getComponent();


}
