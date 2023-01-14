package com.en_circle.slt.plugin.ui.console;

import com.en_circle.slt.plugin.ui.PackageSelectorComponent;
import com.intellij.openapi.project.Project;
import com.intellij.ui.tabs.TabInfo;

import java.awt.*;

public class SltREPL extends SltConsole {

    public SltREPL(Project project) {
        super(project);
    }

    @Override
    public TabInfo create() {
        TabInfo tabInfo = super.create();

        PackageSelectorComponent selectorComponent = new PackageSelectorComponent("Repl", () -> this.currentModule);
        selectorComponent.refresh();
        selectorComponent.getActionToolbar().setTargetComponent(content);
        selectorComponent.setListener(this::setPackage);
        content.add(selectorComponent.getComponent(), BorderLayout.NORTH);

        return tabInfo;
    }

}
