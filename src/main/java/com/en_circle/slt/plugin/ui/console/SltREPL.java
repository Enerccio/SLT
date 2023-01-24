package com.en_circle.slt.plugin.ui.console;

import com.en_circle.slt.plugin.ui.PackageSelectorComponent;
import com.en_circle.slt.plugin.ui.SltCoreWindow;
import com.intellij.openapi.project.Project;
import com.intellij.ui.tabs.TabInfo;

import java.awt.*;

public class SltREPL extends SltConsole {

    private final SltCoreWindow window;

    public SltREPL(Project project, SltCoreWindow window) {
        super(project);
        this.window = window;
    }

    @Override
    public void close() {
        window.removeComponent(this);
        super.close();
    }

    @Override
    public TabInfo create() {
        TabInfo tabInfo = super.create();

        PackageSelectorComponent selectorComponent = new PackageSelectorComponent("Repl", () -> this.currentPackage);
        selectorComponent.refresh();
        selectorComponent.getActionToolbar().setTargetComponent(content);
        selectorComponent.setListener(this::setPackage);
        content.add(selectorComponent.getComponent(), BorderLayout.NORTH);

        return tabInfo;
    }

}
