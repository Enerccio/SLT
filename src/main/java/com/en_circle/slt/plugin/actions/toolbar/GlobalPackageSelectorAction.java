package com.en_circle.slt.plugin.actions.toolbar;

import com.en_circle.slt.plugin.ui.PackageSelectorComponent;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.Presentation;
import com.intellij.openapi.actionSystem.ex.CustomComponentAction;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class GlobalPackageSelectorAction extends AnAction implements CustomComponentAction {

    @Override
    public void actionPerformed(@NotNull AnActionEvent e) {

    }

    @Override
    public @NotNull JComponent createCustomComponent(@NotNull Presentation presentation, @NotNull String place) {
        PackageSelectorComponent packageSelectorComponent = new PackageSelectorComponent("GlobalPackageSelector", () -> "CL-USER");
        return packageSelectorComponent.getComponent();
    }

}
