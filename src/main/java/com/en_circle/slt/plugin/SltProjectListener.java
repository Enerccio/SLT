package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.services.SltProjectService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManagerListener;
import org.jetbrains.annotations.NotNull;

public class SltProjectListener implements ProjectManagerListener {

    @Override
    public void projectOpened(@NotNull Project project) {
        SltProjectService.getInstance(project);
    }

}
