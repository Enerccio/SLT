package com.en_circle.slt.plugin.ui;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.project.Project;

public class SltUIService implements Disposable {

    public static SltUIService getInstance(Project project) {
        return project.getService(SltUIService.class);
    }

    @Override
    public void dispose() {

    }
}
