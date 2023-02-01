package com.en_circle.slt.plugin.highlights;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.project.Project;

public class BraceHighlightService implements Disposable {

    public static BraceHighlightService getService(Project project) {
        return project.getService(BraceHighlightService.class);
    }

    public BraceHighlightService(Project project) {

    }

    @Override
    public void dispose() {

    }
}
