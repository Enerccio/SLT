package com.en_circle.slt.plugin.ui;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;

public class SltGlobalUIService implements Disposable {

    public static SltGlobalUIService getInstance() {
        return ApplicationManager.getApplication().getService(SltGlobalUIService.class);
    }

    @Override
    public void dispose() {

    }
}
