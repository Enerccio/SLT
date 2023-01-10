package com.en_circle.slt.plugin;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

@State(
        name = "org.intellij.sdk.settings.AppSettingsState",
        storages = @Storage("SdkSettingsPlugin.xml")
)
public class SltState implements PersistentStateComponent<SltState> {

    public String sbclExecutable = "sbcl";
    public String quicklispStartScript = "~/quicklisp/setup.lisp";
    public int port = 4005;

    public static SltState getInstance() {
        return ApplicationManager.getApplication().getService(SltState.class);
    }

    @Override
    public @Nullable SltState getState() {
        return this;
    }

    @Override
    public void loadState(@NotNull SltState state) {
        XmlSerializerUtil.copyBean(state, this);
    }
}
