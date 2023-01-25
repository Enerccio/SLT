package com.en_circle.slt.plugin.indentation;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.project.Project;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

@State(
        name = "SltProjectIndentationSettings",
        storages = @Storage("SltSettings.xml")
)
public class SltProjectIndentationSettings implements PersistentStateComponent<SltProjectIndentationSettings> {

    public static SltProjectIndentationSettings getInstance(Project project) {
        return project.getService(SltProjectIndentationSettings.class);
    }

    public boolean overridingApplicationSetting = false;
    public SltIndentationSettings indentationSettings;

    @Override
    public @Nullable SltProjectIndentationSettings getState() {
        return this;
    }

    @Override
    public void loadState(@NotNull SltProjectIndentationSettings state) {
        XmlSerializerUtil.copyBean(state, this);
    }

    public void copySettings(SltIndentationSettings settings) {
        indentationSettings = new SltIndentationSettings();
        XmlSerializerUtil.copyBean(indentationSettings, settings);
    }
}
