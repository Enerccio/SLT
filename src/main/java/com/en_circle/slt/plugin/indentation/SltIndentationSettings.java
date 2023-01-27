package com.en_circle.slt.plugin.indentation;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.project.Project;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

@State(
        name = "SltIndentationSettings",
        storages = @Storage("SltSettings.xml")
)
public class SltIndentationSettings implements PersistentStateComponent<SltIndentationSettings> {

    public static SltIndentationSettings getInstance() {
        return getInstance(null);
    }

    public static SltIndentationSettings getInstance(@Nullable Project project) {
        if (project != null) {
            SltProjectIndentationSettings projectIndentationSettings = SltProjectIndentationSettings.getInstance(project);
            if (projectIndentationSettings.overridingApplicationSetting) {
                return projectIndentationSettings.indentationSettings;
            }
        }
        return ApplicationManager.getApplication().getService(SltIndentationSettings.class);
    }

    public boolean applies = true;
    public int defaultIndentation = 4;
    public int parameterIndentation = 4;
    public int lambdaIndentation = 4;
    public int bodyIndentation = 2;
    public int tagbodyIndentation = 1;

    @Override
    public @Nullable SltIndentationSettings getState() {
        return this;
    }

    @Override
    public void loadState(@NotNull SltIndentationSettings state) {
        XmlSerializerUtil.copyBean(state, this);
    }
}
