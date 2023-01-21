package com.en_circle.slt.plugin.sdk;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.project.Project;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

@State(name = "SltLispProjectSdk")
public class LispProjectSdk implements PersistentStateComponent<LispProjectSdk> {

    public static LispProjectSdk getInstance(Project project) {
        return project.getService(LispProjectSdk.class);
    }

    public LispSdk currentSDK;

    @Override
    public @Nullable LispProjectSdk getState() {
        return this;
    }

    @Override
    public void loadState(@NotNull LispProjectSdk state) {
        XmlSerializerUtil.copyBean(state, this);
    }
}
