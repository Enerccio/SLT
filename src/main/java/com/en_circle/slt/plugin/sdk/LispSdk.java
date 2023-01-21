package com.en_circle.slt.plugin.sdk;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LispSdk implements PersistentStateComponent<LispSdk> {

    public String uuid;
    public String userName;

    public String sbclExecutable;
    public String sbclCorePath;
    public String quickLispPath;

    @Override
    public @Nullable LispSdk getState() {
        return this;
    }

    @Override
    public void loadState(@NotNull LispSdk state) {
        XmlSerializerUtil.copyBean(state, this);
    }

}
