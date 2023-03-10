package com.en_circle.slt.plugin.sdk;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

@State(
        name = "SltSdkList",
        storages = @Storage("SltSettings.xml")
)
public class SdkList implements PersistentStateComponent<SdkList> {

    public static SdkList getInstance() {
        return ApplicationManager.getApplication().getService(SdkList.class);
    }

    public List<LispSdk> sdks = new ArrayList<>();

    @Override
    public @Nullable SdkList getState() {
        return this;
    }

    @Override
    public void loadState(@NotNull SdkList state) {
        XmlSerializerUtil.copyBean(state, this);
    }

    public LispSdk getSdkByUuid(String currentSDK) {
        if (sdks == null)
            return null;

        for (LispSdk sdk : sdks) {
            if (sdk.uuid.equals(currentSDK))
                return sdk;
        }

        return null;
    }

    public boolean hasSdks() {
        return sdks != null && !sdks.isEmpty();
    }
}
