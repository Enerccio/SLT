package com.en_circle.slt.plugin.ui.sdk;

import com.en_circle.slt.plugin.sdk.LispSdk;
import com.intellij.openapi.ui.DialogWrapper;
import org.jetbrains.annotations.NotNull;

import java.awt.*;

public interface SdkDialogProvider {

    DialogWrapper createSdkConfiguration(@NotNull Component parent,
                                              LispSdk instance,
                                              String title,
                                              OnSave onSave);

    interface OnSave {

        void saveAction(LispSdk sdk);

    }

}
