package com.en_circle.slt.plugin;

import com.intellij.DynamicBundle;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.PropertyKey;

public class SltBundle extends DynamicBundle {

    private static final String BUNDLE = "messages.SltBundle";
    private static final SltBundle INSTANCE = new SltBundle();

    public SltBundle() {
        super(BUNDLE);
    }

    @NotNull
    public static @Nls String message(@NotNull @PropertyKey(resourceBundle = BUNDLE) String key,
                                      Object @NotNull ... params) {
        return INSTANCE.getMessage(key, params);
    }
}
