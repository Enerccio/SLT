package com.en_circle.slt.tools.platform;

import com.intellij.openapi.util.SystemInfo;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

public class PlatformActionsContainer {

    private static final List<PlatformAction> platformActions = new ArrayList<>();

    static {
        if (SystemInfo.isWindows) {
            platformActions.add(new DownloadSBCLMsiAction());
        }
    }

    public static <T extends PlatformAction> boolean hasAction(Class<T> trait) {
        return getAction(trait) != null;
    }

    @SuppressWarnings("unchecked")
    public static <T extends PlatformAction> @Nullable T getAction(Class<T> trait) {
        T t = (T) platformActions.stream().filter(x -> trait.isAssignableFrom(x.getClass()))
                .findFirst().orElse(null);
        if (t != null) {
            return (T) t.newInstance();
        }
        return null;
    }

}
