package com.en_circle.slt.plugin.lisp.psi;

import com.intellij.core.CoreApplicationEnvironment;
import com.intellij.openapi.Disposable;
import org.jetbrains.annotations.NotNull;

public class LispCoreApplicationEnviroment extends CoreApplicationEnvironment {

    public LispCoreApplicationEnviroment() {
        super(() -> {});
    }
}
