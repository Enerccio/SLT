package com.en_circle.slt.plugin.lisp.psi;

import com.intellij.core.CoreProjectEnvironment;

public class LispCoreProjectEnvironment extends CoreProjectEnvironment {

    public LispCoreProjectEnvironment() {
        super(() -> {}, new LispCoreApplicationEnviroment());
    }
}
