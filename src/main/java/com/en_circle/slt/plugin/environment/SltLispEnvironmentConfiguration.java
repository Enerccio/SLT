package com.en_circle.slt.plugin.environment;

import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltLispOutputChangedListener;

public interface SltLispEnvironmentConfiguration {

    SltLispOutputChangedListener getListener();

    interface Builder<T extends Builder<T, RESULT>, RESULT extends SltLispEnvironmentConfiguration> {

        Builder<T, RESULT> setListener(SltLispOutputChangedListener listener);

        RESULT build();

    }

}
