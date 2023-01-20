package com.en_circle.slt.plugin.environment;

public interface SltLispEnvironment {

    void start(SltLispEnvironmentConfiguration configuration) throws SltProcessException;
    void stop() throws SltProcessException;

    boolean isActive();
    SltLispProcessInformation getInformation();

    interface SltLispOutputChangedListener {

        void onOutputChanged(SltOutput output, String newData);

    }

    enum SltOutput {
        STDOUT, STDERR
    }

}
