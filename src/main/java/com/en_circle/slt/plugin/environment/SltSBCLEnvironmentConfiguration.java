package com.en_circle.slt.plugin.environment;


import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltLispOutputChangedListener;
import com.en_circle.slt.plugin.environment.SltLispEnvironmentProcess.SltLispEnvironmentProcessConfiguration;

public class SltSBCLEnvironmentConfiguration implements SltLispEnvironmentProcessConfiguration {

    private String executablePath = "sbcl";
    private String corePath = "";
    private String quicklispStartScript = "~/quicklisp/setup.lisp";
    private String projectDirectory = "/tmp";
    private SltLispOutputChangedListener listener = null;

    private SltSBCLEnvironmentConfiguration() {

    }

    public String getExecutablePath() {
        return executablePath;
    }

    public String getQuicklispStartScript() {
        return quicklispStartScript;
    }

    public String getProjectDirectory() {
        return projectDirectory;
    }

    @Override
    public SltLispOutputChangedListener getListener() {
        return listener;
    }

    public String getCorePath() {
        return corePath;
    }

    public static class Builder implements SltLispEnvironmentConfiguration.Builder<Builder, SltSBCLEnvironmentConfiguration> {

        private final SltSBCLEnvironmentConfiguration c = new SltSBCLEnvironmentConfiguration();
        private boolean built = false;

        public Builder setExecutable(String executable) {
            checkNotBuilt();

            c.executablePath = executable;
            return this;
        }

        public Builder setCore(String core) {
            checkNotBuilt();

            c.corePath = core;
            return this;
        }

        public Builder setQuicklispStartScriptPath(String quicklispStartScript) {
            checkNotBuilt();

            c.quicklispStartScript = quicklispStartScript;
            return this;
        }

        public Builder setProjectDirectory(String projectDirectory) {
            checkNotBuilt();

            c.projectDirectory = projectDirectory;
            return this;
        }

        @Override
        public Builder setListener(SltLispOutputChangedListener listener) {
            checkNotBuilt();

            c.listener = listener;
            return this;
        }

        @Override
        public SltSBCLEnvironmentConfiguration build() {
            checkNotBuilt();
            built = true;
            return c;
        }

        private void checkNotBuilt() {
            if (built) throw new IllegalStateException("already built");
        }

    }

}
