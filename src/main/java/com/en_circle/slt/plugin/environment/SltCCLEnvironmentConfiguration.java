package com.en_circle.slt.plugin.environment;


import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltLispOutputChangedListener;
import com.en_circle.slt.plugin.environment.SltLispEnvironmentProcess.SltLispEnvironmentProcessConfiguration;

public class SltCCLEnvironmentConfiguration implements SltLispEnvironmentProcessConfiguration {

    private String executablePath = "clisp";
    private String memoryImage = "";
    private String quicklispStartScript = "~/quicklisp/setup.lisp";
    private String projectDirectory = "/tmp";
    private SltLispOutputChangedListener listener = null;

    private SltCCLEnvironmentConfiguration() {

    }

    public String getExecutablePath() {
        return executablePath;
    }

    public String getMemoryImage() {
        return memoryImage;
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

    public static class Builder implements SltLispEnvironmentConfiguration.Builder<Builder, SltCCLEnvironmentConfiguration> {

        private final SltCCLEnvironmentConfiguration c = new SltCCLEnvironmentConfiguration();
        private boolean built = false;

        public Builder setExecutable(String executable) {
            checkNotBuilt();

            c.executablePath = executable;
            return this;
        }

        public Builder setMemoryImage(String memoryImage) {
            checkNotBuilt();

            c.memoryImage = memoryImage;
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
        public SltCCLEnvironmentConfiguration build() {
            checkNotBuilt();
            built = true;
            return c;
        }

        private void checkNotBuilt() {
            if (built) throw new IllegalStateException("already built");
        }

    }

}
