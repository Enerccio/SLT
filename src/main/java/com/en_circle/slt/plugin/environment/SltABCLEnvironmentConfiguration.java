package com.en_circle.slt.plugin.environment;


import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltLispOutputChangedListener;
import com.en_circle.slt.plugin.environment.SltLispEnvironmentProcess.SltLispEnvironmentProcessConfiguration;

public class SltABCLEnvironmentConfiguration implements SltLispEnvironmentProcessConfiguration {

    private String jvmPath = "java";
    private String jvmArgs = "";
    private String abclJar = "";
    private String quicklispStartScript = "~/quicklisp/setup.lisp";
    private String projectDirectory = "/tmp";
    private SltLispOutputChangedListener listener = null;

    private SltABCLEnvironmentConfiguration() {

    }

    public String getJvmPath() {
        return jvmPath;
    }

    public String getJvmArgs() {
        return jvmArgs;
    }

    public String getAbclJar() {
        return abclJar;
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

    public static class Builder implements SltLispEnvironmentConfiguration.Builder<Builder, SltABCLEnvironmentConfiguration> {

        private final SltABCLEnvironmentConfiguration c = new SltABCLEnvironmentConfiguration();
        private boolean built = false;

        public Builder setJvm(String executable) {
            checkNotBuilt();

            c.jvmPath = executable;
            return this;
        }

        public Builder setJvmArgs(String args) {
            checkNotBuilt();

            c.jvmArgs = args;
            return this;
        }

        public Builder setAbclJar(String jar) {
            checkNotBuilt();

            c.abclJar = jar;
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
        public SltABCLEnvironmentConfiguration build() {
            checkNotBuilt();
            built = true;
            return c;
        }

        private void checkNotBuilt() {
            if (built) throw new IllegalStateException("already built");
        }

    }

}
