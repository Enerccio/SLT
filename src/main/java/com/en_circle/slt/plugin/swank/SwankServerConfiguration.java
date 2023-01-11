package com.en_circle.slt.plugin.swank;

import com.en_circle.slt.plugin.swank.SwankServer.SwankServerListener;

public class SwankServerConfiguration {

    private String executablePath = "sbcl";
    private String quicklispStartScript = "~/quicklisp/setup.lisp";
    private int port = 4005;
    private String projectDirectory = "/tmp";
    private SwankServerListener listener = null;

    private SwankServerConfiguration() {

    }

    public String getExecutablePath() {
        return executablePath;
    }

    public String getQuicklispStartScript() {
        return quicklispStartScript;
    }

    public int getPort() {
        return port;
    }

    public String getProjectDirectory() {
        return projectDirectory;
    }

    public SwankServerListener getListener() {
        return listener;
    }

    public static class Builder {

        private final SwankServerConfiguration c = new SwankServerConfiguration();
        private boolean built = false;

        public Builder setExecutable(String executable) {
            checkNotBuilt();

            c.executablePath = executable;
            return this;
        }

        public Builder setQuicklispStartScriptPath(String quicklispStartScript) {
            checkNotBuilt();

            c.quicklispStartScript = quicklispStartScript;
            return this;
        }

        public Builder setPort(int port) {
            checkNotBuilt();
            assert(port >= 0 && port < 65536);

            c.port = port;
            return this;
        }

        public Builder setProjectDirectory(String projectDirectory) {
            checkNotBuilt();

            c.projectDirectory = projectDirectory;
            return this;
        }

        public Builder setListener(SwankServerListener listener) {
            checkNotBuilt();

            c.listener = listener;
            return this;
        }

        public SwankServerConfiguration build() {
            checkNotBuilt();
            built = true;
            return c;
        }

        private void checkNotBuilt() {
            if (built) throw new IllegalStateException("already built");
        }

    }

}
