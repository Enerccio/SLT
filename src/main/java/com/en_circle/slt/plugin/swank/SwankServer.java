package com.en_circle.slt.plugin.swank;

import com.en_circle.slt.plugin.swank.SwankStreamController.WaitForOccurrence;
import com.intellij.openapi.application.ApplicationManager;
import org.apache.commons.io.FileUtils;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

public class SwankServer {

    public static SwankServer INSTANCE = new SwankServer();

    private Process sbclProcess;
    private SwankStreamController errorController;
    private SwankStreamController outputController;


    public static void startSbcl(String executable, int port) {
        startSbcl(executable, port, null);
    }

    public static void startSbcl(String executable, int port, SwankServerListener listener) {
        INSTANCE.start(executable, port, listener);
    }

    public static void restart(String executable, int port) {
        INSTANCE.stopInstance();
        INSTANCE.start(executable, port, null);
    }

    public static void stop() {
        INSTANCE.stopInstance();
    }

    public static Process getProcess() {
        return INSTANCE.sbclProcess;
    }

    private synchronized void stopInstance() {
        if (sbclProcess != null) {
            sbclProcess.destroy();
            sbclProcess = null;
        }
    }

    private synchronized void start(String command, int port, SwankServerListener listener) {
        if (sbclProcess != null)
            return;

        try {
            File serverStartSetup = File.createTempFile("startServer", ".cl");
            serverStartSetup.deleteOnExit();
            String serverStarter = String.format("(load \"~/quicklisp/setup.lisp\")\n" +
                    "(ql:quickload :swank)\n" +
                    "(swank:create-server :port %s :dont-close nil)\n", port);

            FileUtils.write(serverStartSetup, serverStarter, StandardCharsets.UTF_8);
            String[] commands = new String[]{
                    command,
                    "--load",
                    serverStartSetup.getName()
            };
            ProcessBuilder pb = new ProcessBuilder(commands);
            pb.directory(serverStartSetup.getParentFile());
            sbclProcess = pb.start();

            errorController = new SwankStreamController(sbclProcess.getErrorStream());
            outputController = new SwankStreamController(sbclProcess.getInputStream());

            WaitForOccurrence wait = new WaitForOccurrence("Swank started at port");
            errorController.addUpdateListener(wait);
            if (listener != null) {
                if (ApplicationManager.getApplication() != null) {
                    errorController.addUpdateListener(data ->
                            ApplicationManager.getApplication().invokeLater(() -> listener.onOutputChanged(SwankServerOutput.STDERR, data)));
                    outputController.addUpdateListener(data ->
                            ApplicationManager.getApplication().invokeLater(() -> listener.onOutputChanged(SwankServerOutput.STDOUT, data)));
                } else {
                    errorController.addUpdateListener(data -> listener.onOutputChanged(SwankServerOutput.STDERR, data));
                    outputController.addUpdateListener(data -> listener.onOutputChanged(SwankServerOutput.STDOUT, data));
                }
            }

            errorController.start();
            outputController.start();

            if (!wait.awaitFor(sbclProcess)) {
                throw new IOException("Failed to start sbcl!");
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public boolean isActive() {
        return sbclProcess != null && sbclProcess.isAlive();
    }

    public interface SwankServerListener {

        void onOutputChanged(SwankServerOutput output, String newData);

    }

    public enum SwankServerOutput {
        STDOUT, STDERR
    }

}
