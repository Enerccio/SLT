package com.en_circle.slt.plugin.swank;

import com.en_circle.slt.plugin.swank.SwankStreamController.WaitForOccurrence;
import com.en_circle.slt.templates.InitScriptTemplate;
import com.en_circle.slt.templates.SltScriptTemplate;
import com.intellij.openapi.application.ApplicationManager;
import org.apache.commons.io.FileUtils;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

public class SwankServer {

    public static SwankServer INSTANCE = new SwankServer();

    private Process sbclProcess;

    public static void startSbcl(SwankServerConfiguration configuration) {
        INSTANCE.start(configuration);
    }

    public static void restart(SwankServerConfiguration configuration) {
        INSTANCE.stopInstance();
        INSTANCE.start(configuration);
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

    private synchronized void start(SwankServerConfiguration configuration) {
        if (sbclProcess != null)
            return;

        try {
            File sltCore = File.createTempFile("slt", ".cl");
            sltCore.deleteOnExit();
            String sltScriptTemplate = new SltScriptTemplate().render();
            FileUtils.write(sltCore, sltScriptTemplate, StandardCharsets.UTF_8);

            File serverStartSetup = File.createTempFile("startServer", ".cl");
            serverStartSetup.deleteOnExit();
            String startScriptTemplate = new InitScriptTemplate(configuration, sltCore.getAbsolutePath()).render();
            FileUtils.write(serverStartSetup, startScriptTemplate, StandardCharsets.UTF_8);

            String[] commands = new String[]{
                    configuration.getExecutablePath(),
                    "--load",
                    serverStartSetup.getName()
            };
            ProcessBuilder pb = new ProcessBuilder(commands);
            pb.directory(serverStartSetup.getParentFile());
            sbclProcess = pb.start();

            SwankStreamController errorController = new SwankStreamController(sbclProcess.getErrorStream());
            SwankStreamController outputController = new SwankStreamController(sbclProcess.getInputStream());

            WaitForOccurrence wait = new WaitForOccurrence("Swank started at port");
            errorController.addUpdateListener(wait);

            SwankServerListener listener = configuration.getListener();
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
