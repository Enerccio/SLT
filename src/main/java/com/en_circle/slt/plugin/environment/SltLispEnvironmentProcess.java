package com.en_circle.slt.plugin.environment;

import com.en_circle.slt.plugin.environment.SltProcessStreamGobbler.ProcessInitializationWaiter;
import com.intellij.openapi.application.ApplicationManager;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public abstract class SltLispEnvironmentProcess extends SltLispEnvironmentBase {

    protected Process process;
    protected SltProcessStreamGobbler errorController;
    protected SltProcessStreamGobbler outputController;
    protected List<SltLispOutputChangedListener> listeners = new ArrayList<>();

    protected abstract Object prepareProcessEnvironment(SltLispEnvironmentProcessConfiguration configuration) throws SltProcessException;
    protected abstract File getProcessWorkDirectory(SltLispEnvironmentProcessConfiguration configuration, Object environment) throws SltProcessException;
    protected abstract String[] getProcessCommand(SltLispEnvironmentProcessConfiguration configuration, Object environment) throws SltProcessException;
    protected abstract ProcessInitializationWaiter waitForFullInitialization(SltLispEnvironmentProcessConfiguration configuration, Object environment) throws SltProcessException;

    @Override
    public boolean isActive() {
        return process != null && process.isAlive();
    }

    public void start(SltLispEnvironmentConfiguration configuration) throws SltProcessException {
        if (process != null)
            return;
        if (!(configuration instanceof SltLispEnvironmentProcessConfiguration processConfiguration))
            throw new SltProcessException("Configuration incorrect");

        try {
            Object environment = prepareProcessEnvironment(processConfiguration);

            ProcessBuilder pb = new ProcessBuilder(getProcessCommand(processConfiguration, environment));
            pb.directory(getProcessWorkDirectory(processConfiguration, environment));
            process = pb.start();

            errorController = new SltProcessStreamGobbler(process.getErrorStream());
            outputController = new SltProcessStreamGobbler(process.getInputStream());

            ProcessInitializationWaiter waiter = waitForFullInitialization(processConfiguration, environment);

            SltLispOutputChangedListener listener = processConfiguration.getListener();
            if (listener != null) {
                if (ApplicationManager.getApplication() != null) {
                    errorController.addUpdateListener(data ->
                            ApplicationManager.getApplication().invokeLater(() -> listener.onOutputChanged(SltOutput.STDERR, data)));
                    outputController.addUpdateListener(data ->
                            ApplicationManager.getApplication().invokeLater(() -> listener.onOutputChanged(SltOutput.STDOUT, data)));
                } else {
                    errorController.addUpdateListener(data -> listener.onOutputChanged(SltOutput.STDERR, data));
                    outputController.addUpdateListener(data -> listener.onOutputChanged(SltOutput.STDOUT, data));
                }
            }

            errorController.start();
            outputController.start();

            waiter.awaitFor(process);
        } catch (SltProcessException e) {
            throw e;
        } catch (Exception e) {
            throw new SltProcessException(e);
        }
    }
    public void stop() throws SltProcessException {
        if (process != null) {
            process.destroy();
            process = null;
        }
    }

    public interface SltLispEnvironmentProcessConfiguration extends SltLispEnvironmentConfiguration {

    }

}
