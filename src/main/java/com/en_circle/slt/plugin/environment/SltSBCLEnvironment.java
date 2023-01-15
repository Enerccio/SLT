package com.en_circle.slt.plugin.environment;

import com.en_circle.slt.plugin.environment.SltProcessStreamGobbler.ProcessInitializationWaiter;
import com.en_circle.slt.plugin.environment.SltProcessStreamGobbler.WaitForOccurrence;
import com.en_circle.slt.templates.SltScriptTemplate;
import org.apache.commons.io.FileUtils;
import org.watertemplate.Template;

import java.io.File;
import java.nio.charset.StandardCharsets;

public class SltSBCLEnvironment extends SltLispEnvironmentProcess  {

    @Override
    public SltLispProcessInformation getInformation() {
        return new SltSBCLLispProcessInformation();
    }

    @Override
    protected Object prepareProcessEnvironment(SltLispEnvironmentProcessConfiguration configuration) throws SltProcessException {
        SltSBCLEnvironmentConfiguration c = getConfiguration(configuration);
        SBCLEnvironment e = new SBCLEnvironment();
        try {
            e.sltCore = File.createTempFile("slt", ".cl");
            e.sltCore.deleteOnExit();
            String sltScriptTemplate = new SltScriptTemplate().render();
            FileUtils.write(e.sltCore, sltScriptTemplate, StandardCharsets.UTF_8);

            e.serverStartSetup = File.createTempFile("startServer", ".cl");
            e.serverStartSetup.deleteOnExit();
            String startScriptTemplate = new SBCLInitScriptTemplate(c, e.sltCore.getAbsolutePath()).render();
            FileUtils.write(e.serverStartSetup, startScriptTemplate, StandardCharsets.UTF_8);
        } catch (Exception ex) {
            throw new SltProcessException(ex);
        }
        return e;
    }

    @Override
    protected File getProcessWorkDirectory(SltLispEnvironmentProcessConfiguration configuration, Object environment) throws SltProcessException {
        SltSBCLEnvironmentConfiguration c = getConfiguration(configuration);
        SBCLEnvironment e = getEnvironment(environment);

        return e.serverStartSetup.getParentFile();
    }

    @Override
    protected String[] getProcessCommand(SltLispEnvironmentProcessConfiguration configuration, Object environment) throws SltProcessException {
        SltSBCLEnvironmentConfiguration c = getConfiguration(configuration);
        SBCLEnvironment e = getEnvironment(environment);

        return new String[]{
                c.getExecutablePath(),
                "--load",
                e.serverStartSetup.getName()
        };
    }

    @Override
    protected ProcessInitializationWaiter waitForFullInitialization(SltLispEnvironmentProcessConfiguration configuration, Object environment) throws SltProcessException {
        SltSBCLEnvironmentConfiguration c = getConfiguration(configuration);
        SBCLEnvironment e = getEnvironment(environment);

        WaitForOccurrence wait = new WaitForOccurrence("Swank started at port");
        errorController.addUpdateListener(wait);

        return wait;
    }

    private SltSBCLEnvironmentConfiguration getConfiguration(SltLispEnvironmentProcessConfiguration configuration) throws SltProcessException {
        if (!(configuration instanceof SltSBCLEnvironmentConfiguration))
            throw new SltProcessException("Configuration must be SltSBCLEnvironmentConfiguration");
        return (SltSBCLEnvironmentConfiguration) configuration;
    }

    private SBCLEnvironment getEnvironment(Object environment) {
        assert (environment instanceof SBCLEnvironment);

        return (SBCLEnvironment) environment;
    }

    private class SltSBCLLispProcessInformation implements SltLispProcessInformation {

        @Override
        public String getPid() {
            return "" + process.pid();
        }
    }

    private static class SBCLEnvironment {

        File sltCore;
        File serverStartSetup;

    }

    private static class SBCLInitScriptTemplate extends Template {

        public SBCLInitScriptTemplate(SltSBCLEnvironmentConfiguration configuration, String sltCoreScript) {
            add("qlpath", configuration.getQuicklispStartScript());
            add("port", "" + configuration.getPort());
            add("cwd", configuration.getProjectDirectory());
            add("sbclcorefile", sltCoreScript);
        }

        @Override
        protected String getFilePath() {
            return "initscript.cl";
        }
    }
}
