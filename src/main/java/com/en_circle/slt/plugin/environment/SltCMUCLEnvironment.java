package com.en_circle.slt.plugin.environment;

import com.en_circle.slt.plugin.SltLibrary;
import com.en_circle.slt.plugin.environment.SltProcessStreamGobbler.ProcessInitializationWaiter;
import com.en_circle.slt.plugin.environment.SltProcessStreamGobbler.WaitForOccurrence;
import com.en_circle.slt.tools.PluginPath;
import com.en_circle.slt.tools.SltUtils;
import com.intellij.openapi.util.io.FileUtil;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.watertemplate.Template;

import java.io.File;
import java.io.IOException;
import java.net.ServerSocket;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

public class SltCMUCLEnvironment extends SltLispEnvironmentProcess  {

    private int port;

    @Override
    public int getSwankPort() {
        return port;
    }

    @Override
    public SltLispProcessInformation getInformation() {
        return new SltCMUCLLispProcessInformation();
    }

    @Override
    public Environment getType() {
        return Environment.CMUCL_PROCESS;
    }

    @Override
    protected Object prepareProcessEnvironment(SltLispEnvironmentProcessConfiguration configuration) throws SltProcessException {
        SltCMUCLEnvironmentConfiguration c = getConfiguration(configuration);
        CMUCLEnvironment e = new CMUCLEnvironment();
        try {
            e.port = getFreePort();
            if (e.port == 0) {
                throw new IOException("no free port available");
            }

            File tempDir = FileUtil.createTempDirectory(PluginPath.getPluginFolder(),
                    "SLTinit", "");

            e.sltCore = SltLibrary.getLibraryInitFile();

            e.serverStartSetup = new File(tempDir, "startServer.cl");
            e.serverStartSetup.deleteOnExit();
            String sltCorePath = e.sltCore.getAbsolutePath();
            String swankPath = new File(new File(SltLibrary.getSltPath(), "libs"), "swank").getAbsolutePath();
            String eclectorPath = new File(new File(SltLibrary.getSltPath(), "libs"), "eclector").getAbsolutePath();
            String startScriptTemplate = new CMUCLInitScriptTemplate(c, sltCorePath, swankPath, eclectorPath, e.port).render();
            FileUtils.write(e.serverStartSetup, startScriptTemplate, StandardCharsets.UTF_8);

            tempDir.deleteOnExit();
        } catch (Exception ex) {
            throw new SltProcessException(ex);
        }
        return e;
    }

    @Override
    protected File getProcessWorkDirectory(SltLispEnvironmentProcessConfiguration configuration, Object environment) throws SltProcessException {
        SltCMUCLEnvironmentConfiguration c = getConfiguration(configuration);
        CMUCLEnvironment e = getEnvironment(environment);

        return e.serverStartSetup.getParentFile();
    }

    @Override
    protected String[] getProcessCommand(SltLispEnvironmentProcessConfiguration configuration, Object environment) throws SltProcessException {
        SltCMUCLEnvironmentConfiguration c = getConfiguration(configuration);
        CMUCLEnvironment e = getEnvironment(environment);
        this.port = e.port;

        List<String> parameters = new ArrayList<>();
        SltUtils.addExecutable(parameters, c.getExecutablePath());
        if (StringUtils.isNotBlank(c.getMemoryImage())) {
            parameters.add("-core");
            parameters.add(c.getMemoryImage());
        }

        parameters.add("-load");
        parameters.add(e.serverStartSetup.getName());

        return parameters.toArray(new String[0]);
    }

    @Override
    protected ProcessInitializationWaiter waitForFullInitialization(SltLispEnvironmentProcessConfiguration configuration, Object environment) throws SltProcessException {
        SltCMUCLEnvironmentConfiguration c = getConfiguration(configuration);
        CMUCLEnvironment e = getEnvironment(environment);

        WaitForOccurrence wait = new WaitForOccurrence("Swank started at port");
        errorController.addUpdateListener(wait);

        return wait;
    }

    private SltCMUCLEnvironmentConfiguration getConfiguration(SltLispEnvironmentProcessConfiguration configuration) throws SltProcessException {
        if (!(configuration instanceof SltCMUCLEnvironmentConfiguration))
            throw new SltProcessException("Configuration must be SltCMUCLEnvironmentConfiguration");
        return (SltCMUCLEnvironmentConfiguration) configuration;
    }

    private CMUCLEnvironment getEnvironment(Object environment) {
        assert (environment instanceof CMUCLEnvironment);

        return (CMUCLEnvironment) environment;
    }

    private int getFreePort() {
        var freePort = 0;
        try (ServerSocket s = new ServerSocket(0)) {
            freePort = s.getLocalPort();
        } catch (Exception ignored) {

        }

        return freePort;
    }


    private class SltCMUCLLispProcessInformation implements SltLispProcessInformation {

        @Override
        public String getPid() {
            return "" + process.pid();
        }
    }

    private static class CMUCLEnvironment {

        File sltCore;
        File serverStartSetup;
        int port;

    }

    private static class CMUCLInitScriptTemplate extends Template {

        public CMUCLInitScriptTemplate(SltCMUCLEnvironmentConfiguration configuration, String sltCoreScript,
                                       String swankPath, String eclectorPath, int port) {
            String quicklispPath = configuration.getQuicklispStartScript();
            if (quicklispPath.contains("\\")) {
                quicklispPath = StringUtils.replace(quicklispPath, "\\", "\\\\");
            }
            String cwd = configuration.getProjectDirectory();
            if (cwd.contains("\\")) {
                cwd = StringUtils.replace(cwd, "\\", "\\\\");
            }
            if (sltCoreScript.contains("\\")) {
                sltCoreScript = StringUtils.replace(sltCoreScript, "\\", "\\\\");
            }
            if (swankPath.contains("\\")) {
                swankPath = StringUtils.replace(swankPath, "\\", "\\\\");
            }
            if (eclectorPath.contains("\\")) {
                eclectorPath = StringUtils.replace(eclectorPath, "\\", "\\\\");
            }
            add("qlpath", quicklispPath);
            add("port", "" + port);
            add("cwd", cwd);
            add("corefile", sltCoreScript);
            add("swankPath", swankPath);
            add("eclectorPath", eclectorPath);
            add("interpret", LispInterpret.CMUCL.symbolName);
        }

        @Override
        protected String getFilePath() {
            return "initscript.cl";
        }
    }
}
