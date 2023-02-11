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

public class SltAllegroCLEnvironment extends SltLispEnvironmentProcess  {

    private int port;

    @Override
    public int getSwankPort() {
        return port;
    }

    @Override
    public SltLispProcessInformation getInformation() {
        return new SltAllegroCLLispProcessInformation();
    }

    @Override
    public Environment getType() {
        return Environment.ALLEGRO_PROCESS;
    }

    @Override
    protected Object prepareProcessEnvironment(SltLispEnvironmentProcessConfiguration configuration) throws SltProcessException {
        SltAllegroCLEnvironmentConfiguration c = getConfiguration(configuration);
        AllegroCLEnvironment e = new AllegroCLEnvironment();
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
            String startScriptTemplate = new AllegroCLInitScriptTemplate(c, sltCorePath, e.port).render();
            FileUtils.write(e.serverStartSetup, startScriptTemplate, StandardCharsets.UTF_8);

            tempDir.deleteOnExit();
        } catch (Exception ex) {
            throw new SltProcessException(ex);
        }
        return e;
    }

    @Override
    protected File getProcessWorkDirectory(SltLispEnvironmentProcessConfiguration configuration, Object environment) throws SltProcessException {
        SltAllegroCLEnvironmentConfiguration c = getConfiguration(configuration);
        AllegroCLEnvironment e = getEnvironment(environment);

        return PluginPath.getPluginFolder();
    }

    @Override
    protected String[] getProcessCommand(SltLispEnvironmentProcessConfiguration configuration, Object environment) throws SltProcessException {
        SltAllegroCLEnvironmentConfiguration c = getConfiguration(configuration);
        AllegroCLEnvironment e = getEnvironment(environment);
        this.port = e.port;

        List<String> parameters = new ArrayList<>();
        SltUtils.addExecutable(parameters, c.getExecutablePath());
        if (StringUtils.isNotBlank(c.getMemoryImage())) {
            parameters.add("-I");
            parameters.add(c.getMemoryImage());
        }

        parameters.add("-L");
        parameters.add(e.serverStartSetup.getAbsolutePath());

        return parameters.toArray(new String[0]);
    }

    @Override
    protected ProcessInitializationWaiter waitForFullInitialization(SltLispEnvironmentProcessConfiguration configuration, Object environment) throws SltProcessException {
        SltAllegroCLEnvironmentConfiguration c = getConfiguration(configuration);
        AllegroCLEnvironment e = getEnvironment(environment);

        WaitForOccurrence wait = new WaitForOccurrence("Swank started at port");
        outputController.addUpdateListener(wait);

        return wait;
    }

    private SltAllegroCLEnvironmentConfiguration getConfiguration(SltLispEnvironmentProcessConfiguration configuration) throws SltProcessException {
        if (!(configuration instanceof SltAllegroCLEnvironmentConfiguration))
            throw new SltProcessException("Configuration must be SltAllegroCLEnvironmentConfiguration");
        return (SltAllegroCLEnvironmentConfiguration) configuration;
    }

    private AllegroCLEnvironment getEnvironment(Object environment) {
        assert (environment instanceof AllegroCLEnvironment);

        return (AllegroCLEnvironment) environment;
    }

    private int getFreePort() {
        var freePort = 0;
        try (ServerSocket s = new ServerSocket(0)) {
            freePort = s.getLocalPort();
        } catch (Exception ignored) {

        }

        return freePort;
    }


    private class SltAllegroCLLispProcessInformation implements SltLispProcessInformation {

        @Override
        public String getPid() {
            return "" + process.pid();
        }
    }

    private static class AllegroCLEnvironment {

        File sltCore;
        File serverStartSetup;
        int port;

    }

    private static class AllegroCLInitScriptTemplate extends Template {

        public AllegroCLInitScriptTemplate(SltAllegroCLEnvironmentConfiguration configuration, String sltCoreScript, int port) {
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
            add("qlpath", quicklispPath);
            add("port", "" + port);
            add("cwd", cwd);
            add("corefile", sltCoreScript);
            add("interpret", LispInterpret.ALLEGRO.symbolName);
        }

        @Override
        protected String getFilePath() {
            return "initscript.allegro.cl";
        }
    }
}
