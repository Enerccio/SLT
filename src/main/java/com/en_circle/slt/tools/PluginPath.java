package com.en_circle.slt.tools;

import com.intellij.openapi.application.PathManager;
import jnr.posix.util.Platform;

import java.io.File;

public class PluginPath {

    private static final String PLUGIN_SUFFIX = ".slt";

    public static File getPluginFolder() {
        String path = PathManager.getConfigPath();
        File configPath = new File(path);
        if (configPath.exists()) {
            // Root jetbrains config folder
            configPath = configPath.getParentFile();
        }
        File pluginFolder;
        if (configPath.exists()) {
            pluginFolder = new File(configPath, PLUGIN_SUFFIX);
            pluginFolder.mkdirs();
        } else {
            if (Platform.IS_WINDOWS) {
                pluginFolder = new File(new File(System.getProperty("user.home")), "Application Data\\" + PLUGIN_SUFFIX);
                pluginFolder.mkdirs();
            } else if (Platform.IS_MAC) {
                pluginFolder = new File(new File(System.getProperty("user.home")), PLUGIN_SUFFIX);
                pluginFolder.mkdirs();
            } else {
                pluginFolder = new File(new File(System.getProperty("user.home")), PLUGIN_SUFFIX);
                pluginFolder.mkdirs();
            }
        }
        return pluginFolder;
    }

}
