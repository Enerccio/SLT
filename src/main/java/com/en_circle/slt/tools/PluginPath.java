package com.en_circle.slt.tools;

import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.util.SystemInfo;

import java.io.File;

public class PluginPath {

    private static final String PLUGIN_SUFFIX = ".slt";

    @SuppressWarnings("ResultOfMethodCallIgnored")
    public static File getPluginFolder() {
        String path = PathManager.getConfigPath();
        File configPath = new File(path);
        File pluginFolder;
        if (configPath.exists()) {
            pluginFolder = new File(configPath, PLUGIN_SUFFIX);
            pluginFolder.mkdirs();
        } else {
            if (SystemInfo.isWindows) {
                pluginFolder = new File(new File(System.getProperty("user.home")), "Application Data\\" + PLUGIN_SUFFIX);
                pluginFolder.mkdirs();
            } else if (SystemInfo.isMac) {
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
