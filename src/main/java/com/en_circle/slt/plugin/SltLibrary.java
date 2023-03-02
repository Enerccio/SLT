package com.en_circle.slt.plugin;

import com.en_circle.slt.tools.PluginPath;
import org.apache.commons.io.FileUtils;
import org.rauschig.jarchivelib.ArchiveFormat;
import org.rauschig.jarchivelib.Archiver;
import org.rauschig.jarchivelib.ArchiverFactory;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

public class SltLibrary {

    private static boolean loaded = false;

    public static File getLibraryInitFile() throws IOException {
        File sltPath = getSltPath();

        return new File(sltPath, "load.lisp");
    }

    public static synchronized File getSltPath() throws IOException {
        File pluginPath = PluginPath.getPluginFolder();
        File sltPath = new File(pluginPath, "slt");
        if (!sltPath.exists()) {
            sltPath.mkdirs();
            loaded = false;
        } else if (!loaded) {
            FileUtils.deleteDirectory(sltPath);
            sltPath.mkdirs();
        }

        if (!loaded) {
            extractAndCopy(sltPath);
        }
        return sltPath;
    }

    private static synchronized void extractAndCopy(File sltPath) throws IOException {
        InputStream is = SltLibrary.class.getResourceAsStream("/slt.zip");
        assert is != null;

        Archiver archiver = ArchiverFactory.createArchiver(ArchiveFormat.ZIP);
        archiver.extract(is, sltPath);
    }

}
