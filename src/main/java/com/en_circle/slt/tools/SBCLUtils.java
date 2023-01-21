package com.en_circle.slt.tools;

import com.en_circle.slt.plugin.environment.SltProcessStreamGobbler;
import com.en_circle.slt.plugin.environment.SltProcessStreamGobbler.WaitForOccurrence;
import com.en_circle.slt.templates.SbclVerifyTemplate;
import com.intellij.openapi.util.io.FileUtil;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class SBCLUtils {

    @SuppressWarnings("ResultOfMethodCallIgnored")
    public static boolean verifyAndInstallDependencies(String executable, String core, String quicklisp) {
        try {
            List<String> args = new ArrayList<>();
            args.add(executable);
            if (StringUtils.isNotBlank(core)) {
                args.add("--core");
                args.add(core);
            }
            args.add("--non-interactive");

            File tempTestFile = FileUtil.createTempFile("testSBCL", ".cl");
            if (tempTestFile.exists())
                tempTestFile.delete();
            FileUtils.writeStringToFile(tempTestFile, new SbclVerifyTemplate(quicklisp).render(), StandardCharsets.UTF_8);
            tempTestFile.deleteOnExit();

            args.add("--load");
            args.add(tempTestFile.getAbsolutePath());

            try {
                ProcessBuilder processBuilder = new ProcessBuilder(args.toArray(new String[0]));
                Process process = processBuilder.start();

                SltProcessStreamGobbler errorController = new SltProcessStreamGobbler(process.getErrorStream());
                WaitForOccurrence waiter = new WaitForOccurrence("SltVerified");
                errorController.addUpdateListener(waiter);
                errorController.start();
                if (!waiter.awaitFor(null, errorController, 10, TimeUnit.MINUTES)) {
                    if (process.isAlive())
                        process.destroy();
                    return false;
                }
                if (process.isAlive())
                    process.destroy();
            } finally {
                tempTestFile.delete();
            }
            return true;
        } catch (Exception ignored) {
            return false;
        }
    }

}
