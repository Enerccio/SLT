package com.en_circle.slt.tools.platform;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.sdk.LispSdk;
import com.en_circle.slt.tools.PluginPath;
import com.en_circle.slt.tools.SBCLUtils;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.impl.ProgressResult;
import com.intellij.openapi.progress.impl.ProgressRunner;
import com.intellij.openapi.progress.util.ProgressWindow;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.io.FileUtil;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;

import javax.swing.*;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.UUID;

public class DownloadSBCLMsiAction implements DownloadSBCLAction {

    private LispSdk sdk;
    private JComponent rootPane;
    private Disposable parentDisposable;

    @Override
    public LispSdk getConfiguredSdk() {
        return sdk;
    }

    @Override
    public void setDisposable(Disposable disposable) {
        this.parentDisposable = disposable;
    }

    @Override
    public void setRootPane(JComponent rootPane) {
        this.rootPane = rootPane;
    }

    @Override
    public void run(Runnable callback) {
        String installUuid = StringUtils.replace(UUID.randomUUID().toString(), "-", "");
        SBCLInstallation installation = downloadSbcl(installUuid);
        if (installation != null) {
            String quicklispPath = downloadQuicklisp(installUuid);
            if (quicklispPath != null) {
                String quicklispSetupFile = installQuicklisp(installUuid, installation, quicklispPath);
                if (quicklispSetupFile != null) {
                    verifySBCL(installation.executableFilePath, installation.coreFilePath, quicklispSetupFile, installUuid);
                }
            }
        }

        callback.run();
    }

    private SBCLInstallation downloadSbcl(String uuidInstall) {
        ProgressWindow verifyWindow = new ProgressWindow(true, false, null,
                rootPane, SltBundle.message("slt.ui.settings.sdk.download.cancel"));
        verifyWindow.setTitle(SltBundle.message("slt.ui.settings.sdk.download.downloading"));
        Disposer.register(parentDisposable, verifyWindow);

        ProgressResult<SBCLInstallation> result = new ProgressRunner<>(pi -> downloadSbcl(uuidInstall, pi))
                .sync()
                .onThread(ProgressRunner.ThreadToUse.POOLED)
                .withProgress(verifyWindow)
                .modal()
                .submitAndGet();
        return result.getResult();
    }

    private SBCLInstallation downloadSbcl(String uuidInstall, ProgressIndicator pi) {
        try {
            URL url = new URL("https://sourceforge.net/projects/sbcl/files/sbcl/2.3.0/sbcl-2.3.0-x86-64-windows-binary.msi/download");
            File tempFile = FileUtil.createTempFile(PluginPath.getPluginFolder(), "sbclinstall", ".msi");
            if (!tempFile.delete()) {
                return null;
            }
            IOUtils.copy(url, tempFile);
            if (pi.isCanceled())
                return null;

            File sbclDirName = new File(PluginPath.getPluginFolder(), uuidInstall);
            if (!sbclDirName.mkdirs()) {
                return null;
            }

            ProcessBuilder pb = new ProcessBuilder("msiexec", "/a", tempFile.getAbsolutePath(), "/qb",
                    "TARGETDIR="+getMSDOSName(sbclDirName.getAbsolutePath()));
            Process p = pb.start();
            p.waitFor();

            File sbclFolder = new File(sbclDirName, "PFiles\\Steel Bank Common Lisp");
            File exe = new File(sbclFolder, "sbcl.exe");
            File core = new File(sbclFolder, "sbcl.core");

            if (!exe.exists() || !core.exists()) {
                return null;
            }

            if (pi.isCanceled())
                return null;

            SBCLInstallation installation = new SBCLInstallation();
            installation.executableFilePath = exe.getAbsolutePath();
            installation.coreFilePath = core.getAbsolutePath();

            if (pi.isCanceled())
                return null;
            return installation;
        } catch (Exception ignored) {

        }
        return null;
    }

    public static String getMSDOSName(String path) throws IOException, InterruptedException {
        Process process = new ProcessBuilder().command("cmd", "/c", "for %I in (\"" + path + "\") do @echo %~fsI").start();
        process.waitFor();
        try (BufferedReader br = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
            return br.readLine();
        }
    }

    private String downloadQuicklisp(String uuidInstall) {
        ProgressWindow verifyWindow = new ProgressWindow(true, false, null,
                rootPane, SltBundle.message("slt.ui.settings.sdk.download.cancel"));
        verifyWindow.setTitle(SltBundle.message("slt.ui.settings.sdk.download.downloading.quicklisp.title"));
        Disposer.register(parentDisposable, verifyWindow);

        ProgressResult<String> result = new ProgressRunner<>(pi -> downloadQuicklisp(uuidInstall, pi))
                .sync()
                .onThread(ProgressRunner.ThreadToUse.POOLED)
                .withProgress(verifyWindow)
                .modal()
                .submitAndGet();
        return result.getResult();
    }

    private String downloadQuicklisp(String uuidInstall, ProgressIndicator pi) {
        try {
            URL url = new URL("http://beta.quicklisp.org/quicklisp.lisp");
            File tempFile = FileUtil.createTempFile(PluginPath.getPluginFolder(), "ql", ".lisp");
            if (!tempFile.delete()) {
                return null;
            }
            IOUtils.copy(url, tempFile);
            if (pi.isCanceled())
                return null;

            return tempFile.getAbsolutePath();
        } catch (Exception ignored) {

        }
        return null;
    }

    private String installQuicklisp(String uuidInstall, SBCLInstallation installation, String quicklispPath) {
        ProgressWindow verifyWindow = new ProgressWindow(true, false, null,
                rootPane, SltBundle.message("slt.ui.settings.sdk.download.cancel"));
        verifyWindow.setTitle(SltBundle.message("slt.ui.settings.sdk.download.installing.quicklisp.title"));
        Disposer.register(parentDisposable, verifyWindow);

        ProgressResult<String> result = new ProgressRunner<>(pi -> installQuicklisp(uuidInstall, installation, quicklispPath, pi))
                .sync()
                .onThread(ProgressRunner.ThreadToUse.POOLED)
                .withProgress(verifyWindow)
                .modal()
                .submitAndGet();
        return result.getResult();
    }

    private String installQuicklisp(String uuidInstall, SBCLInstallation installation, String quicklispPath, ProgressIndicator pi) {
        try {
            File sbclDirName = new File(PluginPath.getPluginFolder(), uuidInstall);
            File quicklispInstallPath = new File(sbclDirName, "quicklisp");
            if (!quicklispInstallPath.mkdirs()) {
                return null;
            }

            String targetFolder = quicklispInstallPath.getAbsolutePath();
            targetFolder = StringUtils.replace(targetFolder, "\\", "\\\\");

            String evalForm = String.format("(quicklisp-quickstart:install :path \\\"%s\\\")",
                    targetFolder);

            Process p = new ProcessBuilder(installation.executableFilePath, "--core", installation.coreFilePath,
                    "--non-interactive", "--load", quicklispPath, "--eval", evalForm, "--quit")
                    .inheritIO()
                    .start();

            int res = p.waitFor();
            if (res == 0) {
                File qlSetupFile = new File(quicklispInstallPath, "setup.lisp");
                if (qlSetupFile.exists() && qlSetupFile.isFile()) {
                    return qlSetupFile.getAbsolutePath();
                }
            }
        } catch (Exception ignored) {

        }
        return null;
    }

    private void verifySBCL(String executable, String core, String quicklisp, String installUuid) {
        ProgressWindow verifyWindow = new ProgressWindow(true, false, null,
                rootPane, SltBundle.message("slt.ui.settings.sdk.editor.verifying.cancel"));
        verifyWindow.setTitle(SltBundle.message("slt.ui.settings.sdk.editor.verifying"));
        Disposer.register(parentDisposable, verifyWindow);

        ProgressResult<Boolean> result = new ProgressRunner<>(pi -> verifySbcl(pi, executable, core, quicklisp))
                .sync()
                .onThread(ProgressRunner.ThreadToUse.POOLED)
                .withProgress(verifyWindow)
                .modal()
                .submitAndGet();
        if (Boolean.TRUE.equals(result.getResult())) {
            sdk = new LispSdk();
            sdk.userName = "SBCL 2.3.2";
            sdk.uuid = installUuid;
            sdk.sbclExecutable = executable;
            sdk.sbclCorePath = core;
            sdk.quickLispPath = quicklisp;
        }
    }

    private boolean verifySbcl(ProgressIndicator pi, String executable, String core, String quicklisp) {
        return SBCLUtils.verifyAndInstallDependencies(executable, core, quicklisp, pi);
    }

    private static class SBCLInstallation {
        String executableFilePath;
        String coreFilePath;

    }

}
