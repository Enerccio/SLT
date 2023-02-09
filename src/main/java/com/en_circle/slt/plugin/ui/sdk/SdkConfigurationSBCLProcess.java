package com.en_circle.slt.plugin.ui.sdk;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.sdk.LispSdk;
import com.en_circle.slt.plugin.ui.SltGlobalUIService;
import com.en_circle.slt.plugin.ui.sdk.SdkDialogProvider.OnSave;
import com.en_circle.slt.tools.SBCLUtils;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.fileChooser.FileChooserFactory;
import com.intellij.openapi.fileChooser.FileTextField;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.impl.ProgressResult;
import com.intellij.openapi.progress.impl.ProgressRunner;
import com.intellij.openapi.progress.impl.ProgressRunner.ThreadToUse;
import com.intellij.openapi.progress.util.ProgressWindow;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.util.Disposer;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.FormBuilder;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.event.CaretListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.File;

public class SdkConfigurationSBCLProcess extends DialogWrapper {

    private final Disposable parentDisposable;
    private final LispSdk instance;
    private final OnSave onSave;
    private boolean isVerified = false;

    private JBTextField name;
    private FileTextField sbclExecutable;
    private FileTextField sbclCore;
    private FileTextField quicklispPath;

    public SdkConfigurationSBCLProcess(@NotNull Component parent, LispSdk instance, String title, OnSave onSave) {
        super(parent, true);

        this.parentDisposable = SltGlobalUIService.getInstance();
        this.instance = instance;
        this.onSave = onSave;

        setTitle(title);
        setSize(700, 0);
        init();
    }

    @Override
    protected @Nullable JComponent createCenterPanel() {
        name = new JBTextField();
        name.addCaretListener(createChangeListener());
        name.setText(instance.userName);
        if (StringUtils.isBlank(instance.userName)) {
            name.setText(SltBundle.message("slt.ui.settings.sdk.editor.name.sbcl.default"));
        }

        FileChooserDescriptor descriptor = new FileChooserDescriptor(true, false, false,
                        false, false, false);

        sbclExecutable = FileChooserFactory.getInstance()
                .createFileTextField(descriptor, true, parentDisposable);
        sbclExecutable.getField().addCaretListener(createChangeListener());
        sbclExecutable.getField().setText(instance.sbclExecutable);
        sbclCore = FileChooserFactory.getInstance()
                .createFileTextField(descriptor, true, parentDisposable);
        sbclCore.getField().addCaretListener(createChangeListener());
        sbclCore.getField().setText(instance.sbclCorePath);
        quicklispPath = FileChooserFactory.getInstance()
                .createFileTextField(descriptor, true, parentDisposable);
        quicklispPath.getField().addCaretListener(createChangeListener());
        quicklispPath.getField().setText(instance.quickLispPath);

        TextFieldWithBrowseButton sbclExecutablePicker = new TextFieldWithBrowseButton(sbclExecutable.getField());
        sbclExecutablePicker.addBrowseFolderListener(
                SltBundle.message("slt.ui.settings.sdk.editor.sbcl.executable.select"), "", null, descriptor);
        TextFieldWithBrowseButton sbclCorePicker = new TextFieldWithBrowseButton(sbclCore.getField());
        sbclCorePicker.addBrowseFolderListener(
                SltBundle.message("slt.ui.settings.sdk.editor.sbcl.core.select"), "", null, descriptor);
        TextFieldWithBrowseButton quicklispPathPicker = new TextFieldWithBrowseButton(quicklispPath.getField());
        //noinspection DialogTitleCapitalization
        quicklispPathPicker.addBrowseFolderListener(
                SltBundle.message("slt.ui.settings.sdk.editor.quicklisp.select"), "", null, descriptor);

        return new FormBuilder()
                .addLabeledComponent(SltBundle.message("slt.ui.settings.sdk.editor.name"), name, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.sdk.editor.sbcl.process.executable"),
                        sbclExecutablePicker, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.sdk.editor.sbcl.process.core"),
                        sbclCorePicker, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.sdk.editor.quicklisp"),
                        quicklispPathPicker, 1, false)
                .addComponentFillVertically(new JPanel(), 0)
                .getPanel();
    }

    private CaretListener createChangeListener() {
        return e -> isVerified = false;
    }

    private void verifySdk() {
        name.putClientProperty("JComponent.outline", null);
        sbclExecutable.getField().putClientProperty("JComponent.outline", null);
        sbclCore.getField().putClientProperty("JComponent.outline", null);
        quicklispPath.getField().putClientProperty("JComponent.outline", null);

        boolean verified = true;

        if (StringUtils.isBlank(name.getText())) {
            verified = false;
            name.putClientProperty("JComponent.outline", "error");
        }

        String executable = sbclExecutable.getField().getText();
        if (StringUtils.isBlank(executable)) {
            verified = false;
            sbclExecutable.getField().putClientProperty("JComponent.outline", "error");
        }

        String core = sbclCore.getField().getText();
        if (StringUtils.isNotBlank(core)) {
            File file = new File(core);
            if (!file.exists()) {
                verified = false;
                sbclCore.getField().putClientProperty("JComponent.outline", "error");
            }
        }

        String quicklisp = quicklispPath.getField().getText();
        if (StringUtils.isBlank(quicklisp)) {
            verified = false;
            quicklispPath.getField().putClientProperty("JComponent.outline", "error");
        } else {
            File qlFile = new File(quicklisp);
            if (!qlFile.exists() || !qlFile.isFile()) {
                verified = false;
                quicklispPath.getField().putClientProperty("JComponent.outline", "error");
            }
        }

        name.repaint();
        sbclExecutable.getField().repaint();
        sbclCore.getField().repaint();
        quicklispPath.getField().repaint();

        if (verified)
            verified = checkAndLoadSbcl(executable, core, quicklisp);
        if (!verified) {
            Messages.showErrorDialog(SltBundle.message("slt.ui.settings.sdk.editor.sbcl.process.verifying.error"),
                    SltBundle.message("slt.ui.settings.sdk.editor.verifying.error.title"));
        }

        isVerified = verified;
    }

    private boolean checkAndLoadSbcl(String executable, String core, String quicklisp) {
        ProgressWindow verifyWindow = new ProgressWindow(true, false, null,
                getRootPane(), SltBundle.message("slt.ui.settings.sdk.editor.verifying.cancel"));
        verifyWindow.setTitle(SltBundle.message("slt.ui.settings.sdk.editor.verifying.sbcl"));
        Disposer.register(parentDisposable, verifyWindow);

        ProgressResult<Boolean> result = new ProgressRunner<>(pi -> verifySbcl(pi, executable, core, quicklisp))
                .sync()
                .onThread(ThreadToUse.POOLED)
                .withProgress(verifyWindow)
                .modal()
                .submitAndGet();
        return Boolean.TRUE.equals(result.getResult());
    }

    private boolean verifySbcl(ProgressIndicator pi, String executable, String core, String quicklisp) {
        return SBCLUtils.verifyAndInstallDependencies(executable, core, quicklisp, pi);
    }

    private void save() {
        instance.userName = name.getText();
        instance.sbclExecutable = sbclExecutable.getField().getText();
        instance.sbclCorePath = sbclCore.getField().getText();
        instance.quickLispPath = quicklispPath.getField().getText();
        onSave.saveAction(instance);
        close(0);
    }

    @Override
    protected Action @NotNull [] createActions() {
        return new Action[] {
                new VerifyAction(), new SaveAction()
        };
    }

    public class VerifyAction extends AbstractAction {

        public VerifyAction() {
            super(SltBundle.message("slt.ui.settings.sdk.editor.sbcl.process.verify"));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            verifySdk();
        }
    }

    public class SaveAction extends AbstractAction {

        public SaveAction() {
            super(SltBundle.message("slt.ui.settings.sdk.editor.save"));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!isVerified) {
                Messages.showInfoMessage(SltBundle.message("slt.ui.settings.sdk.editor.notverified.title"),
                        SltBundle.message("slt.ui.settings.sdk.editor.sbcl.process.notverified.message"));
                return;
            }

            save();
        }
    }

}
