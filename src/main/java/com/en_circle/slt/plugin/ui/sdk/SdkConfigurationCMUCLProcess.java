package com.en_circle.slt.plugin.ui.sdk;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.sdk.LispSdk;
import com.en_circle.slt.plugin.ui.SltGlobalUIService;
import com.en_circle.slt.plugin.ui.sdk.SdkDialogProvider.OnSave;
import com.en_circle.slt.tools.CMUCLUtils;
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

public class SdkConfigurationCMUCLProcess extends DialogWrapper {

    private final Disposable parentDisposable;
    private final LispSdk instance;
    private final OnSave onSave;
    private boolean isVerified = false;

    private JBTextField name;
    private FileTextField cmuclExecutable;
    private FileTextField cmuclImage;
    private FileTextField quicklispPath;

    public SdkConfigurationCMUCLProcess(@NotNull Component parent, LispSdk instance, String title, OnSave onSave) {
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
            name.setText(SltBundle.message("slt.ui.settings.sdk.editor.name.cmucl.default"));
        }

        FileChooserDescriptor descriptor = new FileChooserDescriptor(true, false, false,
                        false, false, false);

        cmuclExecutable = FileChooserFactory.getInstance()
                .createFileTextField(descriptor, true, parentDisposable);
        cmuclExecutable.getField().addCaretListener(createChangeListener());
        cmuclExecutable.getField().setText(instance.sbclExecutable);
        cmuclImage = FileChooserFactory.getInstance()
                .createFileTextField(descriptor, true, parentDisposable);
        cmuclImage.getField().addCaretListener(createChangeListener());
        cmuclImage.getField().setText(instance.sbclCorePath);
        quicklispPath = FileChooserFactory.getInstance()
                .createFileTextField(descriptor, true, parentDisposable);
        quicklispPath.getField().addCaretListener(createChangeListener());
        quicklispPath.getField().setText(instance.quickLispPath);

        TextFieldWithBrowseButton cmuclExecutablePicker = new TextFieldWithBrowseButton(cmuclExecutable.getField());
        cmuclExecutablePicker.addBrowseFolderListener(null,
            descriptor.withTitle(SltBundle.message("slt.ui.settings.sdk.editor.cmucl.executable.select")).withDescription(""));
        TextFieldWithBrowseButton cmuclImagePicker = new TextFieldWithBrowseButton(cmuclImage.getField());
        cmuclImagePicker.addBrowseFolderListener(null,
                descriptor.withTitle(SltBundle.message("slt.ui.settings.sdk.editor.cmucl.image.select")).withDescription(""));
        TextFieldWithBrowseButton quicklispPathPicker = new TextFieldWithBrowseButton(quicklispPath.getField());
        //noinspection DialogTitleCapitalization
        quicklispPathPicker.addBrowseFolderListener(null,
                descriptor.withTitle(SltBundle.message("slt.ui.settings.sdk.editor.quicklisp.select")).withDescription(""));

        return new FormBuilder()
                .addLabeledComponent(SltBundle.message("slt.ui.settings.sdk.editor.name"), name, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.sdk.editor.cmucl.process.executable"),
                        cmuclExecutablePicker, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.sdk.editor.cmucl.process.image"),
                        cmuclImagePicker, 1, false)
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
        cmuclExecutable.getField().putClientProperty("JComponent.outline", null);
        cmuclImage.getField().putClientProperty("JComponent.outline", null);
        quicklispPath.getField().putClientProperty("JComponent.outline", null);

        boolean verified = true;

        if (StringUtils.isBlank(name.getText())) {
            verified = false;
            name.putClientProperty("JComponent.outline", "error");
        }

        String executable = cmuclExecutable.getField().getText();
        if (StringUtils.isBlank(executable)) {
            verified = false;
            cmuclExecutable.getField().putClientProperty("JComponent.outline", "error");
        }

        String core = cmuclImage.getField().getText();
        if (StringUtils.isNotBlank(core)) {
            File file = new File(core);
            if (!file.exists()) {
                verified = false;
                cmuclImage.getField().putClientProperty("JComponent.outline", "error");
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
        cmuclExecutable.getField().repaint();
        cmuclImage.getField().repaint();
        quicklispPath.getField().repaint();

        if (verified)
            verified = checkAndLoadCMUCL(executable, core, quicklisp);
        if (!verified) {
            Messages.showErrorDialog(SltBundle.message("slt.ui.settings.sdk.editor.cmucl.process.verifying.error"),
                    SltBundle.message("slt.ui.settings.sdk.editor.verifying.error.title"));
        }

        isVerified = verified;
    }

    private boolean checkAndLoadCMUCL(String executable, String memory, String quicklisp) {
        ProgressWindow verifyWindow = new ProgressWindow(true, false, null,
                getRootPane(), SltBundle.message("slt.ui.settings.sdk.editor.verifying.cancel"));
        verifyWindow.setTitle(SltBundle.message("slt.ui.settings.sdk.editor.verifying.cmucl"));
        Disposer.register(parentDisposable, verifyWindow);

        ProgressResult<Boolean> result = new ProgressRunner<>(pi -> verifyCMUCL(pi, executable, memory, quicklisp))
                .sync()
                .onThread(ThreadToUse.POOLED)
                .withProgress(verifyWindow)
                .modal()
                .submitAndGet();
        return Boolean.TRUE.equals(result.getResult());
    }

    private boolean verifyCMUCL(ProgressIndicator pi, String executable, String core, String quicklisp) {
        return CMUCLUtils.verifyAndInstallDependencies(executable, core, quicklisp, pi);
    }

    private void save() {
        instance.userName = name.getText();
        instance.cmuclExecutable = cmuclExecutable.getField().getText();
        instance.cmuclMemoryImage = cmuclImage.getField().getText();
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
            super(SltBundle.message("slt.ui.settings.sdk.editor.cmucl.process.verify"));
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
                        SltBundle.message("slt.ui.settings.sdk.editor.cmucl.process.notverified.message"));
                return;
            }

            save();
        }
    }

}
