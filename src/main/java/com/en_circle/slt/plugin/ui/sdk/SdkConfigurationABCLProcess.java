package com.en_circle.slt.plugin.ui.sdk;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.sdk.LispSdk;
import com.en_circle.slt.plugin.ui.SltGlobalUIService;
import com.en_circle.slt.plugin.ui.sdk.SdkDialogProvider.OnSave;
import com.en_circle.slt.tools.ABCLUtils;
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

public class SdkConfigurationABCLProcess extends DialogWrapper {

    private final Disposable parentDisposable;
    private final LispSdk instance;
    private final OnSave onSave;
    private boolean isVerified = false;

    private JBTextField name;
    private FileTextField jvmExecutable;
    private JBTextField jvmArguments;
    private FileTextField jar;
    private FileTextField quicklispPath;

    public SdkConfigurationABCLProcess(@NotNull Component parent, LispSdk instance, String title, OnSave onSave) {
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
            name.setText(SltBundle.message("slt.ui.settings.sdk.editor.name.abcl.default"));
        }

        FileChooserDescriptor descriptor = new FileChooserDescriptor(true, false, false,
                        false, false, false);
        FileChooserDescriptor descriptorJar = new FileChooserDescriptor(true, false, true,
                true, false, false);

        jvmExecutable = FileChooserFactory.getInstance()
                .createFileTextField(descriptor, true, parentDisposable);
        jvmExecutable.getField().addCaretListener(createChangeListener());
        jvmExecutable.getField().setText(instance.abclJvm);
        jvmArguments = new JBTextField();
        jvmArguments.addCaretListener(createChangeListener());
        jvmArguments.setText(instance.abclJvmArgs);
        jar = FileChooserFactory.getInstance()
                .createFileTextField(descriptorJar, true, parentDisposable);
        jar.getField().addCaretListener(createChangeListener());
        jar.getField().setText(instance.abclJar);
        quicklispPath = FileChooserFactory.getInstance()
                .createFileTextField(descriptor, true, parentDisposable);
        quicklispPath.getField().addCaretListener(createChangeListener());
        quicklispPath.getField().setText(instance.quickLispPath);

        TextFieldWithBrowseButton abclExecutablePicker = new TextFieldWithBrowseButton(jvmExecutable.getField());
        abclExecutablePicker.addBrowseFolderListener(
                SltBundle.message("slt.ui.settings.sdk.editor.jvm.select"), "", null, descriptor);
        TextFieldWithBrowseButton abclJarPicker = new TextFieldWithBrowseButton(jar.getField());
        abclJarPicker.addBrowseFolderListener(
                SltBundle.message("slt.ui.settings.sdk.editor.abcl.select"), "", null, descriptorJar);
        TextFieldWithBrowseButton quicklispPathPicker = new TextFieldWithBrowseButton(quicklispPath.getField());
        //noinspection DialogTitleCapitalization
        quicklispPathPicker.addBrowseFolderListener(
                SltBundle.message("slt.ui.settings.sdk.editor.quicklisp.select"), "", null, descriptor);

        return new FormBuilder()
                .addLabeledComponent(SltBundle.message("slt.ui.settings.sdk.editor.name"), name, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.sdk.editor.abcl.jvm.executable"),
                        abclExecutablePicker, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.sdk.editor.abcl.jvm.args"),
                        jvmArguments, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.sdk.editor.abcl.jar"),
                        abclJarPicker, 1, false)
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
        jvmExecutable.getField().putClientProperty("JComponent.outline", null);
        jar.getField().putClientProperty("JComponent.outline", null);
        quicklispPath.getField().putClientProperty("JComponent.outline", null);

        boolean verified = true;

        if (StringUtils.isBlank(name.getText())) {
            verified = false;
            name.putClientProperty("JComponent.outline", "error");
        }

        String jvm = jvmExecutable.getField().getText();
        if (StringUtils.isBlank(jvm)) {
            verified = false;
            jvmExecutable.getField().putClientProperty("JComponent.outline", "error");
        }

        String jvmArgs = jvmArguments.getText();

        String jar = this.jar.getField().getText();
        if (StringUtils.isNotBlank(jar)) {
            File file = new File(jar);
            if (!file.exists()) {
                verified = false;
                this.jar.getField().putClientProperty("JComponent.outline", "error");
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
        jvmExecutable.getField().repaint();
        this.jar.getField().repaint();
        quicklispPath.getField().repaint();

        if (verified)
            verified = checkAndLoadAbclCore(jvm, jvmArgs, jar, quicklisp);
        if (!verified) {
            Messages.showErrorDialog(SltBundle.message("slt.ui.settings.sdk.editor.abcl.process.verifying.error"),
                    SltBundle.message("slt.ui.settings.sdk.editor.verifying.error.title"));
        }

        isVerified = verified;
    }

    private boolean checkAndLoadAbclCore(String jvm, String jvmArgs, String jar, String quicklisp) {
        ProgressWindow verifyWindow = new ProgressWindow(true, false, null,
                getRootPane(), SltBundle.message("slt.ui.settings.sdk.editor.verifying.cancel"));
        verifyWindow.setTitle(SltBundle.message("slt.ui.settings.sdk.editor.verifying.abcl"));
        Disposer.register(parentDisposable, verifyWindow);

        ProgressResult<Boolean> result = new ProgressRunner<>(pi -> verifyAbcl(pi, jvm, jvmArgs, jar, quicklisp))
                .sync()
                .onThread(ThreadToUse.POOLED)
                .withProgress(verifyWindow)
                .modal()
                .submitAndGet();
        return Boolean.TRUE.equals(result.getResult());
    }

    private boolean verifyAbcl(ProgressIndicator pi, String jvm, String jvmArgs, String jar, String quicklisp) {
        return ABCLUtils.verifyAndInstallDependencies(jvm, jvmArgs, jar, quicklisp, pi);
    }

    private void save() {
        instance.userName = name.getText();
        instance.abclJvm = jvmExecutable.getField().getText();
        instance.abclJvmArgs = jvmArguments.getText();
        instance.abclJar = jar.getField().getText();
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
            super(SltBundle.message("slt.ui.settings.sdk.editor.abcl.process.verify"));
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
                        SltBundle.message("slt.ui.settings.sdk.editor.abcl.process.notverified.message"));
                return;
            }

            save();
        }
    }

}
