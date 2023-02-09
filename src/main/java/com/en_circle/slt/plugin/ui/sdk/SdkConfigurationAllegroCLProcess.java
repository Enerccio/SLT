package com.en_circle.slt.plugin.ui.sdk;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.sdk.LispSdk;
import com.en_circle.slt.plugin.ui.SltGlobalUIService;
import com.en_circle.slt.plugin.ui.sdk.SdkDialogProvider.OnSave;
import com.en_circle.slt.tools.AllegroCLUtils;
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

public class SdkConfigurationAllegroCLProcess extends DialogWrapper {

    private final Disposable parentDisposable;
    private final LispSdk instance;
    private final OnSave onSave;
    private boolean isVerified = false;

    private JBTextField name;
    private FileTextField allegroExecutable;
    private FileTextField allegroImage;
    private FileTextField quicklispPath;

    public SdkConfigurationAllegroCLProcess(@NotNull Component parent, LispSdk instance, String title, OnSave onSave) {
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
            name.setText(SltBundle.message("slt.ui.settings.sdk.editor.name.allegro.default"));
        }

        FileChooserDescriptor descriptor = new FileChooserDescriptor(true, false, false,
                        false, false, false);

        allegroExecutable = FileChooserFactory.getInstance()
                .createFileTextField(descriptor, true, parentDisposable);
        allegroExecutable.getField().addCaretListener(createChangeListener());
        allegroExecutable.getField().setText(instance.sbclExecutable);
        allegroImage = FileChooserFactory.getInstance()
                .createFileTextField(descriptor, true, parentDisposable);
        allegroImage.getField().addCaretListener(createChangeListener());
        allegroImage.getField().setText(instance.sbclCorePath);
        quicklispPath = FileChooserFactory.getInstance()
                .createFileTextField(descriptor, true, parentDisposable);
        quicklispPath.getField().addCaretListener(createChangeListener());
        quicklispPath.getField().setText(instance.quickLispPath);

        TextFieldWithBrowseButton cclExecutablePicker = new TextFieldWithBrowseButton(allegroExecutable.getField());
        cclExecutablePicker.addBrowseFolderListener(
                SltBundle.message("slt.ui.settings.sdk.editor.allegro.executable.select"), "", null, descriptor);
        TextFieldWithBrowseButton cclImagePicker = new TextFieldWithBrowseButton(allegroImage.getField());
        cclImagePicker.addBrowseFolderListener(
                SltBundle.message("slt.ui.settings.sdk.editor.allegro.image.select"), "", null, descriptor);
        TextFieldWithBrowseButton quicklispPathPicker = new TextFieldWithBrowseButton(quicklispPath.getField());
        //noinspection DialogTitleCapitalization
        quicklispPathPicker.addBrowseFolderListener(
                SltBundle.message("slt.ui.settings.sdk.editor.quicklisp.select"), "", null, descriptor);

        return new FormBuilder()
                .addLabeledComponent(SltBundle.message("slt.ui.settings.sdk.editor.name"), name, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.sdk.editor.allegro.process.executable"),
                        cclExecutablePicker, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.sdk.editor.allegro.process.image"),
                        cclImagePicker, 1, false)
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
        allegroExecutable.getField().putClientProperty("JComponent.outline", null);
        allegroImage.getField().putClientProperty("JComponent.outline", null);
        quicklispPath.getField().putClientProperty("JComponent.outline", null);

        boolean verified = true;

        if (StringUtils.isBlank(name.getText())) {
            verified = false;
            name.putClientProperty("JComponent.outline", "error");
        }

        String executable = allegroExecutable.getField().getText();
        if (StringUtils.isBlank(executable)) {
            verified = false;
            allegroExecutable.getField().putClientProperty("JComponent.outline", "error");
        }

        String core = allegroImage.getField().getText();
        if (StringUtils.isNotBlank(core)) {
            File file = new File(core);
            if (!file.exists()) {
                verified = false;
                allegroImage.getField().putClientProperty("JComponent.outline", "error");
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
        allegroExecutable.getField().repaint();
        allegroImage.getField().repaint();
        quicklispPath.getField().repaint();

        if (verified)
            verified = checkAndLoadAllegro(executable, core, quicklisp);
        if (!verified) {
            Messages.showErrorDialog(SltBundle.message("slt.ui.settings.sdk.editor.allegro.process.verifying.error"),
                    SltBundle.message("slt.ui.settings.sdk.editor.verifying.error.title"));
        }

        isVerified = verified;
    }

    private boolean checkAndLoadAllegro(String executable, String memory, String quicklisp) {
        ProgressWindow verifyWindow = new ProgressWindow(true, false, null,
                getRootPane(), SltBundle.message("slt.ui.settings.sdk.editor.verifying.cancel"));
        verifyWindow.setTitle(SltBundle.message("slt.ui.settings.sdk.editor.verifying.allegro"));
        Disposer.register(parentDisposable, verifyWindow);

        ProgressResult<Boolean> result = new ProgressRunner<>(pi -> verifyAllegro(pi, executable, memory, quicklisp))
                .sync()
                .onThread(ThreadToUse.POOLED)
                .withProgress(verifyWindow)
                .modal()
                .submitAndGet();
        return Boolean.TRUE.equals(result.getResult());
    }

    private boolean verifyAllegro(ProgressIndicator pi, String executable, String core, String quicklisp) {
        return AllegroCLUtils.verifyAndInstallDependencies(executable, core, quicklisp, pi);
    }

    private void save() {
        instance.userName = name.getText();
        instance.allegroExecutable = allegroExecutable.getField().getText();
        instance.allegroMemoryImage = allegroImage.getField().getText();
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
            super(SltBundle.message("slt.ui.settings.sdk.editor.allegro.process.verify"));
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
                        SltBundle.message("slt.ui.settings.sdk.editor.allegro.process.notverified.message"));
                return;
            }

            save();
        }
    }

}
