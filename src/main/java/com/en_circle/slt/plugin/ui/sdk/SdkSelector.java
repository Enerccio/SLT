package com.en_circle.slt.plugin.ui.sdk;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.sdk.LispProjectSdk;
import com.en_circle.slt.plugin.sdk.LispSdk;
import com.en_circle.slt.plugin.sdk.SdkList;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentState;
import com.intellij.icons.AllIcons.Actions;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.ComboBox;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.util.NlsContexts.ConfigurableName;
import com.intellij.util.ui.FormBuilder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

public class SdkSelector implements Configurable {

    private final Project project;
    private JPanel root;
    private ComboBox<SdkSelection> sdkSelector;

    public SdkSelector(Project project) {
        this.project = project;
    }

    @Override
    public @ConfigurableName String getDisplayName() {
        return SltBundle.message("slt.ui.projectsettings.sdk");
    }

    @Override
    public @Nullable JComponent createComponent() {
        sdkSelector = new ComboBox<>();
        sdkSelector.setMinimumAndPreferredWidth(Integer.MAX_VALUE);
        root = new JPanel(new BorderLayout());

        FormBuilder builder = FormBuilder.createFormBuilder();
        builder.addLabeledComponent(SltBundle.message("slt.ui.projectsettings.sdk.selector"), sdkSelector, 1, false);

        SdkList list = SdkList.getInstance();
        if (!list.hasSdks()) {
            sdkSelector.setEditable(false);
            builder.addComponent(new JLabel(SltBundle.message("slt.ui.projectsettings.sdk.nosdkdefined")));
        } else {
            builder.addSeparator(1);
            builder.addComponent(new JLabel(SltBundle.message("slt.ui.projectsettings.sdk.definesdk")));
        }

        builder.addComponentFillVertically(new JPanel(), 0);
        JPanel panel = builder.getPanel();
        root.add(panel, BorderLayout.CENTER);
        load();

        DefaultActionGroup controlGroup = new DefaultActionGroup();
        controlGroup.add(new RefreshAction());
        JPanel east = new JPanel(new BorderLayout());
        ActionToolbar toolbar = ActionManager.getInstance()
                .createActionToolbar("SltSDKsConfiguration", controlGroup, false);
        toolbar.setTargetComponent(root);
        east.add(toolbar.getComponent(), BorderLayout.NORTH);
        root.add(east, BorderLayout.EAST);

        return root;
    }

    private void load() {
        SdkList list = SdkList.getInstance();
        List<SdkSelection> selectionList = new ArrayList<>();
        for (LispSdk sdk : list.sdks) {
            SdkSelection sdkSelection = new SdkSelection();
            sdkSelection.setName(sdk.userName);
            sdkSelection.setUuid(sdk.uuid);
            selectionList.add(sdkSelection);
        }
        sdkSelector.setModel(new DefaultComboBoxModel<>(selectionList.toArray(new SdkSelection[0])));
    }

    @Override
    public boolean isModified() {
        LispProjectSdk projectSdk = LispProjectSdk.getInstance(project);
        if (projectSdk.currentSDK == null) {
            return sdkSelector.getItem() != null;
        } else {
            SdkSelection selection = sdkSelector.getItem();
            if (selection == null) {
                return true;
            } else {
                return !selection.getUuid().equals(projectSdk.currentSDK);
            }
        }
    }

    @Override
    public void apply() throws ConfigurationException {
        LispProjectSdk projectSdk = LispProjectSdk.getInstance(project);
        if (sdkSelector.getItem() == null) {
            projectSdk.currentSDK = null;
        } else {
            projectSdk.currentSDK = sdkSelector.getItem().getUuid();
        }

        if (LispEnvironmentService.getInstance(project).getState() != LispEnvironmentState.STOPPED) {
            if (Messages.YES == Messages.showYesNoDialog(
                    SltBundle.message("slt.ui.projectsettings.sdk.restart.prompt"),
                    SltBundle.message("slt.ui.projectsettings.sdk.restart.title"),
                    SltBundle.message("slt.ui.projectsettings.sdk.restart.yes"),
                    SltBundle.message("slt.ui.projectsettings.sdk.restart.no"),
                    Messages.getQuestionIcon())) {
                LispEnvironmentService.getInstance(project).resetConfiguration();
                LispEnvironmentService.getInstance(project).stop();
                LispEnvironmentService.getInstance(project).start();
            }
        }
    }

    @Override
    public void reset() {
        LispProjectSdk projectSdk = LispProjectSdk.getInstance(project);
        if (projectSdk.currentSDK == null) {
            sdkSelector.setItem(null);
        } else {
            SdkSelection selection = new SdkSelection();
            selection.setUuid(projectSdk.currentSDK);
            sdkSelector.setItem(selection);
        }
    }

    private class RefreshAction extends AnAction {

        private RefreshAction() {
            super("", "", Actions.Refresh);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            load();
            reset();
        }

    }

}
