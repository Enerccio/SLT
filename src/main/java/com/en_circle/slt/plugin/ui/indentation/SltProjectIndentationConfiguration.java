package com.en_circle.slt.plugin.ui.indentation;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.indentation.SltIndentationSettings;
import com.en_circle.slt.plugin.indentation.SltProjectIndentationSettings;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.NlsContexts.ConfigurableName;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.FormBuilder;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class SltProjectIndentationConfiguration implements Configurable {

    private final Project project;
    private JCheckBox applyProject;
    private JCheckBox applyIndentation;
    private JBTextField defaultIndentation;
    private JBTextField parameterIndentation;
    private JBTextField lambdaIndentation;
    private JBTextField bodyIndentation;
    private JBTextField tagbodyIndentation;

    public SltProjectIndentationConfiguration(Project project) {
        this.project = project;
    }

    @Override
    public @ConfigurableName String getDisplayName() {
        return SltBundle.message("slt.ui.settings.indent.project.name");
    }

    @Override
    public @Nullable JComponent createComponent() {
        applyProject = new JCheckBox(SltBundle.message("slt.ui.settings.indent.project.applies"));
        applyIndentation = new JCheckBox(SltBundle.message("slt.ui.settings.indent.applies"));
        defaultIndentation = new JBTextField();
        parameterIndentation = new JBTextField();
        lambdaIndentation = new JBTextField();
        bodyIndentation = new JBTextField();
        tagbodyIndentation = new JBTextField();

        return FormBuilder.createFormBuilder()
                .addComponent(applyProject, 1)
                .addComponent(applyIndentation, 1)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.indent.type.default"), defaultIndentation, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.indent.type.parameter"), parameterIndentation, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.indent.type.lambda"), lambdaIndentation, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.indent.type.body"), bodyIndentation, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.indent.type.tagbody"), tagbodyIndentation, 1, false)
                .addComponentFillVertically(new JPanel(), 0)
                .getPanel();
    }

    @Override
    public boolean isModified() {
        try {
            SltProjectIndentationSettings projectSettings = gather();
            SltIndentationSettings settings = projectSettings.indentationSettings;

            SltProjectIndentationSettings projectOld = SltProjectIndentationSettings.getInstance(project);
            SltIndentationSettings old = projectOld.indentationSettings;

            if (old == null)
                return true;

            return projectSettings.overridingApplicationSetting != projectOld.overridingApplicationSetting ||
                    settings.applies != old.applies ||
                    settings.defaultIndentation != old.defaultIndentation ||
                    settings.parameterIndentation != old.parameterIndentation ||
                    settings.lambdaIndentation != old.lambdaIndentation ||
                    settings.bodyIndentation != old.bodyIndentation ||
                    settings.tagbodyIndentation != old.tagbodyIndentation;
        } catch (NumberFormatException | AssertionError ignored) {
            return true;
        }
    }

    @Override
    public void apply() throws ConfigurationException {
        try {
            SltProjectIndentationSettings settings = gather();
            SltProjectIndentationSettings old = SltProjectIndentationSettings.getInstance(project);
            old.loadState(settings);
        } catch (NumberFormatException | AssertionError exception) {
            throw new ConfigurationException(SltBundle.message("slt.ui.settings.indent.invalid"));
        }
    }

    private SltProjectIndentationSettings gather() throws NumberFormatException, AssertionError {
        SltIndentationSettings settings = new SltIndentationSettings();

        settings.applies = applyIndentation.isSelected();
        settings.defaultIndentation = Integer.parseInt(defaultIndentation.getText());
        assert(settings.defaultIndentation >= 0);
        settings.parameterIndentation = Integer.parseInt(parameterIndentation.getText());
        assert(settings.parameterIndentation >= 0);
        settings.lambdaIndentation = Integer.parseInt(lambdaIndentation.getText());
        assert(settings.lambdaIndentation >= 0);
        settings.bodyIndentation = Integer.parseInt(bodyIndentation.getText());
        assert(settings.bodyIndentation >= 0);
        settings.tagbodyIndentation = Integer.parseInt(tagbodyIndentation.getText());
        assert(settings.tagbodyIndentation >= 0);

        SltProjectIndentationSettings projectIndentationSettings = new SltProjectIndentationSettings();
        projectIndentationSettings.copySettings(settings);
        projectIndentationSettings.overridingApplicationSetting = applyProject.isSelected();

        return projectIndentationSettings;
    }

    @Override
    public void reset() {
        SltIndentationSettings settings = SltIndentationSettings.getInstance();
        SltProjectIndentationSettings projectIndentationSettings = SltProjectIndentationSettings.getInstance(project);
        if (projectIndentationSettings.indentationSettings != null) {
            settings = projectIndentationSettings.indentationSettings;
        } else {
            projectIndentationSettings.copySettings(settings);
        }

        applyProject.setSelected(projectIndentationSettings.overridingApplicationSetting);
        applyIndentation.setSelected(settings.applies);
        defaultIndentation.setText("" + settings.defaultIndentation);
        parameterIndentation.setText("" + settings.parameterIndentation);
        lambdaIndentation.setText("" + settings.lambdaIndentation);
        bodyIndentation.setText("" + settings.bodyIndentation);
        tagbodyIndentation.setText("" + settings.tagbodyIndentation);
    }
}
