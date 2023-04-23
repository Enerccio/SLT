package com.en_circle.slt.plugin.ui.indentation;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.indentation.SltIndentationSettings;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.util.NlsContexts.ConfigurableName;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.FormBuilder;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class SltIndentationConfiguration implements Configurable {

    private JCheckBox applyIndentation;
    private JBTextField defaultIndentation;
    private JBTextField parameterIndentation;
    private JBTextField lambdaIndentation;
    private JBTextField bodyIndentation;
    private JBTextField tagbodyIndentation;
    private JCheckBox rainbow;

    @Override
    public @ConfigurableName String getDisplayName() {
        return SltBundle.message("slt.ui.settings.indent.name");
    }

    @Override
    public @Nullable JComponent createComponent() {
        applyIndentation = new JCheckBox(SltBundle.message("slt.ui.settings.indent.applies"));
        defaultIndentation = new JBTextField();
        parameterIndentation = new JBTextField();
        lambdaIndentation = new JBTextField();
        bodyIndentation = new JBTextField();
        tagbodyIndentation = new JBTextField();
        rainbow = new JCheckBox();

        return FormBuilder.createFormBuilder()
                .addComponent(applyIndentation, 1)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.indent.type.default"), defaultIndentation, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.indent.type.parameter"), parameterIndentation, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.indent.type.lambda"), lambdaIndentation, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.indent.type.body"), bodyIndentation, 1, false)
                .addLabeledComponent(SltBundle.message("slt.ui.settings.indent.type.tagbody"), tagbodyIndentation, 1, false)
                .addSeparator()
                .addLabeledComponent(SltBundle.message("slt.ui.settings.indent.type.rainbow"), rainbow, 1, false)
                .addComponentFillVertically(new JPanel(), 0)
                .getPanel();
    }

    @Override
    public boolean isModified() {
        try {
            SltIndentationSettings settings = gather();
            SltIndentationSettings old = SltIndentationSettings.getInstance();
            return settings.applies != old.applies ||
                    settings.defaultIndentation != old.defaultIndentation ||
                    settings.parameterIndentation != old.parameterIndentation ||
                    settings.lambdaIndentation != old.lambdaIndentation ||
                    settings.bodyIndentation != old.bodyIndentation ||
                    settings.tagbodyIndentation != old.tagbodyIndentation ||
                    settings.rainbow != old.rainbow
                    ;
        } catch (NumberFormatException | AssertionError ignored) {
            return true;
        }
    }

    @Override
    public void apply() throws ConfigurationException {
        try {
            SltIndentationSettings settings = gather();
            SltIndentationSettings old = SltIndentationSettings.getInstance();
            old.loadState(settings);
        } catch (NumberFormatException | AssertionError exception) {
            throw new ConfigurationException(SltBundle.message("slt.ui.settings.indent.invalid"));
        }
    }

    private SltIndentationSettings gather() throws NumberFormatException, AssertionError {
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
        settings.rainbow = rainbow.isSelected();
        return settings;
    }

    @Override
    public void reset() {
        SltIndentationSettings settings = SltIndentationSettings.getInstance();
        applyIndentation.setSelected(settings.applies);
        defaultIndentation.setText("" + settings.defaultIndentation);
        parameterIndentation.setText("" + settings.parameterIndentation);
        lambdaIndentation.setText("" + settings.lambdaIndentation);
        bodyIndentation.setText("" + settings.bodyIndentation);
        tagbodyIndentation.setText("" + settings.tagbodyIndentation);
        rainbow.setSelected(settings.rainbow);
    }
}
