package com.en_circle.slt.plugin.settings;

import com.intellij.ui.JBIntSpinner;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.FormBuilder;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class SltSettingsComponent {

    private final JPanel root;
    private final JBTextField sbclExecutable = new JBTextField("sbcl");
    private final JBTextField port = new JBTextField("4005");

    public SltSettingsComponent() {
        root = FormBuilder.createFormBuilder()
                .addLabeledComponent(new JBLabel("SBCL executable:"), sbclExecutable, 1, false)
                .addComponentFillVertically(new JPanel(), 0)
                .getPanel();
    }

    public JPanel getPanel() {
        return root;
    }

    public JComponent getPreferredFocusedComponent() {
        return sbclExecutable;
    }

    @NotNull
    public String getSbclExecutable() {
        return sbclExecutable.getText();
    }

    public void setSbclExecutable(@NotNull String newText) {
        sbclExecutable.setText(newText);
    }

    public int getPort() {
        return Integer.parseInt(port.getText());
    }

    public void setPort(int p) {
        port.setText("" + p);
    }

}
