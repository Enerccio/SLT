package com.en_circle.slt.plugin.settings;

import com.en_circle.slt.plugin.SltSBCL;
import com.en_circle.slt.plugin.SltState;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.util.NlsContexts.ConfigurableName;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class SltSettingsConfigurable implements Configurable {

    private SltSettingsComponent component;

    @Override
    public @ConfigurableName String getDisplayName() {
        return "SLT Configuration";
    }

    @Override
    public @Nullable JComponent createComponent() {
        component = new SltSettingsComponent();
        return component.getPanel();
    }

    @Override
    public JComponent getPreferredFocusedComponent() {
        return component.getPreferredFocusedComponent();
    }

    @Override
    public boolean isModified() {
        SltState settings = SltState.getInstance();
        boolean modified = !component.getSbclExecutable().equals(settings.sbclExecutable);
        modified |= component.getPort() != settings.port;
        modified |= !component.getQuicklispStartScript().equals(settings.quicklispStartScript);
        return modified;
    }

    @Override
    public void apply() throws ConfigurationException {
        SltState settings = SltState.getInstance();
        boolean restartServer = false;
        if (!settings.sbclExecutable.equals(component.getSbclExecutable())) {
            restartServer = true;
        }
        if (component.getPort() != settings.port) {
            restartServer = true;
        }
        if (!component.getQuicklispStartScript().equals(settings.quicklispStartScript)) {
            restartServer = true;
        }

        settings.sbclExecutable = component.getSbclExecutable();
        if (restartServer) {
            if (Messages.YES == Messages.showYesNoDialog("Settings changed, restart server?", "Settings Changed", "Yes", "No", Messages.getQuestionIcon())) {
                try {
                    SltSBCL.getInstance().stop();
                    SltSBCL.getInstance().start();
                } catch (Exception e) {
                    // TODO: show exception
                }
            }
        }
    }

    @Override
    public void reset() {
        SltState settings = SltState.getInstance();
        component.setSbclExecutable(settings.sbclExecutable);
        component.setQuicklispStartScript(settings.quicklispStartScript);
        component.setPort(settings.port);
    }

    @Override
    public void disposeUIResources() {
        component = null;
    }

}
