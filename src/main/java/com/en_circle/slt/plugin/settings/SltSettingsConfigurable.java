package com.en_circle.slt.plugin.settings;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltState;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentState;
import com.en_circle.slt.tools.ProjectUtils;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.util.NlsContexts.ConfigurableName;
import org.jetbrains.annotations.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;

public class SltSettingsConfigurable implements Configurable {
    private static final Logger log = LoggerFactory.getLogger(SltSettingsConfigurable.class);

    private SltSettingsComponent component;

    @Override
    public @ConfigurableName String getDisplayName() {
        return SltBundle.message("slt.ui.settings.title");
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
        if (!component.getSbclExecutable().equals(settings.sbclExecutable)) {
            restartServer = true;
        }
        if (component.getPort() != settings.port) {
            restartServer = true;
        }
        if (!component.getQuicklispStartScript().equals(settings.quicklispStartScript)) {
            restartServer = true;
        }

        settings.sbclExecutable = component.getSbclExecutable();
        settings.port = component.getPort();
        settings.quicklispStartScript = component.getQuicklispStartScript();

        Project project = ProjectUtils.getCurrentProject();

        if (restartServer && LispEnvironmentService.getInstance(project).getState() != LispEnvironmentState.STOPPED) {
            if (Messages.YES == Messages.showYesNoDialog(
                    SltBundle.message("slt.ui.settings.restart.prompt"),
                    SltBundle.message("slt.ui.settings.restart.title"),
                    SltBundle.message("slt.ui.settings.restart.yes"),
                    SltBundle.message("slt.ui.settings.restart.no"),
                    Messages.getQuestionIcon())) {
                LispEnvironmentService.getInstance(project).resetConfiguration();
                LispEnvironmentService.getInstance(project).stop();
                LispEnvironmentService.getInstance(project).start();
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
