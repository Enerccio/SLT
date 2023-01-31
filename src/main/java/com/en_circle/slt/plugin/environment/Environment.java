package com.en_circle.slt.plugin.environment;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.environment.SltLispEnvironmentConfiguration.Builder;
import com.en_circle.slt.plugin.sdk.LispSdk;
import com.en_circle.slt.plugin.ui.sdk.SdkConfigurationSBCLProcess;
import com.en_circle.slt.plugin.ui.sdk.SdkDialogProvider;
import com.en_circle.slt.tools.ProjectUtils;
import com.en_circle.slt.tools.platform.DownloadLispAction;
import com.en_circle.slt.tools.platform.DownloadSBCLAction;

import java.util.function.Supplier;

public enum Environment {

    SBCL_PROCESS(new EnvironmentDefinition() {
        @Override
        public String getName() {
            return SltBundle.message("slt.environment.sbcl");
        }

        @Override
        public Class<? extends DownloadLispAction> getDownloadActionDef() {
            return DownloadSBCLAction.class;
        }

        @Override
        public Supplier<SltLispEnvironment> getEnvironmentCreator() {
            return SltSBCLEnvironment::new;
        }

        @SuppressWarnings("unchecked")
        @Override
        public <T extends SltLispEnvironmentConfiguration.Builder<T, R>, R extends SltLispEnvironmentConfiguration>
                SltLispEnvironmentConfiguration.Builder<T, R> buildConfiguration(LispSdk sdk) {
            return (Builder<T, R>) new SltSBCLEnvironmentConfiguration.Builder()
                    .setExecutable(sdk.sbclExecutable)
                    .setCore(sdk.sbclCorePath)
                    .setQuicklispStartScriptPath(sdk.quickLispPath)
                    .setProjectDirectory(ProjectUtils.getCurrentProject().getBasePath());
        }

        @Override
        public SdkDialogProvider getDialogProvider() {
            return SdkConfigurationSBCLProcess::new;
        }
    })

    ;

    private final EnvironmentDefinition definition;

    Environment(EnvironmentDefinition definition) {
        this.definition = definition;
    }

    public EnvironmentDefinition getDefinition() {
        return definition;
    }

    public static abstract class EnvironmentDefinition {

        public abstract String getName();
        public abstract Class<? extends DownloadLispAction> getDownloadActionDef();
        public abstract Supplier<SltLispEnvironment> getEnvironmentCreator();
        public abstract <T extends SltLispEnvironmentConfiguration.Builder<T, R>, R extends SltLispEnvironmentConfiguration>
            SltLispEnvironmentConfiguration.Builder<T, R> buildConfiguration(LispSdk sdk);
        public abstract SdkDialogProvider getDialogProvider();

    }

}
