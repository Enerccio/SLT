package com.en_circle.slt.plugin.environment;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.environment.SltLispEnvironmentConfiguration.Builder;
import com.en_circle.slt.plugin.environment.abcl.ABCLOverrides;
import com.en_circle.slt.plugin.environment.sbcl.SBCLOverrides;
import com.en_circle.slt.plugin.sdk.LispSdk;
import com.en_circle.slt.plugin.services.lisp.LispSltOverrides;
import com.en_circle.slt.plugin.ui.sdk.SdkConfigurationABCLProcess;
import com.en_circle.slt.plugin.ui.sdk.SdkConfigurationSBCLProcess;
import com.en_circle.slt.plugin.ui.sdk.SdkDialogProvider;
import com.en_circle.slt.tools.platform.DownloadLispAction;
import com.en_circle.slt.tools.platform.DownloadSBCLAction;
import com.intellij.openapi.project.Project;

import java.util.function.Supplier;

public enum Environment {

    ABCL_PROCESS(new EnvironmentDefinition() {
        @Override
        public String getName() {
            return SltBundle.message("slt.environment.abcl");
        }

        @Override
        public Class<? extends DownloadLispAction> getDownloadActionDef() {
            return null;
        }

        @Override
        public Supplier<SltLispEnvironment> getEnvironmentCreator() {
            return SltABCLEnvironment::new;
        }

        @SuppressWarnings("unchecked")
        @Override
        public <T extends Builder<T, R>, R extends SltLispEnvironmentConfiguration> Builder<T, R>
                    buildConfiguration(LispSdk sdk, Project project) {
            return (Builder<T, R>) new SltABCLEnvironmentConfiguration.Builder()
                    .setJvm(sdk.abclJvm)
                    .setJvmArgs(sdk.abclJvmArgs)
                    .setAbclJar(sdk.abclJar)
                    .setQuicklispStartScriptPath(sdk.quickLispPath)
                    .setProjectDirectory(project.getBasePath());
        }

        @Override
        public SdkDialogProvider getDialogProvider() {
            return SdkConfigurationABCLProcess::new;
        }

        @Override
        public LispSltOverrides getOverrides() {
            return new ABCLOverrides();
        }
    }),
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
                SltLispEnvironmentConfiguration.Builder<T, R> buildConfiguration(LispSdk sdk, Project project) {
            return (Builder<T, R>) new SltSBCLEnvironmentConfiguration.Builder()
                    .setExecutable(sdk.sbclExecutable)
                    .setCore(sdk.sbclCorePath)
                    .setQuicklispStartScriptPath(sdk.quickLispPath)
                    .setProjectDirectory(project.getBasePath());
        }

        @Override
        public SdkDialogProvider getDialogProvider() {
            return SdkConfigurationSBCLProcess::new;
        }

        @Override
        public LispSltOverrides getOverrides() {
            return new SBCLOverrides();
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
            SltLispEnvironmentConfiguration.Builder<T, R> buildConfiguration(LispSdk sdk, Project project);
        public abstract SdkDialogProvider getDialogProvider();
        public abstract LispSltOverrides getOverrides();

    }

}
