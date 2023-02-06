package com.en_circle.slt.plugin.environment;

import com.en_circle.slt.plugin.sdk.LispSdk;
import com.en_circle.slt.plugin.services.lisp.LispSltOverrides;
import com.en_circle.slt.plugin.ui.sdk.SdkDialogProvider;
import com.en_circle.slt.tools.platform.DownloadLispAction;
import com.intellij.openapi.project.Project;

import java.util.function.Supplier;

public abstract class EnvironmentDefinition {

    public abstract String getName();

    public abstract Class<? extends DownloadLispAction> getDownloadActionDef();

    public abstract Supplier<SltLispEnvironment> getEnvironmentCreator();

    public abstract <T extends SltLispEnvironmentConfiguration.Builder<T, R>, R extends SltLispEnvironmentConfiguration>
    SltLispEnvironmentConfiguration.Builder<T, R> buildConfiguration(LispSdk sdk, Project project);

    public abstract SdkDialogProvider getDialogProvider();

    public abstract LispSltOverrides getOverrides();

    public abstract boolean hasFeature(LispFeatures feature);

}
