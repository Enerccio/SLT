package com.en_circle.slt.plugin.environment.allegro;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.environment.*;
import com.en_circle.slt.plugin.environment.SltLispEnvironmentConfiguration.Builder;
import com.en_circle.slt.plugin.sdk.LispSdk;
import com.en_circle.slt.plugin.services.lisp.LispSltOverrides;
import com.en_circle.slt.plugin.ui.sdk.SdkConfigurationAllegroCLProcess;
import com.en_circle.slt.plugin.ui.sdk.SdkDialogProvider;
import com.en_circle.slt.tools.platform.DownloadLispAction;
import com.intellij.openapi.project.Project;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Supplier;

public class AllegroCLEnvironmentDefinition extends EnvironmentDefinition {

    private final Set<LispFeatures> features = new HashSet<>();

    public AllegroCLEnvironmentDefinition() {
        features.add(LispFeatures.REPL);
        features.add(LispFeatures.DOCUMENTATION);
        features.add(LispFeatures.MACROEXPAND);
        features.add(LispFeatures.DEBUGGER_ACTION_ARGLIST);
        features.add(LispFeatures.FRAME_EVAL);
        features.add(LispFeatures.INSPECTOR);
        features.add(LispFeatures.INSPECTOR_HISTORY);
        features.add(LispFeatures.AUTOCOMPLETE);
        features.add(LispFeatures.XREFS);
    }

    @Override
    public String getName() {
        return SltBundle.message("slt.environment.allegro");
    }

    @Override
    public Class<? extends DownloadLispAction> getDownloadActionDef() {
        return null;
    }

    @Override
    public Supplier<SltLispEnvironment> getEnvironmentCreator() {
        return SltAllegroCLEnvironment::new;
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T extends Builder<T, R>, R extends SltLispEnvironmentConfiguration>
    Builder<T, R> buildConfiguration(LispSdk sdk, Project project) {
        return (Builder<T, R>) new SltAllegroCLEnvironmentConfiguration.Builder()
                .setExecutable(sdk.allegroExecutable)
                .setMemoryImage(sdk.allegroMemoryImage)
                .setQuicklispStartScriptPath(sdk.quickLispPath)
                .setProjectDirectory(project.getBasePath());
    }

    @Override
    public SdkDialogProvider getDialogProvider() {
        return SdkConfigurationAllegroCLProcess::new;
    }

    @Override
    public LispSltOverrides getOverrides() {
        return new AllegroCLOverrides();
    }

    @Override
    public boolean hasFeature(LispFeatures feature) {
        return features.contains(feature);
    }

}
