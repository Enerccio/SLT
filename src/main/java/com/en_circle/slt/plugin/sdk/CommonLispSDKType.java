package com.en_circle.slt.plugin.sdk;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltIconProvider;
import com.intellij.openapi.projectRoots.*;
import com.intellij.openapi.roots.ui.configuration.projectRoot.SdkDownload;
import com.intellij.openapi.roots.ui.configuration.projectRoot.SdkDownloadTask;
import org.jdom.Element;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.Nls.Capitalization;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.function.Consumer;

public class CommonLispSDKType extends SdkType implements SdkDownload {

    public static final String CL_SDK = "Common Lisp SDK";

    public CommonLispSDKType() {
        super(CL_SDK);
    }

    public static CommonLispSDKType getInstance() {
        return SdkType.EP_NAME.findExtension(CommonLispSDKType.class);
    }

    @Override
    @Nullable
    public String getVersionString(String sdkHome) {
        return "";
    }

    @Override
    public @Nullable String suggestHomePath() {
        return null;
    }

    @Override
    public boolean isValidSdkHome(@NotNull String path) {
        return false;
    }

    @Override
    public @NotNull String suggestSdkName(@Nullable String currentSdkName, @NotNull String sdkHome) {
        return "SBCL" + getVersionString(sdkHome);
    }

    @Override
    public @Nullable AdditionalDataConfigurable createAdditionalDataConfigurable(@NotNull SdkModel sdkModel, @NotNull SdkModificator sdkModificator) {
        return null;
    }

    @Override
    public @NotNull @Nls(capitalization = Capitalization.Title) String getPresentableName() {
        return SltBundle.message("slt.sdk.name");
    }

    @Override
    public @NotNull Icon getIcon() {
        return SltIconProvider.getSbclIcon();
    }

    @Override
    public void saveAdditionalData(@NotNull SdkAdditionalData additionalData, @NotNull Element additional) {

    }

    @Override
    public boolean supportsDownload(@NotNull SdkTypeId sdkTypeId) {
        return false;
    }

    @Override
    public void showDownloadUI(@NotNull SdkTypeId sdkTypeId, @NotNull SdkModel sdkModel, @NotNull JComponent parentComponent, @Nullable Sdk selectedSdk, @NotNull Consumer<SdkDownloadTask> sdkCreatedCallback) {

    }
}
