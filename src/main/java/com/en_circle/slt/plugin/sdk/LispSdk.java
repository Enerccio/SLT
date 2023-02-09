package com.en_circle.slt.plugin.sdk;

import com.en_circle.slt.plugin.environment.Environment;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;

public class LispSdk implements PersistentStateComponent<LispSdk> {

    public String uuid;
    public String userName;

    // use getter!
    public Environment environment;
    public String quickLispPath;

    // SBCL Process used
    public String sbclExecutable;
    public String sbclCorePath;

    // ABCL Process used
    public String abclJvm;
    public String abclJvmArgs;
    public String abclJar;

    // CCL Process used
    public String cclExecutable;
    public String cclMemoryImage;

    // AllegroCL Process used
    public String allegroExecutable;
    public String allegroMemoryImage;

    // CMUCL Process used
    public String cmuclExecutable;
    public String cmuclMemoryImage;

    public Environment getEnvironment() {
        if (environment == null) {
            // backwards compat
            environment = Environment.SBCL_PROCESS;
        }
        return environment;
    }

    @Override
    public @Nullable LispSdk getState() {
        if (environment == null) {
            // backwards compat
            environment = Environment.SBCL_PROCESS;
        }
        return this;
    }

    @Override
    public void loadState(@NotNull LispSdk state) {
        XmlSerializerUtil.copyBean(state, this);
        if (environment == null) {
            // backwards compat
            environment = Environment.SBCL_PROCESS;
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        LispSdk sdk = (LispSdk) o;

        if (!Objects.equals(uuid, sdk.uuid)) return false;
        if (!Objects.equals(userName, sdk.userName)) return false;
        if (environment != sdk.environment) return false;
        if (!Objects.equals(quickLispPath, sdk.quickLispPath)) return false;
        if (!Objects.equals(sbclExecutable, sdk.sbclExecutable))
            return false;
        if (!Objects.equals(sbclCorePath, sdk.sbclCorePath)) return false;
        if (!Objects.equals(abclJvm, sdk.abclJvm)) return false;
        if (!Objects.equals(abclJvmArgs, sdk.abclJvmArgs)) return false;
        if (!Objects.equals(abclJar, sdk.abclJar)) return false;
        if (!Objects.equals(cclExecutable, sdk.cclExecutable)) return false;
        if (!Objects.equals(cclMemoryImage, sdk.cclMemoryImage))
            return false;
        if (!Objects.equals(allegroExecutable, sdk.allegroExecutable))
            return false;
        return Objects.equals(allegroMemoryImage, sdk.allegroMemoryImage);
    }

    @Override
    public int hashCode() {
        int result = uuid != null ? uuid.hashCode() : 0;
        result = 31 * result + (userName != null ? userName.hashCode() : 0);
        result = 31 * result + (environment != null ? environment.hashCode() : 0);
        result = 31 * result + (quickLispPath != null ? quickLispPath.hashCode() : 0);
        result = 31 * result + (sbclExecutable != null ? sbclExecutable.hashCode() : 0);
        result = 31 * result + (sbclCorePath != null ? sbclCorePath.hashCode() : 0);
        result = 31 * result + (abclJvm != null ? abclJvm.hashCode() : 0);
        result = 31 * result + (abclJvmArgs != null ? abclJvmArgs.hashCode() : 0);
        result = 31 * result + (abclJar != null ? abclJar.hashCode() : 0);
        result = 31 * result + (cclExecutable != null ? cclExecutable.hashCode() : 0);
        result = 31 * result + (cclMemoryImage != null ? cclMemoryImage.hashCode() : 0);
        result = 31 * result + (allegroExecutable != null ? allegroExecutable.hashCode() : 0);
        result = 31 * result + (allegroMemoryImage != null ? allegroMemoryImage.hashCode() : 0);
        return result;
    }
}
