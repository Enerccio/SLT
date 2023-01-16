package com.en_circle.slt.plugin.sdk;

import java.util.List;

public interface SBCLSdkProvider {

    List<String> getSbclSourcesFolders();
    boolean isSbclExecutable(String executable);


}
