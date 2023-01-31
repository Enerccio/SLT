package com.en_circle.slt.tools.platform;

import com.en_circle.slt.plugin.sdk.LispSdk;
import com.intellij.openapi.Disposable;

import javax.swing.*;
import java.util.function.Consumer;

public interface DownloadLispAction extends UIAction {

    void downloadSdk(Disposable disposable, JComponent rootPane, Consumer<LispSdk> acceptor);

}
