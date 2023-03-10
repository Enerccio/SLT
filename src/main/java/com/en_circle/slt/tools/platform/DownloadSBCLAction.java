package com.en_circle.slt.tools.platform;

import com.en_circle.slt.plugin.sdk.LispSdk;
import com.intellij.openapi.Disposable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.util.function.Consumer;

public interface DownloadSBCLAction extends DownloadLispAction {
    Logger log = LoggerFactory.getLogger(DownloadSBCLAction.class);

    LispSdk getConfiguredSdk();
    void setDisposable(Disposable disposable);
    void setRootPane(JComponent rootPane);

    @Override
    default void downloadSdk(Disposable disposable, JComponent rootPane, Consumer<LispSdk> acceptor) {
        setDisposable(disposable);
        setRootPane(rootPane);

        //noinspection Convert2Lambda
        run(new Runnable() {
            @Override
            public void run() {

                try {
                    acceptor.accept(getConfiguredSdk());
                } catch (Exception e) {
                    log.error(e.getLocalizedMessage());
                    log.debug(e.getLocalizedMessage(), e);
                }
            }
        });
    }

}
