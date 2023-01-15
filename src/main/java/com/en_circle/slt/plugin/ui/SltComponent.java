package com.en_circle.slt.plugin.ui;

import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltOutput;
import com.intellij.ui.tabs.TabInfo;

public interface SltComponent {

    TabInfo create();
    TabInfo getTabInfo();

    void onPreStart();
    void onPostStart();

    void handleOutput(SltOutput output, String data);

    void onPreStop();
    void onPostStop();

    String getTitle();

}
