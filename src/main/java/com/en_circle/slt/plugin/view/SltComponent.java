package com.en_circle.slt.plugin.view;

import com.en_circle.slt.plugin.swank.SwankServer.SwankServerOutput;
import com.intellij.ui.tabs.TabInfo;

public interface SltComponent {

    TabInfo create();
    TabInfo getTabInfo();

    void onPreStart();
    void onPostStart();

    void handleOutput(SwankServerOutput output, String data);

    void onPreStop();
    void onPostStop();

    String getTitle();

}
