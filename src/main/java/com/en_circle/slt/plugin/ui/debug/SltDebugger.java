package com.en_circle.slt.plugin.ui.debug;

import com.en_circle.slt.plugin.swank.debug.SltDebugInfo;
import com.intellij.openapi.Disposable;
import com.intellij.ui.tabs.TabInfo;

public interface SltDebugger extends Disposable {

    TabInfo getTab();

    void redraw(SltDebugInfo debugInfo);
    void activate();

}
