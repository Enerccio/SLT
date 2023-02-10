package com.en_circle.slt.plugin.ui.debug;

import com.intellij.util.xmlb.XmlSerializerUtil;
import com.intellij.xdebugger.breakpoints.XBreakpointProperties;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class SltBreakpointProperties extends XBreakpointProperties<SltBreakpointProperties> {

    public String symbolName;
    public String packageName;

    public int offset;
    public String file;

    @Override
    public @Nullable SltBreakpointProperties getState() {
        return this;
    }

    @Override
    public void loadState(@NotNull SltBreakpointProperties state) {
        XmlSerializerUtil.copyBean(state, this);
    }

}
