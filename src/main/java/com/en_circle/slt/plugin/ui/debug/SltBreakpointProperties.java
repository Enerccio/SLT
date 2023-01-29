package com.en_circle.slt.plugin.ui.debug;

import com.en_circle.slt.plugin.ui.debug.SltSymbolBreakpointType.SymbolType;
import com.intellij.util.xmlb.XmlSerializerUtil;
import com.intellij.xdebugger.breakpoints.XBreakpointProperties;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class SltBreakpointProperties extends XBreakpointProperties<SltBreakpointProperties> {

    public String symbolName;
    public String packageName;
    public SymbolType symbolType;
    public String psymbolName;
    public String ppackageName;
    public String fletType;

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
