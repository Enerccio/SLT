package com.en_circle.slt.plugin.ui.debug;

import com.en_circle.slt.plugin.ui.debug.SltSymbolBreakpointType.SymbolType;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.util.xmlb.XmlSerializerUtil;
import com.intellij.xdebugger.breakpoints.XBreakpointProperties;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

@State(
        name = "SltBreakpointProperties",
        storages = @Storage("SltBreakpoints.xml")
)
public class SltBreakpointProperties extends XBreakpointProperties<SltBreakpointProperties> {

    public String symbolName;
    public String packageName;
    public SymbolType symbolType;

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
