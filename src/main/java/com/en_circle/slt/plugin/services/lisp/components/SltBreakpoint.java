package com.en_circle.slt.plugin.services.lisp.components;

import com.en_circle.slt.plugin.ui.debug.SltBreakpointProperties;
import com.intellij.xdebugger.breakpoints.SuspendPolicy;
import com.intellij.xdebugger.breakpoints.XBreakpoint;
import org.jetbrains.annotations.NotNull;

import java.util.Comparator;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

public class SltBreakpoint implements Comparable<SltBreakpoint> {
    private static final Comparator<SltBreakpoint> COMPARATOR =
            Comparator.comparing(SltBreakpoint::getSymbol);

    private String symbol;

    private final Set<XBreakpoint<SltBreakpointProperties>> nativeBreakpoints = new HashSet<>();

    public SltBreakpoint(String breakpointSymbol) {
        this.symbol = breakpointSymbol;
    }

    public Set<XBreakpoint<SltBreakpointProperties>> getNativeBreakpoints() {
        return nativeBreakpoints;
    }

    public String getSymbol() {
        return symbol;
    }

    public void setSymbol(String symbol) {
        this.symbol = symbol;
    }

    @Override
    public String toString() {
        return "Breakpoint(" + symbol + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        SltBreakpoint that = (SltBreakpoint) o;

        return Objects.equals(symbol, that.symbol);
    }

    @Override
    public int hashCode() {
        return symbol != null ? symbol.hashCode() : 0;
    }

    public boolean shouldBeInstalled() {
        for (XBreakpoint<SltBreakpointProperties> breakpoint : nativeBreakpoints) {
            if (breakpoint.isEnabled() && breakpoint.getSourcePosition() != null && breakpoint.getSuspendPolicy() != SuspendPolicy.NONE) {
                return true;
            }
        }
        return false;
    }

    @Override
    public int compareTo(@NotNull SltBreakpoint o) {
        return COMPARATOR.compare(this, o);
    }
}
