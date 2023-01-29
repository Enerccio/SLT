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
            Comparator.comparing(SltBreakpoint::getSymbol)
                    .thenComparing(SltBreakpoint::getParentSymbol, Comparator.nullsFirst(String::compareTo))
                    .thenComparing(SltBreakpoint::getParentBindType, Comparator.nullsFirst(String::compareTo));

    private String symbol;
    private String parentSymbol;
    private String parentBindType;
    private SltBreakpointType type = SltBreakpointType.STANDARD;

    private boolean installed;

    private final Set<XBreakpoint<SltBreakpointProperties>> nativeBreakpoints = new HashSet<>();

    public SltBreakpoint(String breakpointSymbol) {
        this.symbol = breakpointSymbol;
    }

    public SltBreakpointType getType() {
        return type;
    }

    public void setType(SltBreakpointType type) {
        this.type = type;
    }

    public Set<XBreakpoint<SltBreakpointProperties>> getNativeBreakpoints() {
        return nativeBreakpoints;
    }

    public boolean isInstalled() {
        return installed;
    }

    public void setInstalled(boolean installed) {
        this.installed = installed;
    }

    public String getSymbol() {
        return symbol;
    }

    public void setSymbol(String symbol) {
        this.symbol = symbol;
    }

    public String getParentSymbol() {
        return parentSymbol;
    }

    public void setParentSymbol(String parentSymbol) {
        this.parentSymbol = parentSymbol;
    }

    public String getParentBindType() {
        return parentBindType;
    }

    public void setParentBindType(String parentBindType) {
        this.parentBindType = parentBindType;
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

        if (!Objects.equals(symbol, that.symbol)) return false;
        if (!Objects.equals(parentSymbol, that.parentSymbol)) return false;
        if (!Objects.equals(parentBindType, that.parentBindType))
            return false;
        return type == that.type;
    }

    @Override
    public int hashCode() {
        int result = symbol != null ? symbol.hashCode() : 0;
        result = 31 * result + (parentSymbol != null ? parentSymbol.hashCode() : 0);
        result = 31 * result + (parentBindType != null ? parentBindType.hashCode() : 0);
        result = 31 * result + (type != null ? type.hashCode() : 0);
        return result;
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

    public enum SltBreakpointType {

        STANDARD, INNER, METHOD

    }
}
