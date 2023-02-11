package com.en_circle.slt.plugin.services.lisp.components;

import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltOutput;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentListener;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.requests.Eval;
import com.en_circle.slt.plugin.ui.debug.SltBreakpointProperties;
import com.intellij.openapi.project.Project;
import com.intellij.xdebugger.breakpoints.XBreakpoint;
import org.apache.commons.lang3.StringUtils;

import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

public class SltBreakpointContainer implements LispEnvironmentListener {

    private final TreeSet<SltBreakpoint> breakpoints = new TreeSet<>();
    private final Project project;

    public SltBreakpointContainer(Project project) {
        this.project = project;
    }


    public void addBreakpoint(XBreakpoint<SltBreakpointProperties> nativeBreakpoint) {
        SltBreakpoint breakpoint = getBreakpoint(nativeBreakpoint);

        SltBreakpoint existing = search(breakpoint);
        if (existing == null) {
            breakpoints.add(breakpoint);
            existing = breakpoint;
        }

        existing.getNativeBreakpoints().add(nativeBreakpoint);
        updateBreakpoint(existing);
    }

    public void removeBreakpoint(XBreakpoint<SltBreakpointProperties> nativeBreakpoint) {
        SltBreakpoint breakpoint = getBreakpoint(nativeBreakpoint);
        breakpoint.getNativeBreakpoints().remove(nativeBreakpoint);

        if (breakpoint.getNativeBreakpoints().isEmpty()) {
            uninstallBreakpoint(breakpoint);
            breakpoints.remove(breakpoint);
        }
    }

    private SltBreakpoint getBreakpoint(XBreakpoint<SltBreakpointProperties> nativeBreakpoint) {
        SltBreakpointProperties p = nativeBreakpoint.getProperties();
        return new SltBreakpoint(getBreakpointSymbol(p.packageName, p.symbolName));
    }

    public void onUpdate(XBreakpoint<SltBreakpointProperties> nativeBreakpoint) {
        for (SltBreakpoint breakpoint : breakpoints) {
            if (breakpoint.getNativeBreakpoints().contains(nativeBreakpoint)) {
                updateBreakpoint(breakpoint);
                return;
            }
        }
    }

    private String getBreakpointSymbol(String packageName, String symbolName) {
        if (StringUtils.isBlank(packageName)) {
            return symbolName;
        }
        if (symbolName.startsWith(":")) {
            return symbolName;
        }
        return packageName + "::" + symbolName;
    }

    private SltBreakpoint search(SltBreakpoint key) {
        SltBreakpoint ceil  = breakpoints.ceiling(key);
        SltBreakpoint floor = breakpoints.floor(key);
        return ceil == floor? ceil : null;
    }

    private void updateBreakpoint(SltBreakpoint breakpoint) {
        if (breakpoint.shouldBeInstalled()) {
            installBreakpoint(breakpoint);
        }
    }

    private void installBreakpoint(SltBreakpoint breakpoint) {
        try {
            LispEnvironmentService.getInstance(project)
                    .sendToLisp(installRequest(breakpoint), false);
        } catch (Exception ignore) {

        }
    }

    private SlimeRequest installRequest(SltBreakpoint breakpoint) {
        return Eval.eval("(slt-core:install-breakpoint \"" + breakpoint.getSymbol() + "\")",
                false, r -> {});
    }

    private void uninstallBreakpoint(SltBreakpoint breakpoint) {
        try {
            LispEnvironmentService.getInstance(project)
                    .sendToLisp(uninstallRequest(breakpoint), false);
        } catch (Exception ignore) {

        }
    }

    private SlimeRequest uninstallRequest(SltBreakpoint breakpoint) {
        return Eval.eval("(slt-core:uninstall-breakpoint '" + breakpoint.getSymbol() + ")",
                false, r -> {});
    }

    public Collection<SltBreakpoint> getAllBreakpoints() {
        return breakpoints;
    }

    @Override
    public void onOutputChanged(SltOutput output, String newData) {

    }

    @Override
    public void onPreStart() {

    }

    @Override
    public void onPostStart() {
        for (SltBreakpoint breakpoint : breakpoints) {
            updateBreakpoint(breakpoint);
        }
    }

    @Override
    public void onPreStop() {

    }

    @Override
    public void onPostStop() {

    }

    public String getInstallBreakpoints() {
        Set<String> breakpointsToInstall = new TreeSet<>();
        for (SltBreakpoint breakpoint : breakpoints) {
            if (breakpoint.shouldBeInstalled()) {
                breakpointsToInstall.add(breakpoint.getSymbol());
            }
        }
        if (breakpointsToInstall.isEmpty()) {
            return null;
        }
        return String.join(" ", breakpointsToInstall);
    }
}
