package com.en_circle.slt.plugin.services.lisp.components;

import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltOutput;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentListener;
import com.en_circle.slt.plugin.services.lisp.components.SltBreakpoint.SltBreakpointType;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.requests.Eval;
import com.en_circle.slt.plugin.ui.debug.SltBreakpointProperties;
import com.intellij.openapi.project.Project;
import com.intellij.xdebugger.breakpoints.XBreakpoint;
import org.apache.commons.lang3.StringUtils;

import java.util.Collection;
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
            if (breakpoint.isInstalled())
                uninstallBreakpoint(breakpoint);
            breakpoints.remove(breakpoint);
        } else {
            uninstallBreakpoint(breakpoint);
        }
    }

    private SltBreakpoint getBreakpoint(XBreakpoint<SltBreakpointProperties> nativeBreakpoint) {
        SltBreakpointProperties p = nativeBreakpoint.getProperties();
        SltBreakpoint breakpoint = new SltBreakpoint(getBreakpointSymbol(p.packageName, p.symbolName));
        if (p.fletType != null) {
            breakpoint.setParentBindType(p.fletType);
            breakpoint.setParentSymbol(getBreakpointSymbol(p.ppackageName, p.psymbolName));
            breakpoint.setType(SltBreakpointType.INNER);
        }
        return breakpoint;
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
        if (breakpoint.isInstalled()) {
            if (!breakpoint.shouldBeInstalled()) {
                uninstallBreakpoint(breakpoint);
            }
        } else {
            if (breakpoint.shouldBeInstalled()) {
                installBreakpoint(breakpoint);
            }
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
        switch (breakpoint.getType()) {
            case STANDARD -> {
                return Eval.eval("(slt-core:install-breakpoint '" + breakpoint.getSymbol() + ")", r -> installResult(r, breakpoint));
            }
            case INNER -> {
                return Eval.eval("(slt-core:install-inner-breakpoint '" + breakpoint.getSymbol()
                        + " '" + breakpoint.getParentSymbol() + " '" + breakpoint.getParentBindType()
                        + ")", r -> installResult(r, breakpoint));
            }
            case METHOD -> {
                // TODO
                return null;
            }
        }
        return null;
    }

    private void installResult(String s, SltBreakpoint breakpoint) {
        if ("T".equalsIgnoreCase(s)) {
            breakpoint.setInstalled(true);
        }
    }

    private void uninstallBreakpoint(SltBreakpoint breakpoint) {
        try {
            LispEnvironmentService.getInstance(project)
                    .sendToLisp(uninstallRequest(breakpoint), false);
        } catch (Exception ignore) {

        }
    }

    private SlimeRequest uninstallRequest(SltBreakpoint breakpoint) {
        switch (breakpoint.getType()) {
            case STANDARD -> {
                return Eval.eval("(slt-core:uninstall-breakpoint '" + breakpoint.getSymbol() + ")", r -> uninstallResult(r, breakpoint));
            }
            case INNER -> {
                return Eval.eval("(slt-core:uninstall-inner-breakpoint '" + breakpoint.getSymbol()
                        + " '" + breakpoint.getParentSymbol() + " '" + breakpoint.getParentBindType()
                        + ")", r -> uninstallResult(r, breakpoint));
            }
            case METHOD -> {
                // TODO
                return null;
            }
        }
        return null;
    }

    private void uninstallResult(String s, SltBreakpoint breakpoint) {
        if ("T".equalsIgnoreCase(s)) {
            breakpoint.setInstalled(false);
        }
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
        for (SltBreakpoint breakpoint : breakpoints) {
            breakpoint.setInstalled(false);
        }
    }

    @Override
    public void onPostStop() {

    }

}
