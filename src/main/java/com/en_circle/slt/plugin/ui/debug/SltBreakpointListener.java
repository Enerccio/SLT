package com.en_circle.slt.plugin.ui.debug;

import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.intellij.openapi.project.Project;
import com.intellij.xdebugger.breakpoints.XBreakpoint;
import com.intellij.xdebugger.breakpoints.XBreakpointListener;
import org.jetbrains.annotations.NotNull;

@SuppressWarnings({"unchecked"})
public class SltBreakpointListener implements XBreakpointListener<XBreakpoint<?>> {

    private final Project project;

    public SltBreakpointListener(Project project) {
        this.project = project;
    }

    @Override
    public void breakpointAdded(@NotNull XBreakpoint<?> breakpoint) {
        if (breakpoint.getProperties() instanceof SltBreakpointProperties) {
            LispEnvironmentService.getInstance(project)
                    .addBreakpoint((XBreakpoint<SltBreakpointProperties>) breakpoint);
        }
    }

    @Override
    public void breakpointRemoved(@NotNull XBreakpoint<?> breakpoint) {
        if (breakpoint.getProperties() instanceof SltBreakpointProperties) {
            LispEnvironmentService.getInstance(project)
                    .removeBreakpoint((XBreakpoint<SltBreakpointProperties>) breakpoint);
        }
    }

    @Override
    public void breakpointChanged(@NotNull XBreakpoint<?> breakpoint) {
        if (breakpoint.getProperties() instanceof SltBreakpointProperties) {
            LispEnvironmentService.getInstance(project)
                    .nativeBreakpointUpdated((XBreakpoint<SltBreakpointProperties>) breakpoint);
        }
    }
}
