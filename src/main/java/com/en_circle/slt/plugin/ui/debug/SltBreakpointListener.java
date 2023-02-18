package com.en_circle.slt.plugin.ui.debug;

import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.intellij.openapi.project.Project;
import com.intellij.xdebugger.breakpoints.XBreakpointListener;
import com.intellij.xdebugger.breakpoints.XLineBreakpoint;
import org.jetbrains.annotations.NotNull;

@SuppressWarnings("DataFlowIssue")
public class SltBreakpointListener implements XBreakpointListener<XLineBreakpoint<SltBreakpointProperties>> {

    private final Project project;

    public SltBreakpointListener(Project project) {
        this.project = project;
    }

    @Override
    public void breakpointAdded(@NotNull XLineBreakpoint<SltBreakpointProperties> breakpoint) {
        if (breakpoint.getProperties() instanceof SltBreakpointProperties) {
            LispEnvironmentService.getInstance(project)
                    .addBreakpoint(breakpoint);
        }
    }

    @Override
    public void breakpointRemoved(@NotNull XLineBreakpoint<SltBreakpointProperties> breakpoint) {
        if (breakpoint.getProperties() instanceof SltBreakpointProperties) {
            LispEnvironmentService.getInstance(project)
                    .removeBreakpoint(breakpoint);
        }
    }

    @Override
    public void breakpointChanged(@NotNull XLineBreakpoint<SltBreakpointProperties> breakpoint) {
        if (breakpoint.getProperties() instanceof SltBreakpointProperties) {
            LispEnvironmentService.getInstance(project).nativeBreakpointUpdated(breakpoint);
        }
    }
}
