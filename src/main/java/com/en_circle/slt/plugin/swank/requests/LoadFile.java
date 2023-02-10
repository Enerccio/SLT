package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;
import com.intellij.openapi.project.Project;

import java.math.BigInteger;

public class LoadFile extends SlimeRequest {

    public static SlimeRequest loadFile(String file) {
        return new LoadFile(file);
    }

    private final String file;

    public LoadFile(String file) {
        this.file = file;
    }

    @Override
    public SwankPacket createPacket(BigInteger requestId, Project project) {
        return SwankPacket.loadFile(file, LispEnvironmentService.getInstance(project).getBreakpointsForInstall(),
                requestId);
    }
}
