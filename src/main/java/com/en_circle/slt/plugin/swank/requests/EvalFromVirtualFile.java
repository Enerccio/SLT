package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;
import com.intellij.openapi.project.Project;

import java.math.BigInteger;

public class EvalFromVirtualFile extends SlimeRequest {

    public static SlimeRequest eval(String code, String filename, int bufferPosition, int lineno, int charno, Callback callback) {
        return eval(code, filename, bufferPosition, lineno, charno, "cl-user", callback);
    }

    public static SlimeRequest eval(String code, String filename, int bufferPosition, int lineno, int charno, String module, Callback callback) {
        return new EvalFromVirtualFile(code, module, filename, bufferPosition, lineno, charno, callback);
    }

    protected final Callback callback;
    protected final String module;
    protected final String code;
    protected final String filename;
    protected final int bufferPosition;
    protected final int lineno;
    protected final int charno;

    protected EvalFromVirtualFile(String code, String module, String filename, int bufferPosition, int lineno, int charno, Callback callback) {
        this.callback = callback;
        this.module = module;
        this.code = code;
        this.filename = filename;
        this.lineno = lineno;
        this.charno = charno;
        this.bufferPosition = bufferPosition;
    }

    @Override
    public SwankPacket createPacket(BigInteger requestId, Project project) {
        return SwankPacket.swankEvalRegion(code, LispEnvironmentService.getInstance(project).getBreakpointsForInstall(),
                filename, bufferPosition, module, requestId);
    }

    public interface Callback {
        void onResult(String result);
    }
}
