package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;
import com.intellij.openapi.project.Project;

import java.math.BigInteger;

public class EvalStringInFrameEval extends Eval {

    public static SlimeRequest evalInFrame(String code, BigInteger frame, BigInteger thread, String module, Callback callback) {
        return new EvalStringInFrameEval(code, frame, thread, module, callback);
    }

    protected final BigInteger frame;
    protected final BigInteger thread;

    protected EvalStringInFrameEval(String code, BigInteger frame, BigInteger thread, String module, Callback callback) {
        super(code, module, true, callback);
        this.frame = frame;
        this.thread = thread;
    }


    @Override
    public SwankPacket createPacket(BigInteger requestId, Project project) {
        return SwankPacket.evalInFrame(code, LispEnvironmentService.getInstance(project)
                .getBreakpointsForInstall(), frame, module, thread, requestId);
    }


}
