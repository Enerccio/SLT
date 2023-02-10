package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispString;
import com.en_circle.slt.plugin.lisp.lisp.LispSymbol;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;
import com.intellij.openapi.project.Project;

import java.math.BigInteger;

public class Eval extends SlimeRequest {

    public static SlimeRequest eval(String code, String module, boolean includeDebugger, Callback callback) {
        return new Eval(code, module, includeDebugger, callback);
    }

    public static SlimeRequest eval(String code, String module, Callback callback) {
        return eval(code, module, true, callback);
    }

    public static SlimeRequest eval(String code, boolean includeDebugger, Callback callback) {
        return eval(code, "cl-user", includeDebugger, callback);
    }

    public static SlimeRequest eval(String code, Callback callback) {
        return eval(code, true, callback);
    }

    protected final Callback callback;
    protected final boolean includeDebugger;
    protected final String module;
    protected final String code;

    protected Eval(String code, String module, boolean includeDebugger, Callback callback) {
        this.callback = callback;
        this.includeDebugger = includeDebugger;
        this.module = module;
        this.code = code;
    }

    public void processReply(LispContainer data) {
        if (isOk(data)) {
            String returnedText = ((LispString) data.getItems().get(1)).getValue();
            callback.onResult(returnedText);
        }
    }

    private boolean isOk(LispContainer data) {
        return data.getItems().size() > 0 &&
                data.getItems().get(0) instanceof LispSymbol &&
                ":ok".equals(((LispSymbol) data.getItems().get(0)).getValue());
    }

    @Override
    public SwankPacket createPacket(BigInteger requestId, Project project) {
        return SwankPacket.sltEval(code, includeDebugger ? LispEnvironmentService.getInstance(project)
                .getBreakpointsForInstall() : null, module, requestId);
    }

    public interface Callback {
        void onResult(String result);
    }

}
