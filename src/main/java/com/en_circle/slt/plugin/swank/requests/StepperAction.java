package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.lisp.LispSymbol;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;
import com.intellij.openapi.project.Project;

import java.math.BigInteger;

public class StepperAction extends SlimeRequest {

    public static SlimeRequest action(ActionType actionType, BigInteger threadId, String module, Callback callback) {
        return new StepperAction(actionType, threadId, module, callback);
    }

    public static SlimeRequest action(ActionType actionType, BigInteger threadId, Callback callback) {
        return new StepperAction(actionType, threadId, "CL-USER", callback);
    }

    protected final Callback callback;
    protected final String module;
    protected final ActionType actionType;
    protected final BigInteger threadId;

    protected StepperAction(ActionType actionType, BigInteger threadId, String module, Callback callback) {
        this.callback = callback;
        this.module = module;
        this.actionType = actionType;
        this.threadId = threadId;
    }

    public void processReply(LispContainer data) {
        if (isOk(data)) {
            callback.onResult(data.getItems().get(1));
        }
    }

    private boolean isOk(LispContainer data) {
        return data.getItems().size() > 0 &&
                data.getItems().get(0) instanceof LispSymbol &&
                ":ok".equals(((LispSymbol) data.getItems().get(0)).getValue());
    }

    @Override
    public SwankPacket createPacket(BigInteger requestId, Project project) {
        switch (actionType) {
            case ENABLE:
                return SwankPacket.activateStepping(threadId, module, requestId);
            case IN:
                return SwankPacket.stepperIn(threadId, module, requestId);
            case NEXT:
                return SwankPacket.stepperNext(threadId, module, requestId);
            case OUT:
                return SwankPacket.stepperOut(threadId, module, requestId);
        }
        throw new IllegalStateException();
    }

    public interface Callback {
        void onResult(LispElement result);
    }

    public enum ActionType {

        ENABLE, IN, NEXT, OUT

    }
}
