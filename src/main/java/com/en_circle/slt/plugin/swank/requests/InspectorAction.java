package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.lisp.LispSymbol;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;

import java.math.BigInteger;

public class InspectorAction extends SlimeRequest {

    public static SlimeRequest action(ActionType actionType, BigInteger threadId, String module, Callback callback) {
        return new InspectorAction(actionType, threadId, module, callback);
    }

    public static SlimeRequest action(ActionType actionType, BigInteger threadId, Callback callback) {
        return new InspectorAction(actionType, threadId, "CL-USER", callback);
    }

    protected final Callback callback;
    protected final String module;
    protected final ActionType actionType;
    protected final BigInteger threadId;

    protected InspectorAction(ActionType actionType, BigInteger threadId, String module, Callback callback) {
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
    public SwankPacket createPacket(BigInteger requestId) {
        switch (actionType) {

            case GO_BACK:
                return SwankPacket.inspectorBack(threadId, module, requestId);
            case GO_FORWARD:
                return SwankPacket.inspectorForward(threadId, module, requestId);
            case REFRESH:
                return SwankPacket.inspectorRefresh(threadId, module, requestId);
        }
        throw new IllegalStateException();
    }

    public interface Callback {
        void onResult(LispElement result);
    }

    public enum ActionType {

        GO_BACK, GO_FORWARD, REFRESH

    }
}
