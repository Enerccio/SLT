package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.lisp.LispSymbol;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;
import com.intellij.openapi.project.Project;

import java.math.BigInteger;

public class InspectFrameVar extends SlimeRequest {

    public static SlimeRequest inspectVariable(BigInteger ix, BigInteger frame, BigInteger threadId, String module, Callback callback) {
        return new InspectFrameVar(ix, frame, threadId, module, callback);
    }

    public static SlimeRequest inspectVariable(BigInteger ix, BigInteger frame, BigInteger threadId, Callback callback) {
        return new InspectFrameVar(ix, frame, threadId, "CL-USER", callback);
    }

    protected final Callback callback;
    protected final String module;
    protected final BigInteger ix;
    protected final BigInteger frame;
    protected final BigInteger threadId;

    protected InspectFrameVar(BigInteger ix, BigInteger frame, BigInteger threadId, String module, Callback callback) {
        this.callback = callback;
        this.module = module;
        this.ix = ix;
        this.frame = frame;
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
        return SwankPacket.inspectLocal(ix, frame, threadId, module, requestId);
    }

    public interface Callback {
        void onResult(LispElement result);
    }

}
