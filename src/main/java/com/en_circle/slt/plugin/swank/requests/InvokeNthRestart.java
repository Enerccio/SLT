package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispSymbol;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;
import com.intellij.openapi.project.Project;

import java.math.BigInteger;

public class InvokeNthRestart extends SlimeRequest {

    public static SlimeRequest nthRestart(BigInteger threadId, BigInteger option, BigInteger nestLevel, String arg, String args, Callback callback) {
        return new InvokeNthRestart(threadId, option, nestLevel, arg, args, callback);
    }

    protected final Callback callback;
    protected final BigInteger threadId;
    protected final BigInteger restart;
    protected final BigInteger nestLevel;
    protected final String arg;
    protected final String args;

    protected InvokeNthRestart(BigInteger threadId, BigInteger option, BigInteger nestLevel, String arg, String args, Callback callback) {
        this.threadId = threadId;
        this.callback = callback;
        this.restart = option;
        this.nestLevel = nestLevel;
        this.arg = arg;
        this.args = args;
    }

    public void processReply(LispContainer data) {
        if (isOk(data)) {
            callback.onResult();
        }
    }

    private boolean isOk(LispContainer data) {
        return data.getItems().size() > 0 &&
                data.getItems().get(0) instanceof LispSymbol &&
                ":ok".equals(((LispSymbol) data.getItems().get(0)).getValue());
    }

    @Override
    public SwankPacket createPacket(BigInteger requestId, Project project) {
        return SwankPacket.invokeNthRestart(restart, nestLevel, arg, args, threadId, requestId);
    }

    public interface Callback {
        void onResult();
    }

}
