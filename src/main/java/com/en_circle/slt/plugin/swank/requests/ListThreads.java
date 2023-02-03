package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispSymbol;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;
import com.en_circle.slt.plugin.swank.components.ThreadInfo;

import java.math.BigInteger;

public class ListThreads extends SlimeRequest {

    public static SlimeRequest dumpThreads(Callback callback) {
        return new ListThreads(callback);
    }

    private final Callback callback;

    private ListThreads(Callback callback) {
        this.callback = callback;
    }

    @Override
    public SwankPacket createPacket(BigInteger requestId) {
        return SwankPacket.dumpThreads(requestId);
    }

    public void processReply(LispContainer data) {
        if (isOk(data)) {
            callback.onResult(new ThreadInfo((LispContainer) data.getItems().get(1)));
        }
    }

    private boolean isOk(LispContainer data) {
        return data.getItems().size() > 0 &&
                data.getItems().get(0) instanceof LispSymbol &&
                ":ok".equals(((LispSymbol) data.getItems().get(0)).getValue());
    }

    public interface Callback {
        void onResult(ThreadInfo result);
    }

}
