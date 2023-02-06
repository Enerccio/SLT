package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispSymbol;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;

import java.math.BigInteger;

public class SuspendThread extends SlimeRequest {

    public static SlimeRequest suspend(BigInteger id, Callback callback) {
        return new SuspendThread(id, callback);
    }

    private final BigInteger id;
    private final Callback callback;

    private SuspendThread(BigInteger id, Callback callback) {
        this.id = id;
        this.callback = callback;
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
    public SwankPacket createPacket(BigInteger requestId) {
        return SwankPacket.breakThread(id, requestId);
    }

    public interface Callback {
        void onResult();
    }

}
