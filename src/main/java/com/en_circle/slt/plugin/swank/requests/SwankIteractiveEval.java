package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.lisp.lisp.LispList;
import com.en_circle.slt.plugin.lisp.lisp.LispString;
import com.en_circle.slt.plugin.lisp.lisp.LispSymbol;
import com.en_circle.slt.plugin.swank.SlimePacket;
import com.en_circle.slt.plugin.swank.SlimeRequest;

import java.math.BigInteger;

public class SwankIteractiveEval extends SlimeRequest {

    public static SlimeRequest eval(String code, String module, Callback callback) {
        return new SwankIteractiveEval(code, module, callback);
    }

    public static SlimeRequest eval(String code, Callback callback) {
        return new SwankIteractiveEval(code, "cl-user", callback);
    }

    protected final Callback callback;
    protected final String module;
    protected final String code;

    protected SwankIteractiveEval(String code, String module, Callback callback) {
        this.callback = callback;
        this.module = module;
        this.code = code;
    }

    public void processReply(LispList data) {
        if (isOk(data)) {
            String returnedText = ((LispString) data.getItems().get(1)).getValue();
            callback.onResult(returnedText.substring(4, returnedText.length()-1));
        }
    }

    private boolean isOk(LispList data) {
        return data.getItems().size() > 0 &&
                data.getItems().get(0) instanceof LispSymbol &&
                ":ok".equals(((LispSymbol) data.getItems().get(0)).getValue());
    }

    @Override
    public SlimePacket createPacket(BigInteger requestId) {
        return SlimePacket.swankInteractiveEval(code, module, requestId);
    }

    public interface Callback {
        void onResult(String result);
    }

}
