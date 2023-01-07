package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.swank.SlimePacket;
import com.en_circle.slt.plugin.swank.SlimeRequest;

import java.math.BigInteger;

public class SwankEvalRegion extends SwankIteractiveEval {

    public static SlimeRequest eval(String code, String module, Callback callback) {
        return new SwankEvalRegion(code, module, callback);
    }

    public static SlimeRequest eval(String code, Callback callback) {
        return new SwankEvalRegion(code, "cl-user", callback);
    }

    protected SwankEvalRegion(String code, String module, Callback callback) {
        super(code, module, callback);
    }

    @Override
    public SlimePacket createPacket(BigInteger requestId) {
        return SlimePacket.evalRegion(code, module, requestId);
    }
}
