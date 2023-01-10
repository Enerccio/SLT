package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;

import java.math.BigInteger;

public class SwankEvalFromVirtualFile extends SlimeRequest {

    public static SlimeRequest eval(String code, String filename, int lineno, int charno, Callback callback) {
        return eval(code, filename, lineno, charno, "cl-user", callback);
    }

    public static SlimeRequest eval(String code, String filename, int lineno, int charno, String module, Callback callback) {
        return new SwankEvalFromVirtualFile(code, module, filename, lineno, charno, callback);
    }

    protected final Callback callback;
    protected final String module;
    protected final String code;
    protected final String filename;
    protected final int lineno;
    protected final int charno;

    protected SwankEvalFromVirtualFile(String code, String module, String filename, int lineno, int charno, Callback callback) {
        this.callback = callback;
        this.module = module;
        this.code = code;
        this.filename = filename;
        this.lineno = lineno;
        this.charno = charno;
    }

    @Override
    public SwankPacket createPacket(BigInteger requestId) {
        return SwankPacket.swankSltEvalRegion(code, filename, lineno, charno, module, requestId);
    }

    public interface Callback {
        void onResult(String result);
    }
}
