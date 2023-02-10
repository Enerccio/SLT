package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispSymbol;
import com.en_circle.slt.plugin.params.LispArgslist;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;
import com.intellij.openapi.project.Project;

import java.math.BigInteger;

public class Argslist extends SlimeRequest {

    public static SlimeRequest getArgslist(String symbol, String packageName, Callback callback) {
        return new Argslist(symbol, packageName, callback);
    }

    private final String symbol;
    private final String packageName;
    private final Callback callback;

    private Argslist(String symbol, String packageName, Callback callback) {
        this.symbol = symbol;
        this.packageName = packageName;
        this.callback = callback;
    }

    @Override
    public SwankPacket createPacket(BigInteger requestId, Project project) {
        return SwankPacket.argslist(symbol, packageName, requestId);
    }

    public void processReply(LispContainer data) {
        if (isOk(data)) {
            callback.onResult(new LispArgslist(data.getItems().get(1)));
        }
    }

    private boolean isOk(LispContainer data) {
        return data.getItems().size() > 0 &&
                data.getItems().get(0) instanceof LispSymbol &&
                ":ok".equals(((LispSymbol) data.getItems().get(0)).getValue());
    }

    public interface Callback {
        void onResult(LispArgslist result);
    }

}
