package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.lisp.LispSymbol;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;
import com.intellij.openapi.project.Project;

import java.math.BigInteger;

public class Macroexpand1 extends SlimeRequest {

    public static SlimeRequest macroexpand(String form, String module, Callback callback) {
        return new Macroexpand1(form, module, callback);
    }

    private final Callback callback;
    private final String form;
    private final String module;

    public Macroexpand1(String form, String module, Callback callback) {
        this.form = form;
        this.module = module;
        this.callback = callback;
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
        return SwankPacket.macroexpand1(form, module, requestId);
    }

    public interface Callback {
        void onResult(LispElement result);
    }

}
