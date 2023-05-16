package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.lisp.LispSymbol;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;
import com.intellij.openapi.project.Project;

import java.math.BigInteger;

public class Disassemble extends SlimeRequest {

    public static SlimeRequest disassemble(String name, String packageName, Callback callback) {
        return new Disassemble(name, packageName, callback);
    }

    private final String symbol;
    private final String packageName;
    private final Callback callback;

    private Disassemble(String symbol, String packageName, Callback callback) {
        this.symbol = symbol;
        this.packageName = packageName;
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
        return SwankPacket.disassemble(symbol, packageName, requestId);
    }

    public interface Callback {
        void onResult(LispElement result);
    }

    public enum XrefType {

        CALLS(":calls"),
        @Deprecated
        CALLS_WHO(":calls-who"),
        REFERENCES(":references"),
        BINDS(":binds"),
        @Deprecated
        SETS(":sets"),
        MACRO_EXPANDS(":macroexpands"),
        SPECIALIZES(":specializes"),
        @Deprecated
        CALLERS(":callers"),
        @Deprecated
        CALLEES(":callees")

        ;

        private final String name;

        XrefType(String name) {
            this.name = name;
        }

        public String getName() {
            return this.name;
        }

    }

}
