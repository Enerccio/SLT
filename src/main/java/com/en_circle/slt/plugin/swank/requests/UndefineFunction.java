package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;
import com.intellij.openapi.project.Project;

import java.math.BigInteger;

public class UndefineFunction extends SlimeRequest {

    public static SlimeRequest undefineFunction(String name, String packageName) {
        return new UndefineFunction(name, packageName);
    }

    private final String symbol;
    private final String packageName;

    private UndefineFunction(String symbol, String packageName) {
        this.symbol = symbol;
        this.packageName = packageName;
    }

    @Override
    public SwankPacket createPacket(BigInteger requestId, Project project) {
        return SwankPacket.undefineFunction(symbol, packageName, requestId);
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
