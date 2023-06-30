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

}
