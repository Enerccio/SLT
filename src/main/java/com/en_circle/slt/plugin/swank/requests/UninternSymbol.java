package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;
import com.intellij.openapi.project.Project;

import java.math.BigInteger;

public class UninternSymbol extends SlimeRequest {

    public static SlimeRequest uninternSymbol(String name, String packageName) {
        return new UninternSymbol(name, packageName);
    }

    private final String symbol;
    private final String packageName;

    private UninternSymbol(String symbol, String packageName) {
        this.symbol = symbol;
        this.packageName = packageName;
    }

    @Override
    public SwankPacket createPacket(BigInteger requestId, Project project) {
        return SwankPacket.uninternSymbol(symbol, packageName, requestId);
    }

}
