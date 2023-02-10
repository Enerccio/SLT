package com.en_circle.slt.plugin.swank;

import com.intellij.openapi.project.Project;

import java.math.BigInteger;

public abstract  class SlimeRequest {

    public abstract SwankPacket createPacket(BigInteger requestId, Project project);

    public BigInteger getRequestId() {
        return null;
    }

}
