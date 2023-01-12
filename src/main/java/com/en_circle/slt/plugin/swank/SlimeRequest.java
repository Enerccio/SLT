package com.en_circle.slt.plugin.swank;

import java.math.BigInteger;

public abstract  class SlimeRequest {

    public abstract SwankPacket createPacket(BigInteger requestId);

    public BigInteger getRequestId() {
        return null;
    }

}
