package com.en_circle.slt.plugin.swank;

import java.math.BigInteger;

public abstract  class SlimeRequest {

    public abstract SlimePacket createPacket(BigInteger requestId);

}
