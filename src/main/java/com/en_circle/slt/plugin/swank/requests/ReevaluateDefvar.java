package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;
import com.intellij.openapi.project.Project;

import java.math.BigInteger;

public class ReevaluateDefvar extends SlimeRequest {

    public static SlimeRequest reevaluateDefvar(String form) {
        return new ReevaluateDefvar(form);
    }

    private final String form;

    private ReevaluateDefvar(String form) {
        this.form = form;
    }

    @Override
    public SwankPacket createPacket(BigInteger requestId, Project project) {
        return SwankPacket.reevaluteDefvar(form, requestId);
    }

}
