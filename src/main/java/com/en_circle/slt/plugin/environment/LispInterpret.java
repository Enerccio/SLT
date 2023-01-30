package com.en_circle.slt.plugin.environment;

public enum LispInterpret {

    SBCL(":sbcl")

    ;

    public final String symbolName;

    LispInterpret(String symbolName) {
        this.symbolName = symbolName;
    }

}
