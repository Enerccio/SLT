package com.en_circle.slt.plugin.swank.debug;

public class SltDebugArgument {

    private final String name;
    private final boolean isRest;

    public SltDebugArgument(String name, boolean isRest) {
        this.name = name;
        this.isRest = isRest;
    }

    public String getName() {
        return name;
    }

    public boolean isRest() {
        return isRest;
    }
}
