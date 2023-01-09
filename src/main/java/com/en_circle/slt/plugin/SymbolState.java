package com.en_circle.slt.plugin;

public class SymbolState {

    public final String name;

    public SymbolBinding binding = SymbolBinding.NONE;
    public String documentation;

    public SymbolState(String name) {
        this.name = name;
    }

    public enum SymbolBinding {
        NONE, FUNCTION, MACRO, SPECIAL_FORM
    }

}
