package com.en_circle.slt.plugin;

public class SymbolState {

    public final String name;

    public String symbol;
    public Long timestamp = null;

    public SymbolBinding binding = SymbolBinding.NONE;
    public String documentation;

    public SymbolState(String name) {
        this.name = name;
    }

    public enum SymbolBinding {
        NONE, FUNCTION, MACRO, SPECIAL_FORM,
        CONSTANT, KEYWORD, SPECIAL_VARIABLE
    }

}
