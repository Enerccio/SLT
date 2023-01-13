package com.en_circle.slt.plugin;

import com.intellij.openapi.vfs.VirtualFile;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;

public class SymbolState {

    public final String name;
    public final String symbolName;
    public Long timestamp = null;

    public SymbolBinding binding = SymbolBinding.NONE;
    public String documentation;
    public List<WeakReference<VirtualFile>> containerFiles = new ArrayList<>();

    public SymbolState(String name, String symbolName) {
        this.name = name;
        this.symbolName = symbolName;
    }

    public enum SymbolBinding {
        NONE, FUNCTION, MACRO, SPECIAL_FORM,
        CONSTANT, KEYWORD, SPECIAL_VARIABLE
    }

}
