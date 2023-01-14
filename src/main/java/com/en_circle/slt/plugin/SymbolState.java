package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.swank.components.SourceLocation;
import com.intellij.openapi.vfs.VirtualFile;

import java.lang.ref.WeakReference;
import java.util.HashSet;
import java.util.Set;

public class SymbolState {

    public final String name;
    public final String packageName;
    public final String symbolName;
    public Long timestamp = null;

    public SymbolBinding binding = SymbolBinding.NONE;
    public String documentation;
    public Set<WeakReference<VirtualFile>> containerFiles = new HashSet<>();
    public SourceLocation location = new SourceLocation();

    public SymbolState(String name, String packageName, String symbolName) {
        this.name = name;
        this.packageName = packageName;
        this.symbolName = symbolName;
    }

    public enum SymbolBinding {
        NONE, FUNCTION, MACRO, SPECIAL_FORM,
        CONSTANT, KEYWORD, SPECIAL_VARIABLE
    }

}
