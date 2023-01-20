package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.swank.components.SourceLocation;
import com.intellij.openapi.vfs.VirtualFile;

import java.lang.ref.WeakReference;
import java.util.HashSet;
import java.util.Objects;
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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        SymbolState that = (SymbolState) o;

        if (!Objects.equals(name, that.name)) return false;
        if (!Objects.equals(packageName, that.packageName)) return false;
        return Objects.equals(symbolName, that.symbolName);
    }

    @Override
    public int hashCode() {
        int result = name != null ? name.hashCode() : 0;
        result = 31 * result + (packageName != null ? packageName.hashCode() : 0);
        result = 31 * result + (symbolName != null ? symbolName.hashCode() : 0);
        return result;
    }

    public enum SymbolBinding {
        NONE,
        FUNCTION,
        MACRO,
        SPECIAL_FORM,
        CONSTANT,
        KEYWORD,
        SPECIAL_VARIABLE,
        CLASS,
        METHOD
    }
}
