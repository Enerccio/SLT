package com.en_circle.slt.plugin.services.lisp.components;

import com.en_circle.slt.plugin.lisp.lisp.LispString;
import com.en_circle.slt.plugin.lisp.lisp.LispUtils;
import com.en_circle.slt.plugin.lisp.psi.LispList;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.swank.requests.MacroexpandAll;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;

public class SltLispEnvironmentMacroExpandCache {

    private final LoadingCache<MacroExpandEntry, MacroExpandEntry> expandedMacros;

    public SltLispEnvironmentMacroExpandCache() {
        CacheLoader<MacroExpandEntry, MacroExpandEntry> loader = new CacheLoader<>() {
            @Override
            public MacroExpandEntry load(@NotNull SltLispEnvironmentMacroExpandCache.MacroExpandEntry key) throws Exception {
                LispEnvironmentService.getInstance(key.project).sendToLisp(
                        MacroexpandAll.macroexpand(key.form, key.packageName, result -> {
                            key.evaluated = LispUtils.unescape(((LispString) result).getValue());
                        }));
                return key;
            }
        };
        expandedMacros = CacheBuilder.newBuilder()
                .maximumSize(512)
                .build(loader);
    }


    public String macroexpand(LispList form, String packageName) throws Exception {
        MacroExpandEntry entry = new MacroExpandEntry();
        entry.form = form.getNode().getText();
        PsiFile file = form.getContainingFile();
        if (file != null) {
            entry.virtualFile = file.getVirtualFile();
            entry.modification = file.getModificationStamp();
            entry.packageName = packageName;
            entry.project = form.getProject();
            MacroExpandEntry cachedEntry = expandedMacros.get(entry);
            if (cachedEntry.modification < entry.modification) {
                expandedMacros.refresh(cachedEntry);
            }
            return expandedMacros.get(entry).evaluated;
        }
        return null;
    }

    public void clear() {
        expandedMacros.invalidateAll();
    }

    private static class MacroExpandEntry {

        public Project project;
        private String form;
        private String packageName;
        private String evaluated;
        private VirtualFile virtualFile;
        private long modification;

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            MacroExpandEntry entry = (MacroExpandEntry) o;

            if (!Objects.equals(form, entry.form)) return false;
            if (!Objects.equals(packageName, entry.packageName)) return false;
            return Objects.equals(virtualFile, entry.virtualFile);
        }

        @Override
        public int hashCode() {
            int result = form != null ? form.hashCode() : 0;
            result = 31 * result + (packageName != null ? packageName.hashCode() : 0);
            result = 31 * result + (virtualFile != null ? virtualFile.hashCode() : 0);
            return result;
        }
    }
}
