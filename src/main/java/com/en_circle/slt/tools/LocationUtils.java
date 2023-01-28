package com.en_circle.slt.tools;

import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.en_circle.slt.plugin.lisp.psi.LispToplevel;
import com.en_circle.slt.plugin.swank.components.SourceLocation;
import com.intellij.lang.ASTNode;
import com.intellij.lang.FileASTNode;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.util.PsiTreeUtil;

import java.io.File;
import java.util.Collection;
import java.util.function.Function;

public class LocationUtils {

    public static <X> X convertFromLocationToSymbol(Project project, SourceLocation location, String name, Function<LispSymbol, X> converter) {
        if (location.isFile()) {
            VirtualFile vf = LocalFileSystem.getInstance().findFileByIoFile(new File(location.getLocation()));
            if (vf != null) {
                PsiFile file = PsiManager.getInstance(project).findFile(vf);
                if (file != null) {
                    FileASTNode node = file.getNode();
                    if (node != null) {
                        ASTNode reference = node.findLeafElementAt(location.getPosition());
                        if (reference != null) {
                            PsiElement referenceElement = reference.getPsi();
                            LispSymbol ls = PsiTreeUtil.getParentOfType(referenceElement, LispSymbol.class);
                            if (ls != null && ls.getName() != null && ls.getName().equalsIgnoreCase(name)) {
                                return converter.apply(ls);
                            }

                            PsiElement parent = PsiTreeUtil.getParentOfType(referenceElement, LispToplevel.class);
                            if (parent != null) {
                                Collection<LispSymbol> symbols = PsiTreeUtil.findChildrenOfType(parent, LispSymbol.class);
                                for (LispSymbol symbol : symbols) {
                                    if (symbol.getName() != null && symbol.getName().equalsIgnoreCase(name)) {
                                        return converter.apply(symbol);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        return null;
    }

}
